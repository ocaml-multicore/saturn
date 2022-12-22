open Extensions

module type S = sig
  type key
  type 'a t

  val create : unit -> 'a t
  (** Create an empty dictionnary. *)

  val clear : 'a t -> unit
  (** Empty the dictionnary. *)

  val find : key -> 'a t -> 'a
  (** Return the current mapping of the key, or raises {!Not_found}. *)

  val find_opt : key -> 'a t -> 'a option
  (** Return [Some v] if [v] is mapped with the key, or [None]. *)

  val mem : key -> 'a t -> bool
  (** Return [true] if the key is present in the map, or [false]. *)

  val update : key -> ('a option -> 'a option) -> 'a t -> unit
  (** [update k f m] changes the mapping of [k] to [f (find_opt k m)].

      If [f (Some v)] returns [None], the mapping [v] is removed. *)

  val add : key -> 'a -> 'a t -> unit
  (** Add the (key, value) pair to the map, overwriting any previous mapping. *)

  val remove : key -> 'a t -> unit
  (** Remove the mapping of the key *)

  val copy : 'a t -> 'a t
  (** Take a snapshot of the map. This is a O(1) operation.

      The original map can still be used, even in concurrent contexts. *)

  val is_empty : 'a t -> bool
  (** Return [true] if the map contains no data. *)

  val size : 'a t -> int
  (** Return the number of elements in the map.
      Result might not be accurate in concurrent contexts. Take a snapshot first! *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** Apply a function to all mappings in unspecified order.
      The behavior is not specified if the map is modified (by the function
        or concurrent access) during the iteration. *)

  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** [map f t] returns a new dictionnary where every pair (k,v) in t is replaced by (k, f k v);
      The original map is untouched. *)

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f t init] computes [f k1 v1 (f k2 v2 ... (f kN vN init))] where [k1...kN] are the keys
      and [v1...vN] the associated values, in unspecified order.

      If [f] is side-effecting, those effects may happen an unspecified number of times, as operations are restarted in case of CAS conflicts.
      Use a pure function instead. *)

  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  (** [filter_map_inplace f t] applies [f] to all mappings, updating them depending on the result of [f k v], in unspecified order.
      - If [f] returns [None], the mapping is removed.
      - If [f] is side-effecting, those effects may happen an unspecified number of times, as operations are restarted in case of CAS conflicts.
      Use a pure function instead. *)

  val exists : (key -> 'a -> bool) -> 'a t -> bool
  (** Check that there exists at least one (key, value) pair satisfying the predicate. *)

  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  (** Check that all (key, value) pairs satisfy the predicate. *)

  val save_as_dot : (key -> string) * ('a -> string) -> 'a t -> string -> unit
  (** Save the map as a graph in a .dot file.

      Mainly for debugging purposes. *)
end

module Make (H : Hashtbl.HashedType) = struct
  module Types = struct
    type key = H.t
    type z = Zero [@@warning "-37"]
    type nz = NonZero [@@warning "-37"]
    type _ kind = Z : z kind | NZ : nz kind

    type 'a t = { root : ('a, z) iNode }

    and gen = unit ref
    (** The type of generations, an empty ref cell.

        This is a classic OCaml trick to ensure safe (in)equality:
        With this setup
        [let x = ref ();; let y = ref ();; let z = x;;]
        [x == y] is false but [x == z] is true.
        This avoids integer overflow and discarded gen objects will be garbage collected. *)

    and ('a, 'n) iNode = {
      main : ('a, 'n) mainNode Kcas.ref;
      gen : gen Kcas.ref;
    }

    and (_, _) mainNode =
      | CNode : ('a, 'n) cNode -> ('a, 'n) mainNode
      | TNode : 'a leaf option -> ('a, nz) mainNode
      | LNode : 'a leaf list -> ('a, nz) mainNode

    and ('a, 'n) cNode = { bmp : int; array : ('a, 'n) branch array }

    and (_, _) branch =
      | INode : ('a, nz) iNode -> ('a, 'n) branch
      | Leaf : 'a leaf -> ('a, 'n) branch

    and 'a leaf = { key : key; value : 'a }

    (** Inner return type of functions manipulating tries.
        - Type {!'a} is returned if all goes well.
        - Type {!'b} is returned when a cleaning needs to happen.
        - Type {!'n} is the height of the trie, to ensure that [Clean*] can't bubble to the root.
        - Type {!'c} marks the possibility of change, to ensure that {!CleanAfterDive} can't happen at all in read-only operations. *)
    type (_, _, _, _) status =
      | Alright : 'a -> ('a, 'b, 'n, 'c) status
      | GenChange : ('a, 'b, 'n, 'c) status
      | CleanBeforeDive : 'b leaf option -> ('a, 'b, nz, 'c) status
      | CleanAfterDive : 'b leaf option -> ('a, 'b, nz, nz) status
  end

  include Types

  (** Generational Double Compare Single Swap *)
  let gen_dcss inode old_m new_m gen =
    let cas = Kcas.mk_cas inode.main old_m new_m in
    let atomic_read = Kcas.mk_cas inode.gen gen gen in
    Kcas.kCAS [ cas; atomic_read ]

  let create () =
    {
      root =
        {
          main = Kcas.ref (CNode { bmp = 0; array = [||] });
          gen = Kcas.ref (ref ());
        };
    }

  let rec clear t =
    let startgen = Kcas.get t.root.gen in
    let empty_mnode = CNode { bmp = 0; array = [||] } in
    if not @@ gen_dcss t.root (Kcas.get t.root.main) empty_mnode startgen then
      clear t

  (** At each level in the trie, we only use 5 bits of the hash. *)
  let lvl_offset = 5

  (** 2^5 - 1 = 31 = 0x1F*)
  let lvl_mask = 0x1F

  let hash_to_flag lvl hash =
    let shifted = hash lsr lvl in
    let relevant = shifted land lvl_mask in
    1 lsl relevant

  (** {ul {- [flag] is a single bit flag (never 0)}        
          {- [pos] is an index in the array, hence it satisfies 0 <= pos <= popcount bitmap}} *)
  let flagpos lvl bitmap hash =
    let flag = hash_to_flag lvl hash in
    let pos = pred flag land bitmap |> popcount in
    (flag, pos)

  (* This function assumes l_filtered is sorted and it only contains valid indexes *)
  let remove_from_bitmap bmp l_filtered =
    let rec aux cursor bmp l_filtered seen =
      if cursor >= Sys.int_size then bmp
      else
        let bit = 1 lsl cursor in
        let flag = bit land bmp in
        match (flag, l_filtered) with
        | _, [] -> bmp
        | 0, _ -> aux (cursor + 1) bmp l_filtered seen
        | _, x :: xs when x = seen ->
            aux (cursor + 1) (bmp lxor flag) xs (seen + 1)
        | _ -> aux (cursor + 1) bmp l_filtered (seen + 1)
    in
    aux 0 bmp l_filtered 0

  let vertical_compact :
      type n.
      ('a, n) iNode ->
      ('a, n) mainNode ->
      ('a, n) cNode ->
      n kind ->
      gen ->
      bool * (unit, 'a, n, nz) status =
   fun i old_m cnode k startgen ->
    let new_m, (status : (_, _, n, _) status) =
      match k with
      | Z -> (CNode cnode, Alright ())
      | NZ -> (
          match Array.length cnode.array with
          | 0 -> (TNode None, CleanAfterDive None)
          | 1 -> (
              match cnode.array.(0) with
              | Leaf leaf -> (TNode (Some leaf), CleanAfterDive (Some leaf))
              | _ -> (CNode cnode, Alright ()))
          | _ -> (CNode cnode, Alright ()))
    in
    (gen_dcss i old_m new_m startgen, status)

  let horizontal_compact cnode =
    let array, l_filtered =
      Array.filter_map
        (function
          | Leaf _ as l -> Some l
          | INode i as inner -> (
              match Kcas.get i.main with
              | TNode None -> None
              | TNode (Some l) -> Some (Leaf l)
              | _ -> Some inner))
        cnode.array
    in
    let bmp = remove_from_bitmap cnode.bmp l_filtered in
    { array; bmp }

  let clean i old_m cnode k startgen =
    let new_m = horizontal_compact cnode in
    ignore @@ vertical_compact i old_m new_m k startgen

  let cnode_with_insert cnode leaf flag pos =
    let bmp = cnode.bmp lor flag in
    let array = Array.insert cnode.array pos (Leaf leaf) in
    { bmp; array }

  let cnode_with_update cnode branch pos =
    let array = Array.copy cnode.array in
    array.(pos) <- branch;
    { cnode with array }

  let cnode_with_delete cnode flag pos =
    let bmp = cnode.bmp lxor flag in
    let array = Array.remove cnode.array pos in
    { bmp; array }

  let clean_one i old_m cnode flag pos k inner startgen =
    let new_m =
      match inner with
      | Some l -> cnode_with_update cnode (Leaf l) pos
      | None -> cnode_with_delete cnode flag pos
    in
    snd @@ vertical_compact i old_m new_m k startgen

  (** [regen i old_m cnode pos child_main new_gen] updates the generation of the immediate child
      [cnode.array.(pos)] of [i] to [new_gen].
      Fails if [i]'s generation isn't equal to [new_gen].

      We volontarily do not update the generations of deeper INodes, as this is a lazy algorithm.
      TODO: investigate if this is really wise. *)
  let regenerate i old_m cnode pos child_main new_gen =
    let new_cnode =
      cnode_with_update cnode
        (INode { main = Kcas.ref child_main; gen = Kcas.ref new_gen })
        pos
    in
    gen_dcss i old_m (CNode new_cnode) new_gen

  let find key t =
    let hash = H.hash key in
    let rec loop () =
      let startgen = Kcas.get t.root.gen in
      let rec aux :
          type n. ('a, n) iNode -> n kind -> int -> ('a, 'a, n, z) status =
       fun i k lvl ->
        match Kcas.get i.main with
        | TNode l -> CleanBeforeDive l
        | CNode cnode as cn -> (
            let flag, pos = flagpos lvl cnode.bmp hash in
            if flag land cnode.bmp = 0 then raise Not_found
            else
              match cnode.array.(pos) with
              | INode inner ->
                  if Kcas.get inner.gen == startgen then
                    match aux inner NZ (lvl + lvl_offset) with
                    | CleanBeforeDive l ->
                        ignore @@ clean_one i cn cnode flag pos k l startgen;
                        aux i k lvl
                    | (Alright _ | GenChange) as other -> other
                  else if
                    regenerate i cn cnode pos (Kcas.get inner.main) startgen
                  then aux i k lvl
                  else GenChange
              | Leaf leaf ->
                  if H.equal leaf.key key then Alright leaf.value
                  else raise Not_found)
        | LNode lst ->
            let leaf = List.find (fun l -> H.equal l.key key) lst in
            Alright leaf.value
      in
      match aux t.root Z 0 with Alright v -> v | GenChange -> loop ()
    in
    loop ()

  let find_opt key t = try Some (find key t) with Not_found -> None
  let mem key t = Option.is_some (find_opt key t)

  let rec branch_of_pair :
      type n. 'a leaf * int -> 'a leaf * int -> int -> gen -> ('a, n) branch =
   fun (l1, h1) (l2, h2) lvl gen ->
    let new_m =
      if lvl > Sys.int_size then
        (* Maximum depth reached, it's a full hash collision. We just dump everything into a list. *)
        LNode [ l1; l2 ]
      else
        let flag1 = hash_to_flag lvl h1 in
        let flag2 = hash_to_flag lvl h2 in
        let bmp = flag1 lor flag2 in
        let array =
          match compare flag1 flag2 with
          | 0 ->
              (* Collision on this level, we need to go deeper *)
              [| branch_of_pair (l1, h1) (l2, h2) (lvl + lvl_offset) gen |]
          | 1 -> [| Leaf l2; Leaf l1 |]
          | _ -> [| Leaf l1; Leaf l2 |]
        in
        CNode { bmp; array }
    in
    INode { main = Kcas.ref new_m; gen = Kcas.ref gen }

  let update key f t =
    let hash = H.hash key in
    let rec loop () =
      let startgen = Kcas.get t.root.gen in
      let rec aux :
          type n. ('a, n) iNode -> n kind -> int -> (unit, 'a, n, nz) status =
       fun i k lvl ->
        match Kcas.get i.main with
        | TNode l -> CleanBeforeDive l
        | CNode cnode as cn -> (
            let flag, pos = flagpos lvl cnode.bmp hash in
            if flag land cnode.bmp = 0 then
              (* No flag collision, the key isn't in the map. *)
              match f None with
              | Some value ->
                  (* We need to insert it. *)
                  let new_cnode =
                    cnode_with_insert cnode { key; value } flag pos
                  in
                  if gen_dcss i cn (CNode new_cnode) startgen then Alright ()
                  else GenChange
              | None -> Alright ()
            else
              match cnode.array.(pos) with
              | INode inner ->
                  if Kcas.get inner.gen == startgen then
                    match aux inner NZ (lvl + lvl_offset) with
                    | CleanBeforeDive l ->
                        ignore @@ clean_one i cn cnode flag pos k l startgen;
                        aux i k lvl
                    | CleanAfterDive l ->
                        clean_one i cn cnode flag pos k l startgen
                    | (Alright _ | GenChange) as other -> other
                  else if
                    regenerate i cn cnode pos (Kcas.get inner.main) startgen
                  then aux i k lvl
                  else GenChange
              | Leaf l -> (
                  if H.equal l.key key then
                    match f (Some l.value) with
                    | Some value ->
                        (* We found a value to be updated. *)
                        let new_cnode =
                          cnode_with_update cnode (Leaf { key; value }) pos
                        in
                        if gen_dcss i cn (CNode new_cnode) startgen then
                          Alright ()
                        else GenChange
                    | None ->
                        (* We need to remove this value *)
                        let new_cnode = cnode_with_delete cnode flag pos in
                        let cas_success, status =
                          vertical_compact i cn new_cnode k startgen
                        in
                        if not cas_success then GenChange else status
                  else
                    match f None with
                    | Some value ->
                        (* We create a new entry colliding with a leaf, so we create a new level. *)
                        let new_pair =
                          branch_of_pair
                            (l, H.hash l.key)
                            ({ key; value }, hash)
                            (lvl + lvl_offset) startgen
                        in
                        let new_cnode = cnode_with_update cnode new_pair pos in
                        if gen_dcss i cn (CNode new_cnode) startgen then
                          Alright ()
                        else GenChange
                    (* The key isn't in the map. *)
                    | None -> Alright ()))
        | LNode lst as ln ->
            let rec update_list = function
              | [] -> (
                  match f None with
                  | None -> []
                  | Some v -> [ { key; value = v } ])
              | x :: xs ->
                  if H.equal x.key key then
                    match f (Some x.value) with
                    | Some v -> { key; value = v } :: xs
                    | None -> xs
                  else x :: update_list xs
            in
            let new_list = update_list lst in
            let new_m, status =
              match new_list with
              | [ l ] -> (TNode (Some l), CleanAfterDive (Some l))
              | _ -> (LNode new_list, Alright ())
            in
            if gen_dcss i ln new_m startgen then status else GenChange
      in
      match aux t.root Z 0 with Alright () -> () | GenChange -> loop ()
    in
    loop ()

  let add key value t = update key (fun _ -> Some value) t
  let remove key t = update key (fun _ -> None) t

  let rec snapshot t =
    let main = Kcas.get t.root.main in
    let gen = Kcas.get t.root.gen in
    let atomic_read = Kcas.mk_cas t.root.main main main in
    (* The old root is updated to a new generation *)
    let cas = Kcas.mk_cas t.root.gen gen (ref ()) in
    if Kcas.kCAS [ cas; atomic_read ] then
      (* We can return a new root with the old contents *)
      { root = { main = Kcas.ref main; gen = Kcas.ref gen } }
    else snapshot t

  let copy = snapshot

  let is_empty t =
    match Kcas.get t.root.main with CNode cnode -> cnode.bmp = 0

  let filter_map_inplace f t =
    let rec loop () =
      let startgen = Kcas.get t.root.gen in
      let rec aux :
          type n. ('a, n) iNode -> n kind -> (('a, n) iNode, 'a, n, nz) status =
       fun i k ->
        match Kcas.get i.main with
        | TNode l -> CleanBeforeDive l
        | CNode cnode as cn -> (
            let rec inner_loop :
                ('a, n) branch list ->
                int list ->
                int ->
                (('a, n) mainNode, 'a, nz, z) status =
             fun l_mapped l_filtered pos ->
              if pos < 0 then
                match (l_mapped, k) with
                | [], NZ -> Alright (TNode None)
                | [ Leaf l ], NZ -> Alright (TNode (Some l))
                | _ ->
                    let array = Array.of_list l_mapped in
                    let bmp = remove_from_bitmap cnode.bmp l_filtered in
                    Alright (CNode { bmp; array })
              else
                match cnode.array.(pos) with
                | INode inner ->
                    if Kcas.get inner.gen == startgen then
                      match aux inner NZ with
                      | Alright inode ->
                          inner_loop (INode inode :: l_mapped) l_filtered
                            (pos - 1)
                      | CleanAfterDive (Some leaf) ->
                          inner_loop (Leaf leaf :: l_mapped) l_filtered (pos - 1)
                      | CleanAfterDive None ->
                          inner_loop l_mapped (pos :: l_filtered) (pos - 1)
                      | GenChange -> GenChange
                      | CleanBeforeDive _ as other -> other
                    else GenChange
                | Leaf { key; value } -> (
                    match f key value with
                    | Some value ->
                        inner_loop
                          (Leaf { key; value } :: l_mapped)
                          l_filtered (pos - 1)
                    | None -> inner_loop l_mapped (pos :: l_filtered) (pos - 1))
            in
            match inner_loop [] [] (Array.length cnode.array - 1) with
            | Alright (TNode l as tnode) ->
                if gen_dcss i cn tnode startgen then CleanAfterDive l
                else GenChange
            | Alright new_m ->
                if gen_dcss i cn new_m startgen then Alright i else GenChange
            | CleanBeforeDive _ ->
                clean i cn cnode k startgen;
                aux i k
            | GenChange -> GenChange)
        | LNode list as ln ->
            let new_list =
              List.filter_map
                (fun { key; value } ->
                  match f key value with
                  | Some value -> Some { key; value }
                  | None -> None)
                list
            in
            let new_m, status =
              match new_list with
              | [] -> (TNode None, CleanAfterDive None)
              | [ l ] -> (TNode (Some l), CleanAfterDive (Some l))
              | _ -> (LNode new_list, Alright i)
            in
            if gen_dcss i ln new_m startgen then status else GenChange
      in
      match aux t.root Z with Alright _ -> () | GenChange -> loop ()
    in
    loop ()

  let map f t =
    let rec loop () =
      let startgen = Kcas.get t.root.gen in
      let rec aux :
          type n. ('a, n) iNode -> n kind -> (('b, n) iNode, 'a, n, z) status =
       fun i k ->
        match Kcas.get i.main with
        | TNode l -> CleanBeforeDive l
        | CNode cnode as cn -> (
            let rec inner_loop :
                ('b, n) branch list -> int -> (('b, n) iNode, 'a, n, z) status =
             fun lst pos ->
              if pos < 0 then
                let array = Array.of_list lst in
                Alright
                  {
                    main = Kcas.ref (CNode { cnode with array });
                    gen = Kcas.ref startgen;
                  }
              else
                match cnode.array.(pos) with
                | Leaf { key; value } ->
                    inner_loop
                      (Leaf { key; value = f key value } :: lst)
                      (pos - 1)
                | INode inner ->
                    if Kcas.get inner.gen == startgen then (
                      match aux inner NZ with
                      | Alright inode ->
                          inner_loop (INode inode :: lst) (pos - 1)
                      | GenChange -> GenChange
                      | CleanBeforeDive _ ->
                          clean i cn cnode k startgen;
                          aux i k)
                    else if
                      regenerate i cn cnode pos (Kcas.get inner.main) startgen
                    then aux i k
                    else GenChange
            in
            match inner_loop [] (Array.length cnode.array - 1) with
            | CleanBeforeDive _ ->
                clean i cn cnode k startgen;
                aux i k
            | other -> other)
        | LNode list ->
            let new_list =
              List.map
                (function { key; value } -> { key; value = f key value })
                list
            in
            Alright
              { main = Kcas.ref (LNode new_list); gen = Kcas.ref startgen }
      in
      match aux t.root Z with Alright m -> m | GenChange -> loop ()
    in
    { root = loop () }

  let rec reduce f ?(short_circuiting = fun _ -> false) init t =
    let startgen = Kcas.get t.root.gen in
    let rec aux : type n. 'b -> ('a, n) iNode -> n kind -> ('b, _, n, z) status
        =
     fun acc i k ->
      match Kcas.get i.main with
      | TNode l -> CleanBeforeDive l
      | CNode cnode as cn -> (
          let rec inner_loop : _ -> int -> (_, _, n, z) status =
           fun inner_acc pos ->
            if pos < 0 then Alright inner_acc
            else
              match cnode.array.(pos) with
              | Leaf { key; value } ->
                  let elem = f key value inner_acc in
                  if short_circuiting elem then Alright elem
                  else inner_loop elem (pos - 1)
              | INode inner ->
                  if Kcas.get inner.gen == startgen then (
                    match aux inner_acc inner NZ with
                    | Alright elem ->
                        if short_circuiting elem then Alright elem
                        else inner_loop elem (pos - 1)
                    | GenChange -> GenChange
                    | CleanBeforeDive _ ->
                        clean i cn cnode k startgen;
                        aux acc i k)
                  else if
                    regenerate i cn cnode pos (Kcas.get inner.main) startgen
                  then aux acc i k
                  else GenChange
          in
          match inner_loop acc (Array.length cnode.array - 1) with
          | CleanBeforeDive _ ->
              clean i cn cnode k startgen;
              aux acc i k
          | other -> other)
      | LNode list ->
          let rec list_loop inner_acc = function
            | [] -> Alright inner_acc
            | { key; value } :: xs ->
                let elem = f key value inner_acc in
                if short_circuiting elem then Alright elem
                else list_loop elem xs
          in
          list_loop acc list
    in
    match aux init t.root Z with
    | Alright res -> res
    | GenChange -> reduce f ~short_circuiting init t

  let exists pred =
    reduce (fun k v _ -> pred k v) ~short_circuiting:Fun.id false

  let for_all pred = reduce (fun k v _ -> pred k v) ~short_circuiting:not true
  let iter f = reduce (fun k v _ -> f k v) ()
  let fold f t init = reduce f init t
  let size t = reduce (fun _ _ sum -> sum + 1) 0 t

  let save_as_dot (string_of_key, string_of_val) t filename =
    let oc = open_out filename in
    let ic (* cnode *) = ref 0 in
    let il (* lnode *) = ref 0 in
    let ii (* inode *) = ref 0 in
    let it (* tnode *) = ref 0 in
    let iv (* leaf (value) *) = ref 0 in
    Printf.fprintf oc
      "digraph {\n\troot [shape=plaintext];\n\troot -> I0 [style=dotted];\n";
    let pr_cnode_info cnode =
      Printf.fprintf oc "\tC%d [shape=record label=\"<bmp> 0x%X" !ic cnode.bmp;
      Array.iteri (fun i _ -> Printf.fprintf oc "|<i%d> ·" i) cnode.array;
      Printf.fprintf oc "\"];\n"
    in
    let pr_leaf_info leaf =
      Printf.fprintf oc
        "\tV%d [shape=Mrecord label=\"<key> %s|<val> %s\" style=filled \
         color=gold];\n"
        !iv (string_of_key leaf.key) (string_of_val leaf.value)
    in
    let rec pr_inode : type n. ('a, n) iNode -> unit =
     fun inode ->
      let self = !ii in
      incr ii;
      Printf.fprintf oc "\tI%d [style=filled shape=box color=green2];\n" self;
      match Kcas.get inode.main with
      | CNode cnode ->
          pr_cnode_info cnode;
          Printf.fprintf oc "\tI%d -> C%d:bmp;\n" self !ic;
          pr_cnode cnode
      | TNode leaf ->
          Printf.fprintf oc "\tI%d -> T%d;\n" self !it;
          pr_tnode leaf
      | LNode list ->
          Printf.fprintf oc "\tI%d -> L%d;\n" self !il;
          pr_list list
    and pr_cnode : type n. ('a, n) cNode -> unit =
     fun cnode ->
      let self = !ic in
      incr ic;
      Array.iteri
        (fun i b ->
          match b with
          | INode inner ->
              Printf.fprintf oc "\tC%d:i%d -> I%d;\n" self i !ii;
              pr_inode inner
          | Leaf leaf ->
              pr_leaf_info leaf;
              Printf.fprintf oc "\tC%d:i%d -> V%d;\n" self i !iv;
              incr iv)
        cnode.array
    and pr_tnode leaf =
      Printf.fprintf oc
        "\tT%d [style=filled shape=box fontcolor=white color=black];\n" !it;
      (match leaf with
      | Some leaf ->
          pr_leaf_info leaf;
          Printf.fprintf oc "\tT%d -> V%d;\n" !it !iv;
          incr iv
      | None -> ());
      incr it
    and pr_list list =
      Printf.fprintf oc "\tL%d [style=filled fontcolor=white color=red];\n" !il;
      List.iter
        (fun l ->
          pr_leaf_info l;
          Printf.fprintf oc "\tL%d -> V%d [color=red style=bold];\n" !il !iv;
          incr iv)
        list;
      incr il
    in
    pr_inode t.root;
    Printf.fprintf oc "}\n%!";
    close_out oc
end
