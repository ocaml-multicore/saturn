(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module type S = sig
  type 'a t;;

  val still_split_order : 'a t -> bool;;

  val equal : 'a t -> 'a t -> bool;;

  val to_string : 'a t -> string;;

  val to_string_clean : 'a t -> string;;

  val create : unit -> 'a t;;

  val find : 'a t -> int -> 'a option;;

  val mem : 'a t -> int -> bool;;

  val add : 'a t -> int -> 'a -> unit;;

  val remove : 'a t -> int -> unit;;
end;;

module M : S = struct
  module Cas = Kcas.W1;;

  type 'a elem_list = bool * int * 'a option * 'a node_ptr
  and 'a node_ptr = (bool * 'a node) Cas.ref
  and 'a node =
    |Node of 'a elem_list
    |Nil
  ;;

  type 'a table = 'a bucket Cas.ref array
  and 'a bucket =
    |Initialized of 'a elem_list
    |Allocated of 'a table
    |Uninitialized
  ;;

  type 'a t = {
    access      : 'a table Cas.ref;
    store       : 'a node_ptr;
    size        : int Cas.ref;
    content     : int Cas.ref;
    access_size : int Cas.ref;
    resize      : int option Cas.ref;
  };;

  let load = 3;;
  let nb_bucket = 512;;

  let equal t1 t2 =
    let rec loop_l n1 n2 =
      match Cas.get n1, Cas.get n2 with
      |(m1, Node(s1, k1, v1, next1)), _  when s1 -> loop_l next1 n2
      |_, (m2, Node(s2, k2, v2, next2))  when s2 -> loop_l n1 next2
      |(m1, Node(s1, k1, v1, next1)), (m2, Node(s2, k2, v2, next2)) -> k1 = k2 && v1 = v2 && m1 = m2 && (loop_l next1 next2)
      |(_, Nil), (_, Nil) -> true
      |_ -> false
    in
    (*(Cas.get t1.size) = (Cas.get t2.size) &&*)
    (Cas.get t1.content) = (Cas.get t2.content) &&
    loop_l t1.store t2.store
    (*loop_a (Cas.get t1.access) (Cas.get t2.access)*)
  ;;

  let to_string t =
    (*print_endline "TO_STRING";*)
    let buf = Buffer.create 20 in
    Buffer.add_string buf (sprintf "Size = %d and Content = %d\nAccess\n[" (Cas.get t.size) (Cas.get t.content));
    let rec loop r =
      match Cas.get r with
      |Uninitialized -> Buffer.add_string buf "x, "
      |Allocated(a') -> Buffer.add_string buf "{"; Array.iter loop a'; Buffer.add_string buf "}"
      |Initialized(s, k, v, next) -> Buffer.add_string buf (sprintf "(%d), " k)
    in
    Array.iter loop (Cas.get t.access);
    Buffer.add_string buf (sprintf "]\nStore\n");
    let rec loop n =
      (*print_endline "TO_STRING_LOOP";*)
      match Cas.get n with
      |m, Node(s, k, v, next) ->
        (*print_endline (sprintf "%d, " k);*)
        if s then
          Buffer.add_string buf (sprintf "[%d]" k)
        else
          Buffer.add_string buf (sprintf "(%d)" k);
        loop next
      |m, Nil -> ()
    in loop t.store;
    Buffer.add_string buf "\n\n";
    Buffer.contents buf
  ;;

  let to_string_clean t =
    (*print_endline "TO_STRING";*)
    let buf = Buffer.create 20 in
    Buffer.add_string buf (sprintf "Size = %d and Content = %d\nAccess\n[" (Cas.get t.size) (Cas.get t.content));
    Buffer.add_string buf (sprintf "]\nStore\n");
    let rec loop n =
      match Cas.get n with
      |m, Node(s, k, v, next) ->
        (*print_endline (sprintf "%d, " k);*)
        if not s then
          Buffer.add_string buf (sprintf "(%d)" k);
        loop next
      |m, Nil -> ()
    in loop t.store;
    Buffer.add_string buf "\n\n";
    Buffer.contents buf
  ;;

  let rec split_compare a b =
    (*print_endline (Printf.sprintf "%d %d" a b);*)
    if a = 0 && b = 0 then
      0
    else
      let bit_a = a land 1 in
      let bit_b = b land 1 in
      if bit_a = bit_b then
        split_compare (a lsr 1) (b lsr 1)
      else if bit_a < bit_b then
        -1
      else
         1
  ;;

  let still_split_order t =
    let rec loop k n =
      match Cas.get n with
      |_, Node(_, nk, _, next) ->
        if split_compare k nk <= 0 then
          loop nk next
        else begin
          (*print_endline (sprintf "STILL_SPLIT_ORDER FAUX : %d !< %d" k nk);*)
          false
        end
      |_ -> true
    in loop 0 t.store
  ;;

  let list_find sentinel new_k =
    print_endline (sprintf "TH%d : LIST_FIND %d" (Domain.self()) new_k);
    let (_, sk, sv, snext) = sentinel in
    let rec loop prev n =
      (*print_endline "LIST_FIND_LOOP";*)
      match Cas.get n with
      |mark, (Node(s, k, v, next) as nnode) as vn ->
        print_endline (sprintf "TH%d : LIST_FIND_LOOP Cas 1    key: %d" (Domain.self ()) k);
        if split_compare new_k k <= 0 then
          (prev, vn)
        else
          loop nnode next
      |mark, Nil as vn -> print_endline "LIST_FIND_LOOP Cas 2"; (prev, vn)
    in loop (Node(sentinel)) snext
  ;;

  let list_insert sentinel new_s new_k new_v =
    (*print_endline "LIST_INSERT";*)
    let b = Kcas.Backoff.create () in
    let rec loop () =
      (*print_endline "LIST_INSERT_LOOP";*)
      let (prev, next) = list_find sentinel new_k in
      let new_node = (new_s, new_k, new_v, Cas.ref (false, snd next)) in
      match prev, next with
      |Node(ps, pk, pv, pnext), (nm, Node(ns, nk, nv, nnext)) ->
        if not (new_s && ns && new_k = nk) then (* New sentinel not inserted yet *)
          if not nm && Cas.cas pnext next (false, Node(new_node)) then
    ((*print_endline "Branche 1";*)
            new_node)
          else
    ((*print_endline "Branche 2";*)
            (Kcas.Backoff.once b; loop ()))
        else
    ((*print_endline "Branche 3";*)
          (ns, nk, nv, nnext))
      |Node(ps, pk, pv, pnext), (nm, Nil) ->
        if not nm && Cas.cas pnext next (false, Node(new_node)) then
    ((*print_endline "Branche 4";*)
          new_node)
        else begin
          let (m, n) = Cas.get pnext in
          if (m, n) == (false, snd next) then print_endline "Nil------------------";
    ((*print_endline (sprintf "Branche 5 : (%b, %d, _, (%b, _))" ps pk m);*)
          (Kcas.Backoff.once b; loop ()))
        end
      |_ -> raise Exit
    in loop ()
  ;;

  let list_delete sentinel k =
    (*print_endline "LIST_DELETE";*)
    let b = Kcas.Backoff.create () in
    let rec loop () =
      (*print_endline "LIST_DELETE_LOOP";*)
      let (prev, next) = list_find sentinel k in
      match prev, next with
      |Node(ps, pk, pv, pnext), (nm, (Node(ns, nk, nv, nnext))) when not ns && nk = k ->
        (*print_endline (sprintf "ns = %b && nk = %d ?= %d = k" ns nk k);
(*        if not ns && nk = k then begin*)
        print_endline (sprintf "Delete, noeud trouve prev : (%b, %d)    n : (%b, %d)" ps pk ns nk);*)
        let vnnext = Cas.get nnext in
        if Cas.cas nnext vnnext (true, snd vnnext) then begin
          (*print_endline "1er CAS OK";*)
          if not (Cas.cas pnext next (false, snd vnnext)) then
            (Cas.set nnext vnnext; Kcas.Backoff.once b; loop ())
          else
            true
        end else
          (Kcas.Backoff.once b; loop ())
(*        end else ()*)
      |_ ->
        (*print_endline "Non déso";
        if snd next = Nil then print_endline "next encore nil";
        if prev = Nil then print_endline "prev encore nil";*)
        false
    in loop ()
  ;;

  let get_size_of_access a =
    let rec loop a out =
      match Cas.get a.(0) with
      |Allocated(a') -> loop a' (nb_bucket * out)
      |_ -> out
    in loop a nb_bucket
  ;;

  let rec help_resize t old_access old_access_size =
    print_endline (sprintf "TH%d : HELP_RESIZE (size : (%d, %d)    content : %d)" (Domain.self ()) old_access_size (Cas.get t.access_size) (Cas.get t.content));
    let new_a = Array.init nb_bucket (fun i -> Cas.ref Uninitialized) in
    Cas.set new_a.(0) (Allocated(old_access));
    let rec loop () =
      match Cas.get t.resize with
      |Some(new_access_size) as old_resize -> begin
        if (get_size_of_access (Cas.get t.access) >= new_access_size || Cas.cas t.access old_access new_a) &&
           ((Cas.get t.access_size) >= new_access_size || Cas.cas t.access_size old_access_size new_access_size) &&
           ((Cas.get t.resize) <> old_resize || Cas.cas t.resize old_resize None) then
          check_size t
        else
          loop ()
      end
      |None -> check_size t
    in loop ()
  and check_size t =
    let old_access = Cas.get t.access in
    let old_access_size = Cas.get t.access_size in
    let s = Cas.get t.size in
    let c = Cas.get t.content in
    match Cas.get t.resize with
    |Some(_) -> help_resize t old_access old_access_size
    |None when c / s > load ->
      if 2*s <= old_access_size then begin
        print_endline (sprintf "TH%d : CHECK_SIZE OVERLOAD    access_size : %d    s : %d    c : %d" (Domain.self ()) old_access_size s c);
        Cas.cas t.size s (2*s); check_size t
      end else if Cas.cas t.resize None (Some(nb_bucket * old_access_size)) then begin
        print_endline (sprintf "TH%d : CHECK_SIZE EXTEND    access_size : %d    s : %d    c : %d" (Domain.self ()) old_access_size s c);
        help_resize t old_access old_access_size
     end  else
        check_size t
    |_ -> ()
  ;;

  let create () =
    (*print_endline "CREATE";*)
    let nil = Cas.ref (false, Nil) in
    let n1 = Cas.ref (false, Node(true, 1, None, nil)) in
    let n0 = Cas.ref (false, Node(true, 0, None, n1)) in
    let tab = Array.init nb_bucket (fun i -> Cas.ref Uninitialized) in
    Cas.set tab.(0) (Initialized(true, 0, None, n1));
    Cas.set tab.(1) (Initialized(true, 1, None, nil));
    {
      access      = Cas.ref tab;
      store       = n0;
      size        = Cas.ref 2;
      content     = Cas.ref 0;
      access_size = Cas.ref nb_bucket;
      resize      = Cas.ref None;
    }
  ;;

  let hash t k =
    (*print_endline (sprintf "HASH : %d mod %d = %d" k (Cas.get t.size) (k mod (Cas.get t.size)));*)
    k mod (Cas.get t.size)
  ;;

  let get_closest_power n =
    (*print_endline "GET_CLOSEST_POWER";*)
    let rec loop out =
      let new_out = out lsl 1 in
      if new_out  > n then
        out
      else
        loop new_out
    in loop 1
  ;;

  let rec get_bucket t hk =
    (*print_endline (sprintf "TH%d : GET_BUCKET %d  (size : (%d, %d)    content : %d)" (Domain.self ()) hk (Cas.get t.size) (Array.length (Cas.get t.access)) (Cas.get t.content));*)
    (*print_endline (to_string t);*)
    let rec access_bucket a ind size =
      let tmp_ind = ind / size in
      let new_ind = ind mod size in
      let new_size = size / nb_bucket in
      (*print_endline (sprintf "TH%d : ACCESS BUCKET (ind: %d, new_ind: %d, new_size: %d" (Domain.self ()) tmp_ind new_ind new_size);*)
      match Cas.get a.(tmp_ind) with
      |Uninitialized -> initialise_bucket a tmp_ind size; access_bucket a ind size
      |Allocated(a') -> access_bucket a' new_ind new_size
      |Initialized(s) -> s
    and initialise_bucket a ind size =
      (*print_endline (sprintf "TH%d : INIT_BUCKET" (Domain.self ()));*)
      let new_elem =
        (*print_endline (sprintf "TH%d : NEW_ELEM" (Domain.self ()));*)
        if size = 1 then begin
          (*print_endline (sprintf "TH%d : INIT BUCKET SIZE = 1" (Domain.self ()));*)
          let prev_hk = hk mod (get_closest_power hk) in
          let prev_s = get_bucket t prev_hk in
          let (s, k, v, next) = list_insert prev_s true hk None in
          Initialized(s, k, v, next)
        end else
          Allocated(Array.init nb_bucket (fun i -> Cas.ref Uninitialized))
      in
      Cas.cas a.(ind) Uninitialized new_elem;
      ()
    in
    let size = (Cas.get t.access_size) / nb_bucket in
    access_bucket (Cas.get t.access) hk size
  ;;

  let find t k =
    (*print_endline "FIND";*)
    check_size t;
    let hk = hash t k in
    let s = get_bucket t hk in
    let (prev, next) = list_find s k in
    match snd next with
    |Node(ns, nk, nv, _) when split_compare nk k = 0 -> nv
    |_ -> None
  ;;

  let mem t k =
    (*print_endline "MEM";*)
    check_size t;
    let hk = hash t k in
    let s = get_bucket t hk in
    let (prev, next) = list_find s k in
    match snd next with
    |Node(ns, nk, nv, _) when split_compare nk k = 0 -> true
    |_ -> false
  ;;


  let rec add t k v =
    check_size t;
    (*print_endline (sprintf "TH%d : ADD %d" (Domain.self ()) k);*)
    let hk = hash t k in
    let s = get_bucket t hk in
    let _ = list_insert s false k (Some(v)) in
    Cas.incr t.content
  ;;

  let remove t k =
    (*print_endline (sprintf "TH%d : REMOVE %d" (Domain.self ()) k);*)
    (*print_endline "REMOVE";*)
    check_size t;
    let hk = hash t k in
    let s = get_bucket t hk in
    if list_delete s k then
      Cas.decr t.content
  ;;

end;;
