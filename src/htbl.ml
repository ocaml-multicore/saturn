(*
 * Copyright (c) 2023, Carine Morel <carine.morel.pro@gmail.com>
 *)

module Type = struct
  type 'b kind = Dummy | Regular of 'b
  type key = int
end

module Llist = struct
  include Type

  type 'a marked = Last | LRemove | Normal of 'a node | Remove of 'a node
  and 'a node = { key : key; value : 'a kind; next : 'a marked Atomic.t }

  type 'a t = 'a marked Atomic.t

  type 'a local = {
    prev : 'a marked Atomic.t;
    curr : 'a marked;
    next : 'a marked;
  }

  let init () : 'a t = Atomic.make Last

  let convert_to_normal = function
    | (Last | Normal _) as node -> node
    | LRemove -> Last
    | Remove node -> Normal node

  let mark_to_be_removed = function
    | Last -> LRemove
    | Normal node -> Remove node
    | _ as x -> x

  let rec find_loop key t prev curr =
    match curr with
    | Last | LRemove -> (false, { prev; curr; next = Last })
    | Normal node | Remove node -> (
        if Atomic.get prev != curr then try_again key t
        else
          let next = Atomic.get node.next in
          match next with
          | Normal _ | Last ->
              if node.key >= key then (node.key = key, { prev; curr; next })
              else find_loop key t node.next next
          | _ ->
              let next = convert_to_normal next in
              if Atomic.compare_and_set prev curr next then
                find_loop key t prev next
              else try_again key t)

  and try_again key t = find_loop key t t (Atomic.get t)

  let find key t : bool * 'a local = try_again key t

  let rec unsafe_add (key : key) (value : 'a kind) t : bool * 'a local =
    let is_found, local = find key t in
    if is_found then (false, local)
    else if
      Atomic.compare_and_set local.prev local.curr
      @@ Normal { key; next = Atomic.make local.curr; value }
    then (true, local)
    else unsafe_add key value t

  let add (key : key) (value : 'a kind) (t : 'a t) =
    fst (unsafe_add key value t)

  let exchange_value v = function
    | Normal n ->
        let prev_v = n.value in
        (prev_v, Normal { n with value = v })
    | Remove n ->
        let prev_v = n.value in
        (prev_v, Remove { n with value = v })
    | _ -> failwith "Should not happen"

  let rec replace (key : key) (value : 'a kind) (t : 'a t) =
    let is_found, local = find key t in
    if is_found then
      let prev_value, new_node = exchange_value value local.curr in
      if prev_value = value then `Replaced
      else if Atomic.compare_and_set local.prev local.curr new_node then
        `Replaced
      else replace key value t
    else if
      Atomic.compare_and_set local.prev local.curr
      @@ Normal { key; next = Atomic.make local.curr; value }
    then `Added
    else replace key value t

  let rec unsafe_remove (key : key) t =
    let is_found, local = find key t in
    if not is_found then (false, local)
    else
      let curr =
        match local.curr with
        | Normal node -> node
        | _ -> failwith " Should never happen"
      in
      if
        not
          (Atomic.compare_and_set curr.next local.next
             (mark_to_be_removed local.next))
      then unsafe_remove key t
      else if Atomic.compare_and_set local.prev local.curr local.next then
        (true, local)
      else (
        ignore (find key t);
        (true, local))

  let remove (key : key) (t : 'a t) = fst (unsafe_remove key t)
  let mem key t = fst @@ find key t
end

module Common = struct
  (* Key value used in the linked list are computed with [reverse] for the
        dummy node and with [compute_hkey] for the regular node.

     Example of a linked list, simplified to 8 bits integer. Only key
     are written, as values of regular nodes do not change the linked
     list order.

      [ 0000 0000 (0, dummy) ]
     -> [ 0001 0001 (8, regular)]
     -> [ 0100 0000 (2, dummy)]
     -> [ 0101 0001 (10, regular)]
     -> [ 0110 1001 (22, regular)]
     -> [ 1000 0000 (1, dummy)]
     -> [ 1000 1001 (17, regular)]
     -> [ 1100 0000 (3, dummy)]
  *)
  let reverse x =
    (* works for int32 *)
    let x = x land 0xff_ff_ff_ff in
    let x = ((x land 0xaa_aa_aa_aa) lsr 1) lor ((x land 0x55_55_55_55) lsl 1) in
    let x = ((x land 0xcc_cc_cc_cc) lsr 2) lor ((x land 0x33_33_33_33) lsl 2) in
    let x = ((x land 0xf0_f0_f0_f0) lsr 4) lor ((x land 0x0f_0f_0f_0f) lsl 4) in
    let x = ((x land 0xff_00_ff_00) lsr 8) lor ((x land 0x00_ff_00_ff) lsl 8) in
    (x lsr 16) lor (x lsl 16) land 0xffffffff

  let compute_hkey k = reverse k lor 0x00_00_00_01

  (** unset most significant turn on bit (for int32) *)
  let unset_msb key =
    let a = key lor (key lsr 1) in
    let a = a lor (a lsr 2) in
    let a = a lor (a lsr 4) in
    let a = a lor (a lsr 8) in
    let a = a lor (a lsr 16) in
    (a lsr 1) land key

  let new_dummy_node id llist =
    let _, local = Llist.unsafe_add (reverse id) Dummy llist in
    (* If the insertion succeeded ([is_added] = true) then
       [local.prev] contains the atomic value of the new node.

       If it failed that means another domain has already inserted
       this dummy node. In this case, [local.prev] also contains the
       value we seek as the [Llist.find] function calls by
       [Llist.unsafe_add] will have stopped at the right place. *)
    Atomic.get local.prev
end

module Htbl = struct
  open Common
  include Type

  type 'a t = { mask : int; buckets : 'a Llist.marked Atomic.t array }

  let init ~size_exponent =
    let size = Int.shift_left 1 size_exponent in
    let mask = size - 1 in
    let llist = Llist.init () in
    {
      mask;
      buckets =
        Array.init size (fun i ->
            if i = 0 then Atomic.make (new_dummy_node 0 llist)
            else Atomic.make Llist.Last);
    }

  let rec init_bucket buckets ind =
    let parent_ind = unset_msb ind in
    let parent_bucket = buckets.(parent_ind) in

    (if parent_ind <> ind then
       match Atomic.get parent_bucket with
       | Llist.Last -> init_bucket buckets parent_ind
       | LRemove -> failwith "Should never happen."
       | _ -> ());

    new_dummy_node ind parent_bucket |> Atomic.set buckets.(ind)

  (** [get_bucket_id key buckets mask] searches the bucket's index corresponding
      to [key] (= [key mod t.size]), initializes if needed and returns
      it. *)
  let get_bucket_ind key buckets mask =
    let ind = key land mask in
    let bucket = buckets.(ind) in
    match Atomic.get bucket with
    | Llist.Last ->
        init_bucket buckets ind;
        buckets.(ind)
    | LRemove -> failwith "Should never happen."
    | _ -> bucket

  let add key value { buckets; mask; _ } =
    Llist.add (compute_hkey key) (Regular value)
    @@ get_bucket_ind key buckets mask

  let replace key value { buckets; mask; _ } =
    Llist.replace (compute_hkey key) (Regular value)
    @@ get_bucket_ind key buckets mask
    |> ignore

  let find key { buckets; mask; _ } =
    let is_found, local =
      Llist.find (compute_hkey key) (get_bucket_ind key buckets mask)
    in
    if not is_found then None
    else
      match local.curr with
      | Normal { value = Regular k; _ } | Remove { value = Regular k; _ } ->
          Some k
      | _ -> failwith "Should not happen"

  let mem key { buckets; mask; _ } =
    fst @@ Llist.find (compute_hkey key) (get_bucket_ind key buckets mask)

  let remove key { buckets; mask; _ } =
    Llist.remove (compute_hkey key) (get_bucket_ind key buckets mask)
end

module Htbl_resizable = struct
  open Common
  include Type

  (* pointers to a node in linked list *)
  type 'a bucket = 'a Llist.marked Atomic.t
  type 'a segment = 'a bucket array

  type 'a t = {
    (* total item count *)
    count : int Atomic.t;
    (* current number of segments *)
    number_of_buckets : int Atomic.t;
    (* maximum number of items in a bucket (on average).  Maximun
       number of elements in an hash table [t] is [t.sizes] * [t.max] *)
    max : int;
    segments : 'a segment option Atomic.t array;
    max_number_of_buckets : int;
    segment_size : int;
  }

  let init ~size_exponent =
    let max = 2 in
    let grow_exp = 10 in
    let llist = Llist.init () in

    let segment_size = Int.shift_left 1 size_exponent in

    let exp_max = grow_exp + size_exponent in
    let max_number_of_buckets = Int.shift_left 1 exp_max in

    let first_seg =
      Array.init segment_size (fun i ->
          if i = 0 then Atomic.make (new_dummy_node 0 llist)
          else Atomic.make Llist.Last)
    in

    let segments =
      Array.init (Int.shift_left 1 grow_exp) (fun i ->
          match i with
          | 0 -> Atomic.make (Some first_seg)
          | _ -> Atomic.make None)
    in
    {
      count = Atomic.make 0;
      number_of_buckets = Atomic.make segment_size;
      segment_size;
      max_number_of_buckets;
      segments;
      max;
    }

  let is_empty { count; _ } = Atomic.get count = 0

  let get_segment t segment_ind =
    let seg = t.segments.(segment_ind) in
    match Atomic.get seg with
    | Some seg -> seg
    | None ->
        (* Can failed if another domain has already done it *)
        Atomic.compare_and_set seg None
          (Some (Array.init t.segment_size (fun _ -> Atomic.make Llist.Last)))
        |> ignore;
        (* always true as either this domain or another just changed
           the value of this cell and the only values written in
           [t.segments] are `Some` values *)
        Option.get (Atomic.get seg)

  let get_bucket_ref t ind =
    let segment_ind = ind / t.segment_size in
    let segment = get_segment t segment_ind in
    segment.(ind land (t.segment_size - 1))

  (** [init_bucket buckets ind] inits a bucket and all its parents if needed *)
  let rec init_bucket t bucket bucket_ind =
    let parent_ind = unset_msb bucket_ind in
    let parent_bucket = get_bucket_ref t parent_ind in
    (if parent_ind <> bucket_ind then
       match Atomic.get parent_bucket with
       | Llist.Last -> init_bucket t parent_bucket parent_ind
       | LRemove -> failwith "Should never happen"
       | _ -> ());
    new_dummy_node bucket_ind parent_bucket |> Atomic.set bucket

  let get_bucket key t =
    let mask_val = Atomic.get t.number_of_buckets - 1 in
    let bucket_ind = key land mask_val in
    let bucket = get_bucket_ref t bucket_ind in
    match Atomic.get bucket with
    | Llist.Last ->
        init_bucket t bucket bucket_ind;
        bucket
    | LRemove -> failwith "Should never happen"
    | _ -> bucket

  let rec grow ({ number_of_buckets; max_number_of_buckets; max; _ } as t) count
      =
    let number_of_buckets_val = Atomic.get number_of_buckets in
    if
      count > number_of_buckets_val * max
      && number_of_buckets_val < max_number_of_buckets
    then
      if
        Atomic.compare_and_set t.number_of_buckets number_of_buckets_val
          (number_of_buckets_val lsl 1)
      then ()
      else grow t count

  let add (key : key) (value : 'a) (t : 'a t) =
    let is_added =
      Llist.add (compute_hkey key) (Regular value) @@ get_bucket key t
    in
    if not is_added then false
    else (
      Atomic.fetch_and_add t.count 1 |> grow t;
      true)

  let replace (key : key) (value : 'a) (t : 'a t) =
    match
      Llist.replace (compute_hkey key) (Regular value) @@ get_bucket key t
    with
    | `Replaced -> ()
    | `Added -> Atomic.fetch_and_add t.count 1 |> grow t

  let add_no_resize key value t =
    let is_added =
      Llist.add (compute_hkey key) (Regular value) @@ get_bucket key t
    in
    if not is_added then false
    else (
      Atomic.incr t.count;
      true)

  let find key t =
    let is_found, local = Llist.find (compute_hkey key) (get_bucket key t) in
    if not is_found then None
    else
      match local.curr with
      | Normal { value = Regular k; _ } | Remove { value = Regular k; _ } ->
          Some k
      | _ -> failwith "Should not happen"

  let mem key t =
    let is_found, _ = Llist.find (compute_hkey key) (get_bucket key t) in
    is_found

  let remove key t =
    let is_removed = Llist.remove (compute_hkey key) (get_bucket key t) in
    if not is_removed then false
    else (
      Atomic.decr t.count;
      true)
end
