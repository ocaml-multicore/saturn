module type ThreadMetadataSig = sig
  type t

  val create : unit -> t
  val get_insert_count : t -> int -> int (* t -> thread ID -> insert count *)
  val get_delete_count : t -> int -> int (* t -> thread ID -> delete count *)

  val update_insert_count :
    t -> int -> int -> unit (* t -> thread ID -> old insert count -> () *)

  val update_delete_count :
    t -> int -> int -> unit (* t -> thread ID -> old delete count -> () *)

  val get_sum : t -> int
end

module ThreadMetadata : ThreadMetadataSig = struct
  type counter_pair = int Atomic.t * int Atomic.t

  type t =
    (int, counter_pair) Hashtbl.t (* thread id -> (insertions, deletions) *)

  let create () : t = Hashtbl.create 10

  let get_counters tbl thread_id : counter_pair =
    try Hashtbl.find tbl thread_id
    with Not_found ->
      let counters = (Atomic.make 0, Atomic.make 0) in
      Hashtbl.add tbl thread_id counters;
      counters

  let get_insert_count tbl thread_id =
    let insert_count, _ = get_counters tbl thread_id in
    Atomic.get insert_count

  let get_delete_count tbl thread_id =
    let _, delete_count = get_counters tbl thread_id in
    Atomic.get delete_count

  let update_insert_count tbl thread_id old_val =
    let insert_count, _ = get_counters tbl thread_id in
    ignore (Atomic.compare_and_set insert_count old_val (old_val + 1))

  let update_delete_count tbl thread_id old_val =
    let _, delete_count = get_counters tbl thread_id in
    ignore (Atomic.compare_and_set delete_count old_val (old_val + 1))

  let get_sum tbl =
    let snapshot () =
      Hashtbl.fold
        (fun _ (insert_count, delete_count) acc ->
          (Atomic.get insert_count, Atomic.get delete_count) :: acc)
        tbl []
    in
    let rec loop snapshot_vals =
      let sum =
        List.fold_left
          (fun acc (insert_count, delete_count) ->
            acc + insert_count - delete_count)
          0 snapshot_vals
      in
      let current_snapshot = snapshot () in
      if snapshot_vals = current_snapshot then sum else loop current_snapshot
    in
    loop (snapshot ())
end

type update_info = Insert of int * int | Delete of int * int

type 'a node = {
  value : 'a option;
  next : 'a node Atomic.t option;
  update_info : update_info option;
}

type 'a t = { head : 'a node Atomic.t; metadata : ThreadMetadata.t }

let create () =
  {
    head = Atomic.make { value = None; next = None; update_info = None };
    metadata = ThreadMetadata.create ();
  }

let update_metadata (s : 'a t) (update : update_info option) : unit =
  match update with
  | None -> ()
  | Some (Insert (thread_id, old_val)) ->
      ThreadMetadata.update_insert_count s.metadata thread_id old_val
  | Some (Delete (thread_id, old_val)) ->
      ThreadMetadata.update_delete_count s.metadata thread_id old_val

let help_delete node cur_node next_node =
  ignore
    (Atomic.compare_and_set node cur_node
       { cur_node with next = next_node.next })

let insert s thread_id v =
  let rec insert_aux node =
    let cur_node = Atomic.get node in
    update_metadata s cur_node.update_info;

    match cur_node.update_info with
    | Some (Delete _) -> insert_aux s.head
    | _ -> (
        match cur_node.next with
        | None -> (
            let new_node =
              {
                value = Some v;
                next = None;
                update_info =
                  Some
                    (Insert
                       ( thread_id,
                         ThreadMetadata.get_insert_count s.metadata thread_id ));
              }
            in
            match
              Atomic.compare_and_set node cur_node
                { cur_node with next = Some (Atomic.make new_node) }
            with
            | true ->
                update_metadata s new_node.update_info;
                true
            | false -> insert_aux node)
        | Some next_node_atomic -> (
            let next_node = Atomic.get next_node_atomic in
            match next_node.update_info with
            | Some (Delete _) ->
                update_metadata s next_node.update_info;
                help_delete node cur_node next_node;
                insert_aux node
            | _ -> (
                match next_node.value with
                | Some x when x = v -> false
                | Some x when x > v -> (
                    let old_insert_count =
                      ThreadMetadata.get_insert_count s.metadata thread_id
                    in
                    let new_node =
                      {
                        value = Some v;
                        next = Some next_node_atomic;
                        update_info =
                          Some (Insert (thread_id, old_insert_count));
                      }
                    in
                    let atomic_new_node = Atomic.make new_node in
                    match
                      Atomic.compare_and_set node cur_node
                        { cur_node with next = Some atomic_new_node }
                    with
                    | true ->
                        update_metadata s new_node.update_info;
                        ignore
                          (Atomic.compare_and_set atomic_new_node new_node
                             { new_node with update_info = None });
                        true
                    | false -> insert_aux node)
                | Some _ -> insert_aux next_node_atomic
                | None ->
                    failwith "impossible case: value of non-dummy node is None")
            ))
  in
  insert_aux s.head

let contains s v =
  let rec contains_aux node =
    let cur_node = Atomic.get node in
    update_metadata s cur_node.update_info;

    match cur_node.update_info with
    | Some (Delete _) -> contains_aux s.head
    | _ -> (
        match cur_node.next with
        | None -> false
        | Some next_node_atomic -> (
            let next_node = Atomic.get next_node_atomic in
            match next_node.update_info with
            | Some (Delete _) ->
                update_metadata s next_node.update_info;
                help_delete node cur_node next_node;
                contains_aux node
            | _ -> (
                match next_node.value with
                | Some x when x = v -> true
                | Some x when x < v -> contains_aux next_node_atomic
                | Some _ -> false
                | None ->
                    failwith "impossible case: value of non-dummy node is None")
            ))
  in
  contains_aux s.head

let delete s thread_id v =
  let rec delete_aux node =
    let cur_node = Atomic.get node in
    update_metadata s cur_node.update_info;

    match cur_node.update_info with
    | Some (Delete _) -> delete_aux s.head
    | _ -> (
        match cur_node.next with
        | None -> false
        | Some next_node_atomic -> (
            let next_node = Atomic.get next_node_atomic in
            update_metadata s next_node.update_info;

            match next_node.update_info with
            | Some (Delete _) ->
                help_delete node cur_node next_node;
                delete_aux node
            | _ -> (
                match next_node.value with
                | Some x when x = v -> (
                    let old_delete_count =
                      ThreadMetadata.get_delete_count s.metadata thread_id
                    in
                    let new_next_node =
                      {
                        next_node with
                        update_info =
                          Some (Delete (thread_id, old_delete_count));
                      }
                    in
                    match
                      Atomic.compare_and_set next_node_atomic next_node
                        new_next_node
                    with
                    | true ->
                        update_metadata s new_next_node.update_info;
                        help_delete node cur_node new_next_node;
                        true
                    | false -> delete_aux node)
                | Some x when x < v -> delete_aux next_node_atomic
                | Some _ -> false
                | None ->
                    failwith "impossible case: value of non-dummy node is None")
            ))
  in
  delete_aux s.head

let size s = ThreadMetadata.get_sum s.metadata
