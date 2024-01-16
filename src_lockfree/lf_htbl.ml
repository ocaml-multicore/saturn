module Atomic = Transparent_atomic

module Key = struct
  let reverse x =
    (* works for int32 *)
    let x = x land 0xff_ff_ff_ff in
    let x = ((x land 0xaa_aa_aa_aa) lsr 1) lor ((x land 0x55_55_55_55) lsl 1) in
    let x = ((x land 0xcc_cc_cc_cc) lsr 2) lor ((x land 0x33_33_33_33) lsl 2) in
    let x = ((x land 0xf0_f0_f0_f0) lsr 4) lor ((x land 0x0f_0f_0f_0f) lsl 4) in
    let x = ((x land 0xff_00_ff_00) lsr 8) lor ((x land 0x00_ff_00_ff) lsl 8) in
    (x lsr 16) lor (x lsl 16) land 0xffffffff

  (*  let compute_hkey k = reverse k lor 0x00_00_00_01*)

  (** unset most significant turn on bit (for int32) *)
  let _unset_msb key =
    let a = key lor (key lsr 1) in
    let a = a lor (a lsr 2) in
    let a = a lor (a lsr 4) in
    let a = a lor (a lsr 8) in
    let a = a lor (a lsr 16) in
    (a lsr 1) land key
end

module Internal = struct
  open Llist

  let compare = Int.compare

  type 'v bucket = (int, 'v) link Atomic.t

  let first_bucket () =
    Atomic.make
      (Link
         {
           incr = Size.used_once;
           decr = Size.used_once;
           bindings = [];
           next = Null;
         })

  let rec add_new_bucket t size key =
    add_new_bucket_rec size t key t (Atomic.get t)

  and add_new_bucket_rec size t key prev curr =
    let found, prev, curr, _ = find_node_rec compare size t key prev curr in
    if found == 0 then
      match curr.next with Null -> assert false | Node node -> node.content
    else begin
      let incr = Size.used_once in
      let content = Atomic.make @@ Link { curr with bindings = []; incr } in
      let after = (Node { key; content } : (_, _, [ `Node ]) node) in
      if
        Atomic.compare_and_set prev (Link curr)
          (Link { curr with next = after })
      then content
      else add_new_bucket_rec size t key prev (Atomic.get prev)
    end

  let add t size = Llist.add compare size t

  let try_remove ?(empty = false) t size =
    Llist.try_remove ~empty compare size t

  let replace t size = Llist.replace compare size t
end

(* Hashtable*)
type (_, 'v) t = {
  mask : int;
  size : Size.t;
  buckets : 'v Internal.bucket array;
}

let length t = Size.get t.size

let create ~size_exponent : (_, 'v) t =
  let max_size = Int.shift_left 1 size_exponent in
  let mask = max_size - 1 in
  let size = Size.create () in
  (* This is the content of node 0 *)
  let llist = Internal.first_bucket () in

  {
    mask;
    size;
    buckets =
      (let prev = ref llist in
       Array.init max_size (fun i ->
           if i = 0 then llist
           else
             let key = Key.reverse i in
             let r = Internal.add_new_bucket !prev size key in
             prev := r;
             r));
  }

(* TODO : add collision management (register real key and not just hashed key) *)
let[@inline] rec add_rec t hashed_key value =
  let bucket_index = hashed_key land t.mask in
  let bucket = Array.get t.buckets bucket_index in
  if bucket_index == hashed_key then
    match Atomic.get bucket with
    | Link ({ bindings; _ } as before) -> begin
        (* TODO : when resizing a bucket can be marked to be removed *)
        (* TODO : this is copy/paste : to it better *)
        match bindings with
        | [] ->
            let incr = Size.new_once t.size Size.incr in
            let after = { before with bindings = value :: []; incr } in
            if Atomic.compare_and_set bucket (Link before) (Link after) then begin
              if after.incr != Size.used_once then begin
                Size.update_once t.size after.incr;
                after.incr <- Size.used_once
              end
            end
            else add_rec t hashed_key value
        | _ ->
            let after = { before with bindings = value :: bindings } in
            if not @@ Atomic.compare_and_set bucket (Link before) (Link after)
            then add_rec t hashed_key value
      end
  else Internal.add bucket t.size (Key.reverse hashed_key) value

let add (t : ('k, 'v) t) (key : 'k) (value : 'v) =
  let hashed_key = Hashtbl.hash key in
  add_rec t hashed_key value

let[@inline] rec replace_rec (t : ('k, 'v) t) (hashed_key : int) (value : 'v) =
  let bucket_index = hashed_key land t.mask in
  let bucket = Array.get t.buckets bucket_index in

  if bucket_index == hashed_key then
    match Atomic.get bucket with
    | Link ({ bindings; _ } as before) ->
        if
          Atomic.compare_and_set bucket (Link before)
            (Link { before with bindings = Llist.replace_last value bindings })
        then ()
        else replace_rec t hashed_key value
  else Internal.replace bucket t.size hashed_key value

let replace (t : ('k, 'v) t) (key : 'k) (value : 'v) =
  let hashed_key = Hashtbl.hash key |> Key.reverse in
  replace_rec t hashed_key value

let rec try_remove_rec t hashed_key =
  let bucket_index = hashed_key land t.mask in
  let bucket = Array.get t.buckets bucket_index in

  if bucket_index == hashed_key then
    match Atomic.get bucket with
    | Link { bindings = []; _ } -> false
    | Link ({ bindings = _ :: xs; _ } as before) ->
        if
          Atomic.compare_and_set bucket (Link before)
            (Link { before with bindings = xs })
        then true
        else try_remove_rec t hashed_key
  else Internal.try_remove bucket t.size hashed_key

let try_remove t key =
  let hashed_key = Hashtbl.hash key |> Key.reverse in
  try_remove_rec t hashed_key

let find_all t key =
  let hashed_key = Hashtbl.hash key |> Key.reverse in
  let bucket_index = hashed_key land t.mask in
  let bucket = Array.get t.buckets bucket_index in

  if bucket_index == hashed_key then
    match Atomic.get bucket with Link { bindings; _ } -> bindings
  else
    let found, _, _, next = Llist.find_node compare t.size bucket hashed_key in
    if found != 0 then [] else next.bindings

let mem t key =
  let hashed_key = Hashtbl.hash key |> Key.reverse in
  let bucket_index = hashed_key land t.mask in
  let bucket = Array.get t.buckets bucket_index in

  if bucket_index == hashed_key then
    match Atomic.get bucket with
    | Link { bindings = []; _ } -> false
    | _ -> true
  else
    let found, _, _, _ = Llist.find_node compare t.size bucket hashed_key in
    if found != 0 then false else true
