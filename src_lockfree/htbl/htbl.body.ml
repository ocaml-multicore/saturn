open Htbl_utils

type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)

type ('k, 'v, _) tdt =
  | Nil : ('k, 'v, [> `Nil ]) tdt
  | Cons : {
      key : 'k;
      value : 'v;
      rest : ('k, 'v, [ `Nil | `Cons ]) tdt;
    }
      -> ('k, 'v, [> `Cons ]) tdt
  | Nil_with_size : {
      mutable size_modifier : Size.once;
    }
      -> ('k, 'v, [> `Nil_with_size ]) tdt
  | Cons_with_size : {
      mutable size_modifier : Size.once;
      key : 'k;
      value : 'v;
      rest : ('k, 'v, [ `Nil | `Cons ]) tdt;
    }
      -> ('k, 'v, [> `Cons_with_size ]) tdt
  | Resize : {
      spine : ('k, 'v, [ `Nil_with_size | `Cons_with_size | `Nil ]) tdt;
    }
      -> ('k, 'v, [> `Resize ]) tdt
      (** During resizing and snapshotting target buckets will be initialized
          with a physically unique [Resize] value and the source buckets will
          then be gradually updated to [Resize] values and the target buckets
          updated with data from the source buckets. *)

type ('k, 'v) bucket =
  | B :
      ('k, 'v, [< `Nil_with_size | `Cons_with_size | `Resize | `Nil ]) tdt
      -> ('k, 'v) bucket
[@@unboxed]

type ('k, 'v) pending =
  | Nothing
  | Resize of { buckets : ('k, 'v) bucket Atomic_array.t; size : Size.t }

type ('k, 'v) state = {
  hash : 'k -> int;
  buckets : ('k, 'v) bucket Atomic_array.t;
  equal : 'k -> 'k -> bool;
  size : Size.t;
  pending : ('k, 'v) pending;
  min_buckets : int;
  max_buckets : int;
}
(** This record is [7 + 1] words and should be aligned on such a boundary on the
    second generation heap.  It is probably not worth it to pad it further. *)

type ('k, 'v) t = ('k, 'v) state Atomic.t

let create (type k) ?hashed_type ?min_buckets ?max_buckets () =
  let min_buckets =
    match min_buckets with
    | None -> min_buckets_default
    | Some n ->
        let n = Int.max lo_buckets n |> Int.min hi_buckets in
        ceil_pow_2_minus_1 (n - 1) + 1
  in
  let max_buckets =
    match max_buckets with
    | None -> Int.max min_buckets max_buckets_default
    | Some n ->
        let n = Int.max min_buckets n |> Int.min hi_buckets in
        ceil_pow_2_minus_1 (n - 1) + 1
  in
  let equal, hash =
    match hashed_type with
    | None ->
        (( = ), Stdlib.Hashtbl.seeded_hash (Int64.to_int (Random.bits64 ())))
    | Some ((module Hashed_type) : k hashed_type) ->
        (Hashed_type.equal, Hashed_type.hash)
  in
  {
    hash;
    buckets = Atomic_array.make min_buckets (B Nil);
    equal;
    size = Size.create ();
    pending = Nothing;
    min_buckets;
    max_buckets;
  }
  |> Atomic.make_contended

(* *)

let hashed_type_of (type k) (t : (k, _) t) : k hashed_type =
  let r = Atomic.get t in
  (module struct
    type t = k

    let hash = r.hash
    and equal = r.equal
  end)

let min_buckets_of t = (Atomic.get t).min_buckets
let max_buckets_of t = (Atomic.get t).max_buckets

(* *)

let rec take_at backoff size bs i =
  let (B old_bucket) = Atomic_array.unsafe_fenceless_get bs i in
  begin
    (* Make sure size_modifier has been updated. *)
    match old_bucket with
    | Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ } ->
        if size_modifier != Size.used_once then
          Size.update_once size size_modifier
    | _ -> ()
  end;

  match old_bucket with
  | (Nil_with_size _ | Cons_with_size _ | Nil) as spine -> begin
      if
        Atomic_array.unsafe_compare_and_set bs i (B spine)
          (B (Resize { spine }))
      then spine
      else take_at (Backoff.once backoff) size bs i
    end
  | Resize spine_r -> spine_r.spine

let rec copy_all r target i t step =
  let i = (i + step) land (Atomic_array.length target - 1) in
  let spine = take_at Backoff.default r.size r.buckets i in
  let (B before) = Atomic_array.unsafe_fenceless_get target i in
  (* The [before] value is physically different for each resize and so checking
     that the resize has not finished is sufficient to ensure that the
     [compare_and_set] below does not disrupt the next resize. *)
  Atomic.get t == r
  && begin
       begin
         match before with
         | Resize _ ->
             Atomic_array.unsafe_compare_and_set target i (B before) (B spine)
             |> ignore
         | Nil_with_size _ | Cons_with_size _ | Nil -> ()
       end;
       i = 0 || copy_all r target i t step
     end

(* *)

let[@tail_mod_cons] rec filter_ t msk chk = function
  | Nil -> Nil
  | Cons r ->
      if t r.key land msk = chk then
        Cons { r with rest = filter_ t msk chk r.rest }
      else filter_ t msk chk r.rest

let[@tail_mod_cons] rec filter_fst t mask chk = function
  | Nil -> Nil
  | Cons r ->
      if t r.key land mask = chk then
        Cons_with_size
          {
            key = r.key;
            value = r.value;
            size_modifier = Size.used_once;
            rest = filter_ t mask chk r.rest;
          }
      else filter_fst t mask chk r.rest

let filter t mask chk :
    ('a, 'b, [ `Cons_with_size | `Nil_with_size | `Nil ]) tdt ->
    ('a, 'b, [> `Cons_with_size | `Nil_with_size | `Nil ]) tdt = function
  | Nil_with_size s -> Nil_with_size s
  | Cons_with_size r -> begin
      if t r.key land mask = chk then
        Cons_with_size { r with rest = filter_ t mask chk r.rest }
      else filter_fst t mask chk r.rest
    end
  | Nil -> Nil

let rec split_all r target i t step =
  let i = (i + step) land (Atomic_array.length r.buckets - 1) in
  let spine = take_at Backoff.default r.size r.buckets i in
  let high = Atomic_array.length r.buckets in
  let after_lo = filter r.hash high 0 spine in
  let after_hi = filter r.hash high high spine in
  let (B before_lo) = Atomic_array.unsafe_fenceless_get target i in
  let (B before_hi) = Atomic_array.unsafe_fenceless_get target (i + high) in
  (* The [before_lo] and [before_hi] values are physically different for each
     resize and so checking that the resize has not finished is sufficient to
     ensure that the [compare_and_set] below does not disrupt the next
     resize. *)
  Atomic.get t == r
  && begin
       begin
         match before_lo with
         | Resize _ ->
             Atomic_array.unsafe_compare_and_set target i (B before_lo)
               (B after_lo)
             |> ignore
         | Nil_with_size _ | Cons_with_size _ | Nil -> ()
       end;
       begin
         match before_hi with
         | Resize _ ->
             Atomic_array.unsafe_compare_and_set target (i + high) (B before_hi)
               (B after_hi)
             |> ignore
         | Nil_with_size _ | Cons_with_size _ | Nil -> ()
       end;
       i = 0 || split_all r target i t step
     end

(* *)

let[@tail_mod_cons] rec merge rest = function
  | Nil -> rest
  | Cons r -> Cons { r with rest = merge rest r.rest }

let merge size
    (rest : ('a, 'b, [ `Nil_with_size | `Cons_with_size | `Nil ]) tdt) :
    ('a, 'b, [ `Nil_with_size | `Cons_with_size | `Nil ]) tdt ->
    ('a, 'b, [ `Nil_with_size | `Cons_with_size | `Nil ]) tdt = function
  | Nil_with_size r ->
      (* Each size_modifier that is going to be removed need to be applied *)
      if r.size_modifier != Size.used_once then
        Size.update_once size r.size_modifier;
      rest
  | Nil -> rest
  | Cons_with_size r -> begin
      begin
        match rest with
        | Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ }
          ->
            if size_modifier != Size.used_once then
              Size.update_once size size_modifier
        | Nil -> ()
      end;
      match rest with
      | Nil_with_size _ -> Cons_with_size r
      | Nil -> Cons_with_size r
      | Cons_with_size r' -> begin
          Cons_with_size
            {
              r with
              rest =
                merge
                  (Cons { key = r'.key; value = r'.value; rest = r'.rest })
                  r.rest;
            }
        end
    end

let rec merge_all r target i t step =
  let i = (i + step) land (Atomic_array.length target - 1) in
  let spine_lo = take_at Backoff.default r.size r.buckets i in
  let spine_hi =
    take_at Backoff.default r.size r.buckets (i + Atomic_array.length target)
  in
  let after = merge r.size spine_lo spine_hi in
  let (B before) = Atomic_array.unsafe_fenceless_get target i in
  (* The [before] value is physically different for each resize and so checking
     that the resize has not finished is sufficient to ensure that the
     [compare_and_set] below does not disrupt the next resize. *)
  Atomic.get t == r
  && begin
       begin
         match before with
         | Resize _ ->
             Atomic_array.unsafe_compare_and_set target i (B before) (B after)
             |> ignore
         | Nil_with_size _ | Cons_with_size _ | Nil -> ()
       end;
       i = 0 || merge_all r target i t step
     end

(* *)

let[@inline never] rec finish t r =
  match r.pending with
  | Nothing -> r
  | Resize { buckets; size } ->
      let high_source = Atomic_array.length r.buckets in
      let high_target = Atomic_array.length buckets in
      (* We step by random amount to better allow cores to work in parallel.
         The number of buckets is always a power of two, so any odd number is
         relatively prime or coprime. *)
      let step = Int64.to_int (Random.bits64 ()) lor 1 in
      if
        if high_source < high_target then begin
          (* We are growing the table. *)
          split_all r buckets 0 t step
        end
        else if high_target < high_source then begin
          (* We are shrinking the table. *)
          merge_all r buckets 0 t step
        end
        else begin
          (* We are snapshotting the table. *)
          copy_all r buckets 0 t step
        end
      then
        let new_r = { r with buckets; size; pending = Nothing } in
        if Atomic.compare_and_set t r new_r then new_r
        else finish t (Atomic.get t)
      else finish t (Atomic.get t)

(* *)

(** This must be called with [r.pending == Nothing]. *)
let[@inline never] try_resize t r new_capacity ~clear =
  (* We must make sure that on every resize we use a physically different
     [Resize _] value to indicate unprocessed target buckets.  The use of
     [Sys.opaque_identity] below ensures that a new value is allocated. *)
  let resize_avoid_aba =
    if clear then B Nil else B (Resize { spine = Sys.opaque_identity Nil })
  in
  let buckets = Atomic_array.make new_capacity resize_avoid_aba in
  let new_r =
    {
      r with
      pending =
        Resize { buckets; size = (if clear then Size.create () else r.size) };
    }
  in
  Atomic.compare_and_set t r new_r
  && begin
       finish t new_r |> ignore;
       true
     end

let adjust_size t r node mask result =
  begin
    match node with
    | Nil_with_size nil_r ->
        if nil_r.size_modifier != Size.used_once then begin
          Size.update_once r.size nil_r.size_modifier;
          nil_r.size_modifier <- Size.used_once
        end
    | Cons_with_size cons_r ->
        if cons_r.size_modifier != Size.used_once then begin
          Size.update_once r.size cons_r.size_modifier;
          cons_r.size_modifier <- Size.used_once
        end
    | _ -> ()
  end;

  if
    r.pending == Nothing
    && Int64.to_int (Random.bits64 ()) land mask = 0
    && Atomic.get t == r
  then begin
    (* Reading the size is potentially expensive, so we only check it
       occasionally.  The bigger the table the less frequently we should need to
       resize. *)
    let size = Size.get r.size in
    let capacity = Atomic_array.length r.buckets in
    if capacity < size && capacity < r.max_buckets then
      try_resize t r (capacity + capacity) ~clear:false |> ignore
    else if r.min_buckets < capacity && size + size + size < capacity then
      try_resize t r (capacity lsr 1) ~clear:false |> ignore
  end;
  result

(* *)

(** [get] only returns with a state where [pending = Nothing]. *)
let[@inline] get t =
  let r = Atomic.get t in
  if r.pending == Nothing then r else finish t r

(* *)

let rec exists t key = function
  | Nil -> false
  | Cons r ->
      let result = t r.key key in
      if result then result else exists t key r.rest

let rec mem r key bucket =
  begin
    match bucket with
    | B (Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ })
      ->
        if size_modifier != Size.used_once then
          Size.update_once r.size size_modifier
    | _ -> ()
  end;

  match bucket with
  | B (Nil_with_size _ | Nil) -> false
  | B (Cons_with_size cons_r) ->
      let result = r.equal cons_r.key key in
      if result then result else exists r.equal key cons_r.rest
  | B (Resize resize_r) ->
      (* A resize is in progress.  The spine of the resize still holds what was
         in the bucket before resize reached that bucket. *)
      mem r key (B resize_r.spine)

let mem t key =
  (* Reads can proceed in parallel with writes. *)
  let r = Atomic.get t in
  let h = r.hash key in
  let mask = Atomic_array.length r.buckets - 1 in
  let i = h land mask in
  mem r key @@ Atomic_array.unsafe_fenceless_get r.buckets i

(* *)
let rec assoc t key = function
  | Nil -> raise_notrace Not_found
  | Cons r -> if t r.key key then r.value else assoc t key r.rest

let rec find_exn r key bucket =
  begin
    match bucket with
    | B (Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ })
      ->
        if size_modifier != Size.used_once then
          Size.update_once r.size size_modifier
    | _ -> ()
  end;

  match bucket with
  | B (Nil_with_size _ | Nil) -> raise_notrace Not_found
  | B (Cons_with_size cons_r) ->
      if r.equal cons_r.key key then cons_r.value
      else assoc r.equal key cons_r.rest
  | B (Resize resize_r) ->
      (* A resize is in progress.  The spine of the resize still holds what was
         in the bucket before resize reached that bucket. *)
      find_exn r key (B resize_r.spine)

let find_exn t key =
  let r = Atomic.get t in
  let h = r.hash key in
  let mask = Atomic_array.length r.buckets - 1 in
  let i = h land mask in
  find_exn r key @@ Atomic_array.unsafe_fenceless_get r.buckets i

(* *)

let rec try_add t key value backoff =
  let r = Atomic.get t in
  let h = r.hash key in
  let mask = Atomic_array.length r.buckets - 1 in
  let i = h land mask in
  let (B old_bucket) = Atomic_array.unsafe_fenceless_get r.buckets i in
  begin
    (* Make sure size_modifier has been updated. *)
    match old_bucket with
    | Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ } ->
        if size_modifier != Size.used_once then
          Size.update_once r.size size_modifier
    | _ -> ()
  end;

  match old_bucket with
  | (Nil_with_size _ | Nil) as before ->
      let after =
        Cons_with_size
          {
            size_modifier = Size.new_once r.size Size.incr;
            key;
            value;
            rest = Nil;
          }
      in
      if Atomic_array.unsafe_compare_and_set r.buckets i (B before) (B after)
      then adjust_size t r after mask true
      else try_add t key value (Backoff.once backoff)
  | Cons_with_size cons_r as before ->
      if r.equal cons_r.key key then false
      else if exists r.equal key cons_r.rest then false
      else
        let after =
          Cons_with_size
            {
              size_modifier = Size.new_once r.size Size.incr;
              key;
              value;
              rest =
                Cons
                  { key = cons_r.key; value = cons_r.value; rest = cons_r.rest };
            }
        in
        if Atomic_array.unsafe_compare_and_set r.buckets i (B before) (B after)
        then adjust_size t r after mask true
        else try_add t key value (Backoff.once backoff)
  | Resize _ ->
      let _ = finish t (Atomic.get t) in
      try_add t key value Backoff.default

let[@inline] try_add t key value = try_add t key value Backoff.default

(* *)

type ('v, _, _) op =
  | Compare : ('v, 'v, bool) op
  | Exists : ('v, _, bool) op
  | Return : ('v, _, 'v) op

let rec try_reassoc :
    type v c r. (_, v) t -> _ -> c -> v -> (v, c, r) op -> _ -> r =
 fun t key present future op backoff ->
  let r = Atomic.get t in
  let h = r.hash key in
  let mask = Atomic_array.length r.buckets - 1 in
  let i = h land mask in
  let not_found (type v c r) (op : (v, c, r) op) : r =
    match op with
    | Compare -> false
    | Exists -> false
    | Return -> raise_notrace Not_found
  in

  let old_bucket : (_, v) bucket =
    Atomic_array.unsafe_fenceless_get r.buckets i
  in
  begin
    (* Make sure size_modifier has been updated. *)
    match old_bucket with
    | B (Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ })
      ->
        if size_modifier != Size.used_once then begin
          Size.update_once r.size size_modifier
        end
    | _ -> ()
  end;

  match old_bucket with
  | B (Nil | Nil_with_size _) -> not_found op
  | B (Cons_with_size cons_r as before) -> begin
      if r.equal cons_r.key key then
        if
          match op with
          | Exists | Return -> true
          | Compare -> cons_r.value == present
        then
          let after = Cons_with_size { cons_r with value = future } in
          if
            Atomic_array.unsafe_compare_and_set r.buckets i (B before) (B after)
          then
            match op with
            | Compare -> true
            | Exists -> true
            | Return -> cons_r.value
          else try_reassoc t key present future op (Backoff.once backoff)
        else not_found op
      else
        let[@tail_mod_cons] rec reassoc :
            type v c r.
            _ -> _ -> c -> v -> (v, c, r) op -> (_, v, 't) tdt -> (_, v, 't) tdt
            =
         fun t key present future op -> function
          | Nil -> raise_notrace Not_found
          | Cons r ->
              if t key r.key then
                match op with
                | Exists | Return -> Cons { r with value = future }
                | Compare ->
                    if r.value == present then Cons { r with value = future }
                    else raise_notrace Not_found
              else Cons { r with rest = reassoc t key present future op r.rest }
        in
        match reassoc r.equal key present future op cons_r.rest with
        | rest ->
            let after = Cons_with_size { cons_r with rest } in
            if
              Atomic_array.unsafe_compare_and_set r.buckets i (B before)
                (B after)
            then
              match op with
              | Compare -> true
              | Exists -> true
              | Return -> assoc r.equal key cons_r.rest
            else try_reassoc t key present future op (Backoff.once backoff)
        | exception Not_found -> not_found op
    end
  | B (Resize _) ->
      let _ = finish t (Atomic.get t) in
      try_reassoc t key present future op Backoff.default

let[@inline] try_set t key future =
  try_reassoc t key future future Exists Backoff.default

let[@inline] try_compare_and_set t key present future =
  try_reassoc t key present future Compare Backoff.default

let[@inline] set_exn t key value =
  try_reassoc t key key value Return Backoff.default

(* *)
let rec try_dissoc : type v c r. (_, v) t -> _ -> c -> (v, c, r) op -> _ -> r =
 fun t key present op backoff ->
  let r = Atomic.get t in
  let h = r.hash key in
  let mask = Atomic_array.length r.buckets - 1 in
  let i = h land mask in
  let not_found (type v c r) (op : (v, c, r) op) : r =
    match op with
    | Compare -> false
    | Exists -> false
    | Return -> raise_notrace Not_found
  in

  let old_bucket : (_, v) bucket =
    Atomic_array.unsafe_fenceless_get r.buckets i
  in
  begin
    (* Make sure size_modifier has been updated. *)
    match old_bucket with
    | B (Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ })
      ->
        if size_modifier != Size.used_once then begin
          Size.update_once r.size size_modifier
        end
    | _ -> ()
  end;

  match old_bucket with
  | B (Nil_with_size _ | Nil) -> not_found op
  | B (Cons_with_size cons_r as before) -> begin
      let size_modifier = Size.new_once r.size Size.decr in
      if r.equal cons_r.key key then
        if
          match op with
          | Exists | Return -> true
          | Compare -> cons_r.value == present
        then
          let after =
            match cons_r.rest with
            | Nil -> Nil_with_size { size_modifier }
            | Cons next ->
                Cons_with_size
                  {
                    size_modifier;
                    key = next.key;
                    value = next.value;
                    rest = next.rest;
                  }
          in
          if
            Atomic_array.unsafe_compare_and_set r.buckets i (B before) (B after)
          then
            let res : r =
              match op with
              | Compare -> true
              | Exists -> true
              | Return -> cons_r.value
            in
            adjust_size t r after mask res
          else try_dissoc t key present op (Backoff.once backoff)
        else not_found op
      else
        let[@tail_mod_cons] rec dissoc :
            type v c r.
            _ -> _ -> c -> (v, c, r) op -> (_, v, 't) tdt -> (_, v, 't) tdt =
         fun t key present op -> function
          | Nil -> raise_notrace Not_found
          | Cons r ->
              if t key r.key then
                match op with
                | Exists | Return -> r.rest
                | Compare ->
                    if r.value == present then r.rest
                    else raise_notrace Not_found
              else Cons { r with rest = dissoc t key present op r.rest }
        in
        match dissoc r.equal key present op cons_r.rest with
        | (Nil | Cons _) as rest ->
            let after = Cons_with_size { cons_r with rest; size_modifier } in
            if
              Atomic_array.unsafe_compare_and_set r.buckets i (B before)
                (B after)
            then
              let res : r =
                match op with
                | Compare -> true
                | Exists -> true
                | Return -> assoc r.equal key cons_r.rest
              in
              adjust_size t r after mask res
            else try_dissoc t key present op (Backoff.once backoff)
        | exception Not_found -> not_found op
    end
  | B (Resize _) ->
      let _ = finish t (Atomic.get t) in
      try_dissoc t key present op Backoff.default

let[@inline] try_remove t key = try_dissoc t key key Exists Backoff.default

let[@inline] try_compare_and_remove t key present =
  try_dissoc t key present Compare Backoff.default

let[@inline] remove_exn t key = try_dissoc t key key Return Backoff.default

(* *)

let rec snapshot t ~clear backoff =
  let r = get t in
  if try_resize t r (Atomic_array.length r.buckets) ~clear then begin
    (* At this point the resize has been completed and a new array is used for
       buckets and [r.buckets] now has an immutable copy of what was in the hash
       table. *)
    let snapshot = r.buckets in
    let rec loop i kvs () =
      match kvs with
      | Nil ->
          if i = Atomic_array.length snapshot then Seq.Nil
          else
            loop (i + 1)
              (match Atomic_array.unsafe_fenceless_get snapshot i with
              | B (Resize { spine = Nil_with_size _ | Nil }) -> Nil
              | B (Resize { spine = Cons_with_size cons_r }) ->
                  Cons
                    {
                      key = cons_r.key;
                      value = cons_r.value;
                      rest = cons_r.rest;
                    }
              | B (Nil_with_size _ | Cons_with_size _ | Nil) ->
                  (* After resize only [Resize] values should be left in the old
                     buckets. *)
                  assert false)
              ()
      | Cons r -> Seq.Cons ((r.key, r.value), loop i r.rest)
    in
    loop 0 Nil
  end
  else snapshot t ~clear (Backoff.once backoff)

let to_seq t = snapshot t ~clear:false Backoff.default
let remove_all t = snapshot t ~clear:true Backoff.default

(* *)

let find_random_exn t =
  let try_find_random_non_empty_bucket t =
    let r = Atomic.get t in
    let buckets = r.buckets in
    let seed = Int64.to_int (Random.bits64 ()) in
    let rec try_find_random_non_empty_bucket buckets seed i =
      let (B old_bucket) = Atomic_array.unsafe_fenceless_get buckets i in
      begin
        match old_bucket with
        | Nil_with_size { size_modifier } | Cons_with_size { size_modifier; _ }
          ->
            if size_modifier != Size.used_once then
              Size.update_once r.size size_modifier
        | Resize
            {
              spine =
                ( Nil_with_size { size_modifier }
                | Cons_with_size { size_modifier; _ } );
            } ->
            if size_modifier != Size.used_once then
              Size.update_once r.size size_modifier
        | _ -> ()
      end;

      match old_bucket with
      | Nil_with_size _ | Nil | Resize { spine = Nil_with_size _ | Nil } ->
          let mask = Atomic_array.length buckets - 1 in
          let i = (i + 1) land mask in
          if i <> seed land mask then
            try_find_random_non_empty_bucket buckets seed i
          else Nil
      | Cons_with_size cons_r | Resize { spine = Cons_with_size cons_r } ->
          Cons { key = cons_r.key; value = cons_r.value; rest = cons_r.rest }
    in
    try_find_random_non_empty_bucket buckets seed
      (seed land (Atomic_array.length buckets - 1))
  in
  match try_find_random_non_empty_bucket t with
  | (Cons cons_r as spine : (_, _, [< `Nil | `Cons ]) tdt) ->
      (* We found a non-empty bucket - the fast way. *)
      if cons_r.rest == Nil then cons_r.key
      else
        let rec length spine n =
          match spine with Nil -> n | Cons r -> length r.rest (n + 1)
        in
        let n = length cons_r.rest 1 in
        let rec nth spine i =
          match spine with
          | Nil -> impossible ()
          | Cons r -> if i <= 0 then r.key else nth r.rest (i - 1)
        in
        nth spine (Random.int n)
  | Nil ->
      (* We couldn't find a non-empty bucket - the slow way. *)
      let bindings = to_seq t |> Array.of_seq in
      let n = Array.length bindings in
      if n <> 0 then fst (Array.unsafe_get bindings (Random.int n))
      else raise_notrace Not_found

let length t = (Atomic.get t).size |> Size.get
