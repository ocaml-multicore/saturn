open Kcas

(** Optimized operations on internal association lists with custom equality. *)
module Assoc = struct
  type change = Nop | Removed | Replaced | Added
  type ('k, 'v) t = Nil | Cons of { k : 'k; v : 'v; kvs : ('k, 'v) t }

  let[@inline] cons k v kvs = Cons { k; v; kvs }

  let rec fold fn accum = function
    | Nil -> accum
    | Cons { k; v; kvs } -> fold fn (fn k v accum) kvs

  let length kvs = fold (fun _ _ n -> n + 1) 0 kvs

  let rec rev_append kvs accum =
    match kvs with
    | Nil -> accum
    | Cons { k; v; kvs } -> rev_append kvs (Cons { k; v; kvs = accum })

  let rev kvs = rev_append kvs Nil

  let rec iter fn = function
    | Nil -> ()
    | Cons { k; v; kvs } ->
        fn k v;
        iter fn kvs

  let iter_rev fn = function
    | Nil -> ()
    | Cons { k; v; kvs = Nil } -> fn k v
    | kvs -> kvs |> rev |> iter fn

  let rec find_opt equal k' = function
    | Nil -> None
    | Cons r -> if equal r.k k' then Some r.v else find_opt equal k' r.kvs

  let[@tail_mod_cons] rec find_all equal k' = function
    | Nil -> []
    | Cons { k; v; kvs } ->
        if equal k k' then v :: find_all equal k' kvs else find_all equal k' kvs

  let rec mem equal k' = function
    | Nil -> false
    | Cons r -> equal r.k k' || mem equal k' r.kvs

  let[@tail_mod_cons] rec remove equal change k' = function
    | Nil -> Nil
    | Cons r ->
        if equal r.k k' then begin
          change := Removed;
          r.kvs
        end
        else Cons { k = r.k; v = r.v; kvs = remove equal change k' r.kvs }

  let[@tail_mod_cons] rec replace equal change k' v' = function
    | Nil ->
        change := Added;
        Cons { k = k'; v = v'; kvs = Nil }
    | Cons r as original ->
        if equal r.k k' then
          if r.v == v' then original
          else begin
            change := Replaced;
            Cons { k = r.k; v = v'; kvs = r.kvs }
          end
        else Cons { k = r.k; v = r.v; kvs = replace equal change k' v' r.kvs }

  let[@tail_mod_cons] rec filter_map fn delta = function
    | Nil -> Nil
    | Cons { k; v; kvs } -> begin
        match fn k v with
        | None ->
            decr delta;
            filter_map fn delta kvs
        | Some v' -> Cons { k; v = v'; kvs = filter_map fn delta kvs }
      end
end

type ('k, 'v) pending =
  | Nothing
  | Rehash of {
      state : int Loc.t;
      new_capacity : int;
      new_buckets : ('k, 'v) Assoc.t Loc.t array Loc.t;
    }
  | Snapshot of { state : int Loc.t; snapshot : ('k, 'v) Assoc.t array Loc.t }
  | Filter_map of {
      state : int Loc.t;
      fn : 'k -> 'v -> 'v option;
      raised : exn Loc.t;
      new_buckets : ('k, 'v) Assoc.t Loc.t array Loc.t;
    }

type ('k, 'v) r = {
  pending : ('k, 'v) pending;
  length : Accumulator.t;
  buckets : ('k, 'v) Assoc.t Loc.t array;
  hash : 'k -> int;
  equal : 'k -> 'k -> bool;
  min_buckets : int;
  max_buckets : int;
}

type ('k, 'v) t = ('k, 'v) r Loc.t
type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)

let lo_buckets = 1 lsl 5
let hi_buckets = (Sys.max_array_length lsr 1) + 1
let () = assert (Bits.is_pow_2 hi_buckets)
let min_buckets_default = lo_buckets
let max_buckets_default = Int.min hi_buckets (1 lsl 30 (* Limit of [hash] *))

module HashedType = struct
  let pack (type k) hash equal : k hashed_type =
    (module struct
      type t = k

      let hash = hash
      and equal = equal
    end)

  let unpack (type k) ((module HashedType) : k hashed_type) =
    (HashedType.hash, HashedType.equal)

  let is_same_as (type k) hash equal ((module HashedType) : k hashed_type) =
    hash == HashedType.hash && equal == HashedType.equal
end

let create ?hashed_type ?min_buckets ?max_buckets ?n_way () =
  let min_buckets =
    match min_buckets with
    | None -> min_buckets_default
    | Some c -> Int.max lo_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  in
  let t = Loc.make (Obj.magic ()) in
  let max_buckets =
    match max_buckets with
    | None -> Int.max min_buckets max_buckets_default
    | Some c -> Int.max min_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  and hash, equal =
    match hashed_type with
    | None -> (Stdlib.Hashtbl.seeded_hash (Random.bits ()), ( = ))
    | Some hashed_type -> HashedType.unpack hashed_type
  and pending = Nothing
  and buckets = Loc.make_array min_buckets Assoc.Nil
  and length = Accumulator.make ?n_way 0 in
  Loc.set t { pending; length; buckets; hash; equal; min_buckets; max_buckets };
  t

let n_way_of t = Accumulator.n_way_of (Loc.get t).length
let min_buckets_of t = (Loc.get t).min_buckets
let max_buckets_of t = (Loc.get t).max_buckets

let hashed_type_of t =
  let r = Loc.get t in
  HashedType.pack r.hash r.equal

let bucket_of hash key buckets =
  Array.unsafe_get buckets (hash key land (Array.length buckets - 1))

exception Done

module Xt = struct
  let find_opt ~xt t k =
    let r = Xt.get ~xt t in
    r.buckets |> bucket_of r.hash k |> Xt.get ~xt |> Assoc.find_opt r.equal k

  let find_all ~xt t k =
    let r = Xt.get ~xt t in
    r.buckets |> bucket_of r.hash k |> Xt.get ~xt |> Assoc.find_all r.equal k

  let mem ~xt t k =
    let r = Xt.get ~xt t in
    r.buckets |> bucket_of r.hash k |> Xt.get ~xt |> Assoc.mem r.equal k

  let get_or_alloc array_loc make length =
    let tx ~xt =
      let array = Xt.get ~xt array_loc in
      if array != [||] then array
      else
        let array = make length Assoc.Nil in
        Xt.set ~xt array_loc array;
        array
    in
    Xt.commit { tx }

  (** Pending operations are performed incrementally in small batches. *)
  let batch_size = 3

  let perform_pending ~xt t =
    (* TODO: Implement pending operations such that multiple domains may be
       working to complete them in parallel by extending the [state] to an array
       of multiple partition [states]. *)
    let must_be_done_in_this_tx = Xt.is_in_log ~xt t in
    let r = Xt.get ~xt t in
    match r.pending with
    | Nothing -> r
    | Rehash { state; new_capacity; new_buckets } -> begin
        let new_buckets =
          get_or_alloc new_buckets Loc.make_array new_capacity
        in
        let old_buckets = r.buckets in
        let r = { r with pending = Nothing; buckets = new_buckets } in
        Xt.set ~xt t r;
        let hash = r.hash and mask = new_capacity - 1 in
        let rehash_a_few_buckets ~xt =
          (* We process buckets in descending order as that is slightly faster
             with the transaction log.  It also makes sure that we know when the
             operation has already been performed independently of the
             buckets array we read above. *)
          let i = Xt.fetch_and_add ~xt state (-batch_size) in
          if i <= 0 then raise_notrace Done;
          for i = i - 1 downto Bits.max_0 (i - batch_size) do
            Array.unsafe_get old_buckets i
            |> Xt.get ~xt
            |> Assoc.iter_rev @@ fun k v ->
               Xt.unsafe_modify ~xt
                 (Array.unsafe_get new_buckets (hash k land mask))
                 (Assoc.cons k v)
          done
        in
        try
          if must_be_done_in_this_tx then
            (* If the old buckets have already been accessed, we cannot perform
               rehashing outside of the transaction.  In this case rehashing
               becomes linearithmic, O(n*log(n)), because that is the best that
               the transaction log promises.  However, as we access the bucket
               locations mostly in order, we often actually get linear time,
               O(n), performance. *)
            let initial_state = Array.length old_buckets in
            while true do
              (* If state is modified outside our expensive tx would fail. *)
              if Loc.fenceless_get state != initial_state then Retry.invalid ();
              rehash_a_few_buckets ~xt
            done
          else
            (* When possible, rehashing is performed cooperatively a few buckets
               at a time.  This gives expected linear time, O(n). *)
            while true do
              Xt.commit { tx = rehash_a_few_buckets }
            done;
          r
        with Done -> r
      end
    | Snapshot { state; snapshot } -> begin
        assert (not must_be_done_in_this_tx);
        let buckets = r.buckets in
        let r = { r with pending = Nothing } in
        Xt.set ~xt t r;
        (* Check state to ensure that buckets have not been updated. *)
        if Loc.fenceless_get state < 0 then Retry.invalid ();
        let snapshot =
          get_or_alloc snapshot Array.make (Array.length buckets)
        in
        let snapshot_a_few_buckets ~xt =
          let i = Xt.fetch_and_add ~xt state (-batch_size) in
          if i <= 0 then raise_notrace Done;
          for i = i - 1 downto Bits.max_0 (i - batch_size) do
            Array.unsafe_get buckets i |> Xt.get ~xt
            |> Array.unsafe_set snapshot i
          done
        in
        try
          while true do
            Xt.commit { tx = snapshot_a_few_buckets }
          done;
          r
        with Done -> r
      end
    | Filter_map { state; fn; raised; new_buckets } -> begin
        assert (not must_be_done_in_this_tx);
        let old_buckets = r.buckets in
        (* Check state to ensure that buckets have not been updated. *)
        if Loc.fenceless_get state < 0 then Retry.invalid ();
        let new_capacity = Array.length old_buckets in
        let new_buckets =
          get_or_alloc new_buckets Loc.make_array new_capacity
        in
        let filter_map_a_few_buckets ~xt =
          let i = Xt.fetch_and_add ~xt state (-batch_size) in
          if i <= 0 then raise_notrace Done;
          let a_few_buckets_delta = ref 0 in
          for i = i - 1 downto Bits.max_0 (i - batch_size) do
            Xt.get ~xt (Array.unsafe_get old_buckets i)
            |> Assoc.filter_map fn a_few_buckets_delta
            |> Xt.set ~xt (Array.unsafe_get new_buckets i)
          done;
          !a_few_buckets_delta
        in
        let total_delta = ref 0 in
        try
          while true do
            total_delta :=
              !total_delta + Xt.commit { tx = filter_map_a_few_buckets }
          done;
          r
        with
        | Done ->
            Accumulator.Xt.add ~xt r.length !total_delta;
            let r = { r with pending = Nothing; buckets = new_buckets } in
            Xt.set ~xt t r;
            r
        | exn ->
            Loc.compare_and_set raised Done exn |> ignore;
            let r = { r with pending = Nothing } in
            Xt.set ~xt t r;
            r
      end

  let make_rehash old_capacity new_capacity =
    let state = Loc.make old_capacity and new_buckets = Loc.make [||] in
    Rehash { state; new_capacity; new_buckets }
  [@@inline]

  let reset ~xt t =
    let r = perform_pending ~xt t in
    Accumulator.Xt.set ~xt r.length 0;
    Xt.set ~xt t
      { r with pending = make_rehash 0 r.min_buckets; buckets = [||] }

  let clear ~xt t = reset ~xt t

  let remove ~xt t k =
    let r = perform_pending ~xt t in
    let buckets = r.buckets in
    let mask = Array.length buckets - 1 in
    let bucket = Array.unsafe_get buckets (r.hash k land mask) in
    let change = ref Assoc.Nop in
    Xt.unsafe_modify ~xt bucket (fun kvs ->
        let kvs' = Assoc.remove r.equal change k kvs in
        if !change != Assoc.Nop then kvs' else kvs);
    if !change == Assoc.Removed then begin
      Accumulator.Xt.decr ~xt r.length;
      if r.min_buckets <= mask && Random.bits () land mask = 0 then
        let capacity = mask + 1 in
        let length = Accumulator.Xt.get ~xt r.length in
        if length * 4 < capacity then
          Xt.set ~xt t
            { r with pending = make_rehash capacity (capacity asr 1) }
    end

  let add ~xt t k v =
    let r = perform_pending ~xt t in
    let buckets = r.buckets in
    let mask = Array.length buckets - 1 in
    let bucket = Array.unsafe_get buckets (r.hash k land mask) in
    Xt.unsafe_modify ~xt bucket (Assoc.cons k v);
    Accumulator.Xt.incr ~xt r.length;
    if mask + 1 < r.max_buckets && Random.bits () land mask = 0 then
      let capacity = mask + 1 in
      let length = Accumulator.Xt.get ~xt r.length in
      if capacity < length then
        Xt.set ~xt t { r with pending = make_rehash capacity (capacity * 2) }

  let replace ~xt t k v =
    let r = perform_pending ~xt t in
    let buckets = r.buckets in
    let mask = Array.length buckets - 1 in
    let bucket = Array.unsafe_get buckets (r.hash k land mask) in
    let change = ref Assoc.Nop in
    Xt.unsafe_modify ~xt bucket (fun kvs ->
        let kvs' = Assoc.replace r.equal change k v kvs in
        if !change != Assoc.Nop then kvs' else kvs);
    if !change == Assoc.Added then begin
      Accumulator.Xt.incr ~xt r.length;
      if mask + 1 < r.max_buckets && Random.bits () land mask = 0 then
        let capacity = mask + 1 in
        let length = Accumulator.Xt.get ~xt r.length in
        if capacity < length then
          Xt.set ~xt t { r with pending = make_rehash capacity (capacity * 2) }
    end

  let length ~xt t = Accumulator.Xt.get ~xt (Xt.get ~xt t).length
  let swap = Xt.swap
end

let find_opt t k =
  let t = Loc.get t in
  (* Fenceless is safe as we have a fence above. *)
  t.buckets |> bucket_of t.hash k |> Loc.fenceless_get
  |> Assoc.find_opt t.equal k

let find_all t k =
  let t = Loc.get t in
  (* Fenceless is safe as we have a fence above. *)
  t.buckets |> bucket_of t.hash k |> Loc.fenceless_get
  |> Assoc.find_all t.equal k

let find t k = match find_opt t k with None -> raise Not_found | Some v -> v

let mem t k =
  let t = Loc.get t in
  (* Fenceless is safe as we have a fence above. *)
  t.buckets |> bucket_of t.hash k |> Loc.fenceless_get |> Assoc.mem t.equal k

let clear t = Kcas.Xt.commit { tx = Xt.clear t }
let reset t = Kcas.Xt.commit { tx = Xt.reset t }
let remove t k = Kcas.Xt.commit { tx = Xt.remove t k }
let add t k v = Kcas.Xt.commit { tx = Xt.add t k v }
let replace t k v = Kcas.Xt.commit { tx = Xt.replace t k v }
let length t = Accumulator.get (Loc.get t).length
let swap t1 t2 = Kcas.Xt.commit { tx = Xt.swap t1 t2 }

let snapshot ?length ?record t =
  let state = Loc.make 0 and snapshot = Loc.make [||] in
  let pending = Snapshot { state; snapshot } in
  let tx ~xt =
    let r = Xt.perform_pending ~xt t in
    length
    |> Option.iter (fun length -> length := Accumulator.Xt.get ~xt r.length);
    record |> Option.iter (fun record -> record := r);
    Loc.set state (Array.length r.buckets);
    Kcas.Xt.set ~xt t { r with pending }
  in
  Kcas.Xt.commit { tx };
  Kcas.Xt.commit { tx = Xt.perform_pending t } |> ignore;
  (* Fenceless is safe as commit above has fences. *)
  Loc.fenceless_get snapshot

let to_seq t =
  let snapshot = snapshot t in
  let rec loop i kvs () =
    match kvs with
    | Assoc.Nil ->
        if i = Array.length snapshot then Seq.Nil
        else loop (i + 1) (Array.unsafe_get snapshot i) ()
    | Cons { k; v; kvs } -> Seq.Cons ((k, v), loop i kvs)
  in
  loop 0 Nil

let to_seq_keys t = to_seq t |> Seq.map fst
let to_seq_values t = to_seq t |> Seq.map snd

let of_seq ?hashed_type ?min_buckets ?max_buckets ?n_way xs =
  let t = create ?hashed_type ?min_buckets ?max_buckets ?n_way () in
  Seq.iter (fun (k, v) -> replace t k v) xs;
  t

let rebuild ?hashed_type ?min_buckets ?max_buckets ?n_way t =
  let record = ref (Obj.magic ()) and length = ref 0 in
  let snapshot = snapshot ~length ~record t in
  let r = !record in
  let min_buckets =
    match min_buckets with
    | None -> r.min_buckets
    | Some c -> Int.max lo_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  in
  let max_buckets =
    match max_buckets with
    | None -> Int.max min_buckets r.max_buckets
    | Some c -> Int.max min_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  and n_way =
    match n_way with None -> Accumulator.n_way_of r.length | Some n -> n
  in
  let is_same_hashed_type =
    match hashed_type with
    | None -> true
    | Some hashed_type -> HashedType.is_same_as r.hash r.equal hashed_type
  and length = !length in
  if is_same_hashed_type && min_buckets <= length && length <= max_buckets then begin
    let t = Loc.make (Obj.magic ()) in
    let pending = Nothing
    and buckets = Array.map Loc.make snapshot
    and length = Accumulator.make ~n_way length in
    Loc.set t { r with pending; length; buckets; min_buckets; max_buckets };
    t
  end
  else
    let t = create ?hashed_type ~min_buckets ~max_buckets ~n_way () in
    snapshot |> Array.iter (Assoc.iter_rev (add t));
    t

let copy t = rebuild t
let fold fn t a = Array.fold_left (Assoc.fold fn) a (snapshot t)
let iter f t = fold (fun k v () -> f k v) t ()

let filter_map_inplace fn t =
  let state = Loc.make 0
  and raised = Loc.make Done
  and new_buckets = Loc.make [||] in
  let pending = Filter_map { state; fn; raised; new_buckets } in
  let tx ~xt =
    let r = Xt.perform_pending ~xt t in
    Loc.set state (Array.length r.buckets);
    Kcas.Xt.set ~xt t { r with pending }
  in
  Kcas.Xt.commit { tx };
  Kcas.Xt.commit { tx = Xt.perform_pending t } |> ignore;
  (* Fenceless is safe as commit above has fences. *)
  match Loc.fenceless_get raised with Done -> () | exn -> raise exn

let stats t =
  let length = ref 0 in
  let snapshot = snapshot ~length t in
  let num_bindings = !length in
  let num_buckets = Array.length snapshot in
  let bucket_lengths = Array.map Assoc.length snapshot in
  let max_bucket_length = Array.fold_left Int.max 0 bucket_lengths in
  let bucket_histogram = Array.make (max_bucket_length + 1) 0 in
  bucket_lengths
  |> Array.iter (fun i -> bucket_histogram.(i) <- 1 + bucket_histogram.(i));
  Stdlib.Hashtbl.
    { num_bindings; num_buckets; max_bucket_length; bucket_histogram }
