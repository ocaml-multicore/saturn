(* Copyright (c) 2023 Vesa Karvonen

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

(** ⚠️ Beware that this implementation uses a bunch of low level data
    representation tricks to minimize overheads. *)

module Atomic = Multicore_magic.Transparent_atomic

let max_value = Int.max_int

module Snapshot = struct
  type t = int Atomic.t array
  (** We use an optimized flat representation where the first element of the
      array holds the status of the snapshot.

        +--------+---------+---------+---------+- - -
        | status | counter | counter | counter | ...
        +--------+---------+---------+---------+- - -

      The status is either {!collecting}, {!computing}, or a non-negative value.

      The counter snapshot values are initialized to a negative value and after
      collecting they will all be non-negative. *)

  let zero = [| Atomic.make 0 |]
  let collecting = -1
  let computing = -2

  let[@inline] is_collecting (s : t) =
    Atomic.get (Array.unsafe_get s 0) = collecting

  let create n = Array.init n @@ fun _ -> Atomic.make collecting

  let[@inline] set s i after =
    let snap = Array.unsafe_get s i in
    let after = after land max_value in
    let before = Atomic.get snap in
    if
      before = collecting
      || (* NOTE: The condition below accounts for overflow. *)
      (after - before - 1) land max_value < max_value / 2
    then Atomic.compare_and_set snap before after |> ignore

  let[@inline] forward s i after =
    let snap = Array.unsafe_get s i in
    let after = after land max_value in
    while
      let before = Atomic.get snap in
      (before = collecting
      || (* NOTE: The condition below accounts for overflow. *)
      (after - before - 1) land max_value < max_value / 2)
      && not (Atomic.compare_and_set snap before after)
    do
      ()
    done

  let rec compute s sum i =
    if 0 < i then
      (* NOTE: Operations below are in specific order for performance. *)
      let decr = Array.unsafe_get s i in
      let incr = Array.unsafe_get s (i + 1) in
      let decr = Atomic.get decr in
      let incr = Atomic.get incr in
      compute s (sum - decr + incr) (i - 2)
    else sum land max_value

  let compute s = compute s 0 (Array.length s - 2)

  let compute s =
    let status = Array.unsafe_get s 0 in
    if Atomic.get status = collecting then
      Atomic.compare_and_set status collecting computing |> ignore;
    if Atomic.get status = computing then begin
      let computed = compute s in
      if Atomic.get status = computing then
        Atomic.compare_and_set status computing computed |> ignore
    end;
    Atomic.get status
end

type _ state =
  | Open : { mutable index : int } -> [ `Open ] state
  | Used : [ `Used ] state

let used_index = 0

type tx = { value : int; once : [ `Open ] state }
type t = tx Atomic.t array Atomic.t

(** We use an optimized flat representation where the first element of the array
    holds a reference to the snapshot and the other elements are the counters.

      +----------+------+------+------+------+- - -
      | snapshot | decr | incr | decr | incr | ...
      +----------+------+------+------+------+- - -

    Counters at odd numbered indices are for [decr]ements and the counters at
    even numbered indices are for [incr]ements.

    A counter refers to a unique [tx] record. *)

let[@inline] snapshot_of txs : Snapshot.t Atomic.t =
  Obj.magic (Array.unsafe_get txs 0)

(* *)

let zero = { value = 0; once = Open { index = used_index } }

let create () =
  Array.init
    ((1 * 2) + 1)
    (fun i ->
      Atomic.make (if i = 0 then Obj.magic Snapshot.zero else zero)
      |> Multicore_magic.copy_as_padded)
  |> Atomic.make |> Multicore_magic.copy_as_padded

(* *)

type once = Once : _ state -> once [@@unboxed]

let get_index (Open r) = r.index
let use_index (Open r) = r.index <- used_index

(* *)

let used_once = Once Used

(* *)

type update = int

let decr = 1
let incr = 2

let rec new_once t update =
  let index = (Multicore_magic.instantaneous_domain_index () * 2) + update in
  let txs = Atomic.fenceless_get t in
  let n = Array.length txs in
  if index < n then Once (Open { index })
  else
    let txs_new =
      (* The length of [txs_new] will be a power of two minus 1, which means the
         whole heap block will have a power of two number of words, which may
         help to keep it cache line aligned. *)
      Array.init ((n * 2) + 1) @@ fun i ->
      if i = 0 then
        Obj.magic (Multicore_magic.copy_as_padded @@ Atomic.make Snapshot.zero)
      else if i < n then Array.unsafe_get txs i
      else Multicore_magic.copy_as_padded (Atomic.make zero)
    in
    Atomic.compare_and_set t txs txs_new |> ignore;
    new_once t update

let new_once t update =
  let index = (Multicore_magic.instantaneous_domain_index () * 2) + update in
  let txs = Atomic.fenceless_get t in
  if index < Array.length txs then Once (Open { index }) else new_once t update

(* *)

let rec update_once txs once counter =
  let before = Atomic.get counter in
  let index = get_index once in
  if index != used_index && before.once != once then begin
    use_index before.once;
    let after = { value = before.value + 1; once } in
    if Atomic.compare_and_set counter before after then begin
      let snapshot = Atomic.get (snapshot_of txs) in
      if Snapshot.is_collecting snapshot then
        Snapshot.forward snapshot index after.value
    end
    else update_once txs once (Array.unsafe_get txs index)
  end

let update_once t = function
  | Once Used -> ()
  | Once (Open _ as once) ->
      let index = get_index once in
      if index != used_index then
        let txs = Atomic.fenceless_get t in
        update_once txs once (Array.unsafe_get txs index)

(* *)

let get_collecting_snapshot txs =
  let snapshot = snapshot_of txs in
  let before = Atomic.get snapshot in
  if Snapshot.is_collecting before then before
  else
    let after = Snapshot.create (Array.length txs) in
    if Atomic.compare_and_set snapshot before after then after
    else Atomic.get snapshot

let rec collect txs snapshot i =
  if 0 < i then begin
    let after = Atomic.get (Array.unsafe_get txs i) in
    Snapshot.set snapshot i after.value;
    collect txs snapshot (i - 1)
  end

let rec get t =
  let txs = Atomic.fenceless_get t in
  let snapshot = get_collecting_snapshot txs in
  collect txs snapshot (Array.length txs - 1);
  let size = Snapshot.compute snapshot in
  if Atomic.fenceless_get t == txs then size else get t
