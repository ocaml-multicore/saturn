(*
   The general idea behind FAD queues is the easiest to first understand on an
   array of infinite size. In such a case each slot is a single-use pigeonhole.
   Incrementing [tail] assigns a particular slot to an pushr, and incrementing
   [head] assigns a particular slot to dequeuer.

   pushr never fails, as it always gets a brand-new slot. Dequeuer, on the other
   hand, can witness an empty slot, if [head] jumps behind [tail]. What can
   dequeuer do? It needs to check whether its slot has anything in it. If it does,
   it simply returns the value. Otherwise, let's keep spinning until an element
   appears.

   Thus each slot has to be atomic - it's the place where paired pushr and
   dequeuer synchronize. That's an extra atomic operation when comparing to e.g. CAS
   queue, which runs no risk of [head] overtaking [tail]. But CAS queue experiences
   a lot of contention on [head], [tail] and ends up with worse throughput even with
   just a few threads.

   The principle is similar on a bounded array but now the number of threads
   accessing a single cell is not capped at 2. If queue wraps around multiple
   times, there could be N pushrs and N dequeuers on a single slot. Still, as long
   as long as pushrs retry and all operations are properly synchronized, it
   linearizes. This is the queue described in the paper above.

   --

   It may happen that requested action cannot be completed. Queue could be full (preventing
   enqueue) or empty (preventing dequeue). We're only gonna learn this after incrementing
   respective index. Push will see that assigned slot still has an item. Pop will see
   assigned slot with nothing in it. There's a bunch of things that can be done:
   1. Busy wait until able to finish.
   2. Rollback its index with CAS (unassign itself from slot).
   3. Move forward other index with CAS (assign itself to the same slot as opposite action).
   4. Mark slot as burned - dequeue only.

   Which one then?

   Let's optimize for stability, i.e. some reasonable latency that won't get much worse
   under heavy load. Busy wait is great because it does not cause any contention in the
   hotspots ([head], [tail]). Let's start with busy wait (1) for say 30 iterations - that
   will handle the case when queue is heavily loaded. Once N busy-loops happen and nothing
   changes, we probably just want to return.

   (2), (3) both allow that. (2) is a little nicer than (3), in that it doesn't add
   contention to the other index like (3) does. Say, there's a lot more dequeuers than
   enqueuers, if all dequeurs did (3), they would add a fair amount of contention to
   the [tail] index and slow the already-outnumbered enqueuers further. So, (2) > (3) for
   that reason.

   However, in the previous scenario, some dequeuers will really struggle to return. If many
   dequeuers are constatly trying to pop an element and fail, they will form a chain.

    tl                 hd
    |                  |
   [.]-[A]-[B]-[C]-..-[X]

   For A to rollback, B has to rollback first. For B to dequeue C has to rollback first.

   [A] is likely to experience a large latency spike. In such a case, it is easier for [A]
   to do (3) than hope all other active dequeuers will unblock [A] at some point. Thus, it's
   worthwile also trying to do (3) periodically.

   It's good to always assign non-zero probability to each of the options. An item for A
   may appear at any time. Neighbour succeeding with other strategy may render ours
   impossible (e.g. if dequeuer [B] moves forward [tail] then [A] will never succed in
   rolling [head] back).

   What about burned slots (4)?

   It's present in the literature. Weakly I'm not a fan. In the case where dequeuers
   are faster to remove items than enqueuers supply them, slots burned by dequeuers
   are going to make enqueuers do even more work.

   --

   The queue can be made auto-resizable by user using a lockfree list.
*)

type 'a t = {
  array : 'a Option.t Atomic.t Array.t;
  head : int Atomic.t;
  tail : int Atomic.t;
  mask : int;
}

let create ~size_exponent () : 'a t =
  let size = 1 lsl size_exponent in
  let array = Array.init size (fun _ -> Atomic.make None) in
  let mask = size - 1 in
  let head = Atomic.make 0 in
  let tail = Atomic.make 0 in
  { array; head; tail; mask }

let spin_threshold = 30

let ccas cell seen v =
  if Atomic.get cell != seen then false else Atomic.compare_and_set cell seen v

let push { array; tail; head; mask; _ } item =
  let tail_val = Atomic.fetch_and_add tail 1 in
  let index = tail_val land mask in
  let cell = Array.get array index in
  (* spin for a bit *)
  let i = ref 0 in
  while !i < spin_threshold && not (ccas cell None (Some item)) do
    i := !i + 1
  done;
  (* define clean up function *)
  let rec take_or_rollback () =
    if ccas cell None (Some item) then (* succedded to push *)
      true
    else if ccas tail (tail_val + 1) tail_val then (* rolled back tail *)
      false
    else if ccas head tail_val (tail_val + 1) then
      (* pushed forward head *)
      false
    else (* retry *)
      take_or_rollback ()
  in
  (* if succeeded return true otherwise clean up *)
  if !i < spin_threshold then true else take_or_rollback ()

let pop queue =
  let ({ array; head; tail; mask; _ } : 'a t) = queue in
  let head_value = Atomic.get head in
  let tail_value = Atomic.get tail in
  if head_value >= tail_value then None
  else
    let old_head = Atomic.fetch_and_add head 1 in
    let cell = Array.get array (old_head land mask) in
    let rec take_or_rollback () =
      let value = Atomic.get cell in
      if Option.is_some value && Atomic.compare_and_set cell value None then
        (* dequeued an item, return it *)
        value
      else if
        Atomic.get tail <= old_head
        && Atomic.compare_and_set head (old_head + 1) old_head
      then (* rolled back head *)
        None
      else take_or_rollback ()
    in
    take_or_rollback ()
