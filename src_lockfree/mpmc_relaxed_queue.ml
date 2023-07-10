(*
  # General idea

  It is the easiest to explain the general idea on an array of infinite size.
  Let's start with that. Each element in such an array constitutes a single-use
  exchange slot. Enqueuer increments [tail] and treats prior value as index of
  its slot. Same for dequeuer and [head]. This effectively creates pairs
  (enqueuer, dequeuer) assigned to the same slot. Enqueuer leaves the value in
  the slot, dequer copies it out.

  Enqueuer never fails. It always gets a brand-new slot and places item in it.
  Dequeuer, on the other hand, may witness an empty slot. That's because [head]
  may jump behind [tail]. Remember, indices are implemented blindy. For now,
  assume dequeuer simply spins on the empty slot until an item appears.

  That's it. There's a few things flowing from this construction:
  * Slots are atomic. This is where paired enqueuer and dequeuer communicate.
  * [head] overshooting [tail] is a normal condition and that's good - we want
  to keep operations on [head] and [tail] independent.

  # Finite array

  Now, to make it work in real-world, simply treat finite array as circular,
  i.e. wrap around when reached the end. Slots are now re-used, so we need to be
  more careful.

  Firstly, if there's too many items, enqueuer may witness a full slot. Let's assume
  enqueuer simply spins on full slot until some dequeuer appears and takes the old
  value.

  Secondly, in the case of overlap, there can be more than 2 threads (1x enqueuer,
  1x dequeuer) assigned to a single slot (imagine 10 enqueuers spinning on an 8-slot
  array). In fact, it could be any number. Thus, all operations on slot have to use
  CAS to ensure that no item is overwrriten on store and no item is dequeued by two
  threads at once.

  Above works okay in practise, and there is some relevant literature, e.g.
  (DOI: 10.1145/3437801.3441583) analyzed this particular design. There's also
  plenty older papers looking at similar approaches
  (e.g. DOI: 10.1145/2851141.2851168).

  Note, this design may violate FIFO (on overlap). The risk can be minimized by
  ensuring size of array >> number of threads but it's never zero.
  (github.com/rigtorp/MPMCQueue has a nice way of fixing this, we could add it).

  # Blocking (non-lockfree paths on full, empty)

  Up until now [push] and [pop] were allowed to block indefinitely on empty and full
  queue. Overall, what can be done in those states?

  1. Busy wait until able to finish.
  2. Rollback own index with CAS (unassign itself from slot).
  3. Move forward other index with CAS (assign itself to the same slot as opposite
  action).
  4. Mark slot as burned - dequeue only.

  Which one then?

  Let's optimize for stability, i.e. some reasonable latency that won't get much worse
  under heavy load. Busy wait is great because it does not cause any contention in the
  hotspots ([head], [tail]). Thus, start with busy wait (1). If queue is busy and
  moving fast, there is a fair chance that within, say, 30 spins, we'll manage to
  complete action without having to add contention elsewhere.

  Once N busy-loops happen and nothing changes, we probably want to return even if its
  costs. (2), (3) both allow that. (2) doesn't add contention to the other index like
  (3) does. Say, there's a lot more dequeuers than enqueuers, if all dequeurs did (3),
  they would add a fair amount of contention to the [tail] index and slow the
  already-outnumbered enqueuers further. So, (2) > (3) for that reason.

  However, with just (2), some dequeuers will struggle to return. If many dequeuers
  constatly try to pop an element and fail, they will form a chain.

   tl                 hd
   |                  |
  [.]-[A]-[B]-[C]-..-[X]

  For A to rollback, B has to rollback first. For B to rollback C has to rollback first.

  [A] is likely to experience a large latency spike. In such a case, it is easier for [A]
  to do (3) rather than hope all other active dequeuers will unblock it at some point.
  Thus, it's worthwile also trying to do (3) periodically.

  Thus, the current policy does (1) for a bit, then (1), (2) with periodic (3).

  What about burned slots (4)?

  It's present in the literature. Weakly I'm not a fan. If dequeuers are faster to remove
  items than enqueuers supply them, slots burned by dequeuers are going to make enqueuers
  do even more work.

  # Resizing

  The queue does not support resizing, but it can be simulated by wrapping it in a
  lockfree list.
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

(* [ccas] A slightly nicer CAS. Tries without taking microarch lock first. Use on indices. *)
let ccas cell seen v =
  if Atomic.get cell != seen then false else Atomic.compare_and_set cell seen v

let push { array; tail; mask; _ } item =
  let tail_val = Atomic.fetch_and_add tail 1 in
  let index = tail_val land mask in
  let cell = Array.get array index in
  while not (ccas cell None (Some item)) do
    Domain.cpu_relax ()
  done

let pop { array; head; mask; _ } =
  let head_val = Atomic.fetch_and_add head 1 in
  let index = head_val land mask in
  let cell = Array.get array index in
  let item = ref (Atomic.get cell) in
  while Option.is_none !item || not (ccas cell !item None) do
    Domain.cpu_relax ();
    item := Atomic.get cell
  done;
  Option.get !item
