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
  a little bit more careful, but just a little. 
  
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

  # Non-blocking operations

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

(* [spin_threshold] Number of times on spin on a slot before trying an exit strategy. *)
let spin_threshold = 30

(* [try_other_exit_every_n] There is two strategies that push/pop can take to fix state (
   to be able to return without completion). Generally, we want to try to do "rollback" more
   than "push forward", as the latter adds contention to the side that might already not
   be keeping up. *)
let try_other_exit_every_n = 10
let time_to_try_push_forward n = n mod try_other_exit_every_n == 0

(* [ccas] A slightly nicer CAS. Tries without taking microarch lock first. Use on indices. *)
let ccas cell seen v =
  if Atomic.get cell != seen then false else Atomic.compare_and_set cell seen v

let push { array; tail; head; mask; _ } item =
  let tail_val = Atomic.fetch_and_add tail 1 in
  let index = tail_val land mask in
  let cell = Array.get array index in

  (* spin for a bit *)
  let i = ref 0 in
  while
    !i < spin_threshold && not (Atomic.compare_and_set cell None (Some item))
  do
    i := !i + 1
  done;

  (* define clean up function *)
  let rec take_or_rollback nth_attempt =
    if Atomic.compare_and_set cell None (Some item) then
      (* succedded to push *)
      true
    else if ccas tail (tail_val + 1) tail_val then (* rolled back tail *)
      false
    else if
      time_to_try_push_forward nth_attempt && ccas head tail_val (tail_val + 1)
    then (* pushed forward head *)
      false
    else (* retry *)
      take_or_rollback (nth_attempt + 1)
  in

  (* if succeeded return true otherwise clean up *)
  if !i < spin_threshold then true else take_or_rollback 0

let take_item cell =
  let value = Atomic.get cell in
  if Option.is_some value && Atomic.compare_and_set cell value None then value
  else None

let pop queue =
  let ({ array; head; tail; mask; _ } : 'a t) = queue in
  let head_value = Atomic.get head in
  let tail_value = Atomic.get tail in
  if head_value - tail_value >= 0 then None
  else
    let old_head = Atomic.fetch_and_add head 1 in
    let cell = Array.get array (old_head land mask) in

    (* spin for a bit *)
    let i = ref 0 in
    let item = ref None in
    while !i < spin_threshold && not (Option.is_some !item) do
      item := take_item cell;
      i := !i + 1
    done;

    (* define clean up function *)
    let rec take_or_rollback nth_attempt =
      let value = Atomic.get cell in
      if Option.is_some value && Atomic.compare_and_set cell value None then
        (* dequeued an item, return it *)
        value
      else if ccas head (old_head + 1) old_head then (* rolled back head *)
        None
      else if
        time_to_try_push_forward nth_attempt && ccas tail old_head (old_head + 1)
      then (* pushed tail forward *)
        None
      else take_or_rollback (nth_attempt + 1)
    in

    (* return if got item, clean up otherwise *)
    if Option.is_some !item then !item else take_or_rollback 0

module CAS_interface = struct
  let rec push ({ array; tail; head; mask; _ } as t) item =
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    let size = mask + 1 in
    if tail_val - head_val >= size then false
    else if ccas tail tail_val (tail_val + 1) then (
      let index = tail_val land mask in
      let cell = Array.get array index in
      (*
         Given that code above checks for overlap, is this CAS needed?

         Yes. Even though a thread cannot explicitely enter overlap,
         it can still occur just because enqueuer may theoretically be
         unscheduled for unbounded amount of time between incrementing
         index and filling the slot.

         I doubt we'd observe that case in real-life (outside some
         extreme circumstances), but this optimization has to be left
         for the user to decide. After all, algorithm would not pass
         model-checking without it.

         Incidentally, it also makes this method interoperable with
         standard interface.
      *)
      while not (Atomic.compare_and_set cell None (Some item)) do
        ()
      done;
      true)
    else push t item

  let rec pop ({ array; tail; head; mask; _ } as t) =
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    if head_val - tail_val >= 0 then None
    else if ccas head head_val (head_val + 1) then (
      let index = head_val land mask in
      let cell = Array.get array index in
      let item = ref (Atomic.get cell) in
      while
        not (Option.is_some !item && Atomic.compare_and_set cell !item None)
      do
        item := Atomic.get cell
      done;
      !item)
    else pop t
end

