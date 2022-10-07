(*
  The idea behind FAD queues is easier to explain on an array of infinite size. 
  each element in such array constitutes a single-use exchange slot. Enqueuer 
  increments [tail] and uses its previous value as the index of its slot. Same 
  for dequeuer and [head].   
   
  # Infinite array

  Enqueuer never fails. It always gets a brand-new slot and places item in it. 
  Dequeuer, on the other hand, may witness an empty slot, if [head] jumps behind 
  [tail]. Remember, we blindly increment respective index. Assume dequeuer 
  simply keeps spinning on checking the slot until an item appears. 

  That's it. There's a few things flowing from this construction: 
  * Slots are atomic. This is where paired enqueuer and dequeuer communicate. 
  * [head] overshooting [tail] is a normal condition and that's good - we want 
  to keep operations on [head] and [tail] independent.
  

  # Finite array

  Simply treat finite array as circular, i.e. wrap around when reached the end. 
  Slots are now re-used, so we need to be a little more careful, but just a little. 
  
  Firstly, if there's too many items enqueuer may witness a full slot. Let's assume 
  enqueuer keeps spinning until some dequeuer appears and creates space. 

  Secondly, in the case of overlap, there can be more than 2 threads assigned to a 
  single slot (e.g. 20 enqueuers spinning on 16-slot array). In fact, it could be 
  any number. Thus, all operations on slot have to be use CAS to ensure that no item 
  is overwrriten on store and no item is dequeued by two threads at once. 

  Above works fine in practises. For example (DOI: 10.1145/3437801.3441583) analyzed
  this particular design. But this kind of approach is a far bit older and also 
  present in other papers (e.g. DOI: 10.1145/2851141.2851168) and some GitHub repos. 

  Note, this design may violate FIFO (on overlap). The risk can be minimized by 
  ensuring size_of_array >> num_of_threads but it's never zero.   
  (github.com/rigtorp/MPMCQueue has a nice way of fixing this, we could add it).

  # Non-blocking operations

  Up until now [push] and [pop] were allowed to block on empty and full queue. 
  There's a few ways to fix that: 

  1. Busy wait until able to finish.
  2. Rollback its index with CAS (unassign itself from slot).
  3. Move forward other index with CAS (assign itself to the same slot as opposite 
  action).
  4. Mark slot as burned - dequeue only.

  Which one then?

  Let's optimize for stability, i.e. some reasonable latency that won't get much worse
  under heavy load. Busy wait is great because it does not cause any contention in the
  hotspots ([head], [tail]). Start with busy wait (1) for, say, 30 iterations - that
  will handle the case when queue is heavily loaded and moving forward fast. Once N 
  busy-loops happen and nothing changes, we probably just want to return.

  (2), (3) both allow that. (2) doesn't add contention to the other index like (3) 
  does. Say, there's a lot more dequeuers than enqueuers, if all dequeurs did (3), 
  they would add a fair amount of contention to the [tail] index and slow the 
  already-outnumbered enqueuers further. So, (2) > (3) for that reason.

  However, in the previous scenario, some dequeuers will struggle to return. If many
  dequeuers are constatly trying to pop an element and fail, they will form a chain.

   tl                 hd
   |                  |
  [.]-[A]-[B]-[C]-..-[X]

  For A to rollback, B has to rollback first. For B to rollback C has to rollback first.

  [A] is likely to experience a large latency spike. In such a case, it is easier for [A]
  to do (3) rather than hope all other active dequeuers will unblock it at some point. 
  Thus, it's worthwile also trying to do (3) periodically.

  It's good to always assign non-zero probability to each of the options. An item for A
  may appear at any time. Neighbour succeeding with other strategy may render ours
  impossible (e.g. if dequeuer [B] moves forward [tail] then [A] will never succed in
  rolling [head] back).

  What about burned slots (4)?

  It's present in the literature. Weakly I'm not a fan. In the case where dequeuers
  are faster to remove items than enqueuers supply them, slots burned by dequeuers
  are going to make enqueuers do even more work.

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
  if head_value >= tail_value then None
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
