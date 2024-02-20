include Saturn_lockfree.Relaxed_queue

module Spin = struct
  let push = push
  let pop = pop
end

(* [ccas] A slightly nicer CAS. Tries without taking microarch lock first. Use on indices. *)
let ccas cell seen v =
  if Atomic.get cell != seen then false else Atomic.compare_and_set cell seen v

module Not_lockfree = struct
  (* [spin_threshold] Number of times on spin on a slot before trying an exit strategy. *)
  let spin_threshold = 30

  (* [try_other_exit_every_n] There is two strategies that push/pop can take to fix state (
     to be able to return without completion). Generally, we want to try to do "rollback" more
     than "push forward", as the latter adds contention to the side that might already not
     be keeping up. *)
  let try_other_exit_every_n = 10
  let time_to_try_push_forward n = n mod try_other_exit_every_n == 0

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
      else begin
        Domain.cpu_relax ();
        (* retry *)
        take_or_rollback (nth_attempt + 1)
      end
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
        else if ccas head (old_head + 1) old_head then
          (* rolled back head *)
          None
        else if
          time_to_try_push_forward nth_attempt
          && ccas tail old_head (old_head + 1)
        then (* pushed tail forward *)
          None
        else begin
          Domain.cpu_relax ();
          take_or_rollback (nth_attempt + 1)
        end
      in

      (* return if got item, clean up otherwise *)
      if Option.is_some !item then !item else take_or_rollback 0

  module CAS_interface = struct
    let rec push ({ array; tail; head; mask; _ } as t) item =
      let tail_val = Atomic.get tail in
      let head_val = Atomic.get head in
      let size = mask + 1 in
      if tail_val - head_val >= size then false
      else if ccas tail tail_val (tail_val + 1) then begin
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
          Domain.cpu_relax ()
        done;
        true
      end
      else push t item

    let rec pop ({ array; tail; head; mask; _ } as t) =
      let tail_val = Atomic.get tail in
      let head_val = Atomic.get head in
      if head_val - tail_val >= 0 then None
      else if ccas head head_val (head_val + 1) then begin
        let index = head_val land mask in
        let cell = Array.get array index in
        let item = ref (Atomic.get cell) in
        while
          not (Option.is_some !item && Atomic.compare_and_set cell !item None)
        do
          Domain.cpu_relax ();
          item := Atomic.get cell
        done;
        !item
      end
      else pop t
  end
end
