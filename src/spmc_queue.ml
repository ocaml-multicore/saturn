(* Notes:
   - Local deque does not have to synchronize with the writer, only other readers.
   - Tail index marks the "logical tail", physical one is the cell with oldest Value.
   - Local dequeue and enqueue are quite close to wait freedom. Enqueuer can simply
   check whether the slot it will get is free before incrementing tail, dequeuer can
   just roll back after a few spins. I don't think it makes sense to implement that
   in the general case though, as we don't actually want to return in those cases.
   - Some accesses do not have to be atomic but Atomic.t does not have unsafe
   methods. Probably doesn't matter on x86.
*)

type 'a t = {
  head : int Atomic.t;
  tail : int Atomic.t;
  mask : int Atomic.t;
  array : 'a option Atomic.t Array.t Atomic.t;
}

let create ~size_exponent () =
  let size = Int.shift_left 1 size_exponent in
  {
    head = Atomic.make 0;
    tail = Atomic.make 0;
    mask = Atomic.make (size - 1);
    array = Atomic.make (Array.init size (fun _ -> Atomic.make None));
  }

let indicative_size { head; tail; _ } =
  max (Atomic.get tail - Atomic.get head) 0

module Local = struct
  (* [local_resize] resizes the array.

     1. Create new array.
     2. Overshoot the head to discourage any new stealers.
     3. Transfer covered elements to the new array.
     4. Ensure all other stealers finished (i.e. no elements in the old array).
     5. Swap arrays.
     6. Update mask, set tail and head to 0. In this order to discourage stealers.
  *)
  let resize t =
    let { mask; array; tail; head; _ } = t in
    let mask_val = Atomic.get mask in
    let array_val = Atomic.get array in
    let size = Atomic.get mask + 1 in
    let new_size = size * 2 in
    let new_mask = new_size - 1 in
    let new_array_val = Array.init new_size (fun _ -> Atomic.make None) in
    let old_head_val = Atomic.fetch_and_add head size in
    let temp_head_val = old_head_val + size in
    let tail_val = Atomic.get tail in
    let num_of_items_to_copy = tail_val - old_head_val in
    for i = 0 to num_of_items_to_copy - 1 do
      let index_in_old_array = (old_head_val + i) land mask_val in
      let old_cell = Array.get array_val index_in_old_array in
      let value = Atomic.get old_cell in
      (* Gc things, probably not necessary since old array should not live
         for long. *)
      Atomic.set old_cell None;
      assert (Option.is_some value);
      (* Why not start at index 0 in the new array? Easier than changing tail *)
      let index_in_new_array = (old_head_val + i) land new_mask in
      let new_cell = Array.get new_array_val index_in_new_array in
      Atomic.set new_cell value
    done;

    (* just for extra safety, wait until stealers finish - should not
       be necessary for correctness *)
    let num_of_items_to_ensure_taken = temp_head_val - tail_val in
    for i = 0 to num_of_items_to_ensure_taken - 1 do
      let index = (tail_val + i) land mask_val in
      while Option.is_some (Atomic.get (Array.get array_val index)) do
        ()
      done
    done;
    Atomic.set array new_array_val;
    Atomic.set mask new_mask;
    Atomic.set head old_head_val

  let push { tail; head; mask; array; _ } element =
    let mask, array = Atomic.(get mask, get array) in
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    let size = mask + 1 in
    let current_size = tail_val - head_val in
    assert (current_size <= size);
    if current_size = size then false
    else
      let index = tail_val land mask in
      let cell = Array.get array index in
      while Option.is_some (Atomic.get cell) do
        ()
      done;
      Atomic.set cell (Some element);
      Atomic.set tail (tail_val + 1);
      true

  let rec push_with_autoresize ({ tail; mask; array; _ } as t) element =
    let mask = Atomic.get mask in
    let array = Atomic.get array in
    let tail_val = Atomic.get tail in
    let cell = Array.get array (tail_val land mask) in
    match Atomic.get cell with
    | None ->
        Atomic.set cell (Some element);
        (* tail might have been changed by resize *)
        Atomic.set tail (Atomic.get tail + 1)
    | Some _ ->
        (* I suppose we should be getting increasingly hesitant to increase
           as the buffer is already big. *)
        resize t;
        push_with_autoresize t element

  let pop { head; tail; mask; array; _ } : 'a option =
    let mask = Atomic.get mask in
    let array = Atomic.get array in
    (* dequeue is optimistic because it can use the fact that
       there is no enqueuer *)
    let old_head_val = Atomic.fetch_and_add head 1 in
    let tail_val = Atomic.get tail in
    assert (old_head_val - tail_val <= 0);
    if old_head_val = tail_val then (
      (* nobody else would speculate *)
      (* CAS here is not necessary but leaving it it for assertion.
         Can be removed once we're confident. *)
      assert (Atomic.compare_and_set head (old_head_val + 1) old_head_val);
      (* Atomic.set head index; *)
      None)
    else
      let index = old_head_val land mask in
      let cell = Array.get array index in
      let item = ref (Atomic.get cell) in
      while
        not (Option.is_some !item && Atomic.compare_and_set cell !item None)
      do
        item := Atomic.get cell
      done;
      !item

  (* successfuly rolled back *)
  let is_empty_thorough { head = _; tail; mask; array; _ } : bool =
    let mask = Atomic.get mask in
    let array = Atomic.get array in
    let size = Array.length array in
    let tail_value = Atomic.get tail in
    let seen_not_free = ref false in
    let i = ref (size - 1) in
    while (not !seen_not_free) && !i >= 0 do
      let cell = Array.get array ((tail_value + !i) land mask) in
      seen_not_free := Atomic.get cell != None;
      i := !i - 1
    done;
    not !seen_not_free

  let is_empty queue =
    let { head; tail; _ } = queue in
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    if tail_val - head_val != 0 then false else is_empty_thorough queue

  let steal ~from ({ mask = local_mask; _ } as local) =
    (* need to be careful with from queue, which is not local *)
    let local_size = Atomic.get local_mask + 1 in
    let ({
           head = from_head;
           tail = from_tail;
           mask = from_mask;
           array = from_array;
           _;
         }
          : 'a t) =
      from
    in
    let from_mask_val = Atomic.get from_mask in
    let from_array = Atomic.get from_array in
    (* assumes there's space in the queue *)
    let from_tail_val = Atomic.get from_tail in
    let from_head_val = Atomic.get from_head in
    let from_size = from_tail_val - from_head_val in
    if from_size < 1 then 0
    else
      let stealable =
        (* steal even if there's a single element, thus *+1* *)
        min ((from_size + 1) / 2) local_size
      in
      let new_from_head_val = from_head_val + stealable in
      let acquired_items =
        Atomic.compare_and_set from_head from_head_val new_from_head_val
      in
      assert (from_tail_val - new_from_head_val >= 0);
      if not acquired_items then 0
      else (
        for i = 0 to stealable - 1 do
          let from_index = (from_head_val + i) land from_mask_val in
          let cell = Array.get from_array from_index in
          let value = ref (Atomic.get cell) in
          while
            not
              (Option.is_some !value && Atomic.compare_and_set cell !value None)
          do
            value := Atomic.get cell
          done;
          let value_exn = function None -> assert false | Some v -> v in
          while not (push local (value_exn !value)) do
            ()
          done
        done;
        stealable)
end

module M = struct
  (*
     Fitted into the module type expected by Domainslib.
     Note stealing now only takes 1 element.
  *)
  type nonrec 'a t = 'a t

  let create () = create ~size_exponent:5 ()
  let push = Local.push_with_autoresize
  let pop t = match Local.pop t with Some v -> v | None -> raise Exit

  let steal { head; tail; mask; array } =
    (* need to be careful with from queue, which is not local *)
    let mask_val = Atomic.get mask in
    let array = Atomic.get array in
    (* assumes there's space in the queue *)
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    let size = tail_val - head_val in
    if size < 1 then raise Exit
    else
      let new_head_val = head_val + 1 in
      let acquired_item = Atomic.compare_and_set head head_val new_head_val in
      if not acquired_item then raise Exit
      else
        let index = head_val land mask_val in
        let cell = Array.get array index in
        let value = ref (Atomic.get cell) in
        while
          not (Option.is_some !value && Atomic.compare_and_set cell !value None)
        do
          value := Atomic.get cell
        done;
        match !value with None -> assert false | Some v -> v
end
