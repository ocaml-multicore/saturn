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
  mask : int;
  size_exponent : int;
  array : 'a option Atomic.t Array.t;
  tail : int Atomic.t;
}

let create ~size_exponent () =
  let size = Int.shift_left 1 size_exponent in
  {
    head = Atomic.make 0;
    mask = size - 1;
    tail = Atomic.make 0;
    size_exponent;
    array = Array.init size (fun _ -> Atomic.make None);
  }

let indicative_size { head; tail; _ } =
  max (Atomic.get tail - Atomic.get head) 0

module Local = struct
  let push { tail; head; mask; array; _ } element =
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
      true;;
  
  let pop { head; tail; mask; array; _ } : 'a option =
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
    let local_size = local_mask + 1 in
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
          let from_index = (from_head_val + i) land from_mask in
          let cell = Array.get from_array from_index in
          let value = Atomic.get cell in
          assert (Atomic.compare_and_set cell value None);
          assert (Option.is_some value);
          let value_exn = function None -> assert false | Some v -> v in
          while not (push local (value_exn value)) do
            ()
          done
        done;
        stealable)

  let steal_one { head; tail; mask; array; _ } =
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
        let index = head_val land mask in
        let cell = Array.get array index in
        let value = Atomic.get cell in
        assert (Atomic.compare_and_set cell value None);
        assert (Option.is_some value);
        match value with None -> assert false | Some v -> v
end



module Resizable = struct 
  type nonrec 'a t = 'a t Atomic.t
  
  module Local = struct
    let resize (t : 'a t) : unit = 
      let t_val = Atomic.get t in 
      let ({size_exponent; _}) = Atomic.get t in 
      let new_t_val = create ~size_exponent:(size_exponent+1) () in 
      (* transfer all elements *)
      let finished = ref false in 
      while not !finished do 
        match Local.pop t_val with 
        | None -> finished := true 
        | Some item -> 
          assert (Local.push new_t_val item);
      done; 
      (* save the new queue *)
      Atomic.set t new_t_val
    ;;

      
    let rec push_with_autoresize (t : 'a t) (element : 'a) =
      let t_val = Atomic.get t in 
      if Local.push t_val element 
      then () 
      else 
        (resize t;
        push_with_autoresize t element);;
    
    let push t item = Local.push (Atomic.get t) item 
    
    let pop t = Local.pop (Atomic.get t) 

    let steal ~from t = Local.steal ~from:(Atomic.get from) (Atomic.get t)
      
    let steal_one t = Local.steal_one (Atomic.get t)

    let is_empty t = Local.is_empty (Atomic.get t)
  end

  let create ~size_exponent () = Atomic.make (create ~size_exponent ())
end

module M = struct
  (*
     Fitted into the module type expected by Domainslib.
     Note stealing now only takes 1 element.
  *)

  let create () = Resizable.create ~size_exponent:5 ()
  let push : ('a Resizable.t -> 'a -> unit) = Resizable.Local.push_with_autoresize

  let pop t = match Resizable.Local.pop t with Some v -> v | None -> raise Exit

  let steal = Resizable.Local.steal_one
end
