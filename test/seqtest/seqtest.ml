open Monolith
open Saturn_lockfree.Work_stealing_deque

(* This sequential implementation of stacks serves as a reference. *)

(* For efficiency, we implement a bounded deque inside an array. *)

(* [bound] is both the maximum size of the deque in the reference
   implementation and the maximum length of the test scenarios that
   we use. *)

let bound = 1050

module R (* Reference *) = struct
  (* To avoid difficuties with initialization problem, we specialize
     our deque: its elements are integers. This is good enough for
     our purposes. *)

  type deque = { mutable top : int; mutable bottom : int; data : int array }

  let default = -1

  let create () =
    let top = 0 and bottom = 0 and data = Array.make bound default in
    { top; bottom; data }

  let push deque x =
    (* The capacity of the array cannot be exceeded because our test
       scenarios are sufficiently short. *)
    assert (deque.bottom < bound);
    deque.data.(deque.bottom) <- x;
    deque.bottom <- deque.bottom + 1

  let pop deque =
    assert (deque.top <= deque.bottom);
    if deque.top = deque.bottom then raise Exit
    else (
      deque.bottom <- deque.bottom - 1;
      let x = deque.data.(deque.bottom) in
      x)

  let steal deque =
    assert (deque.top <= deque.bottom);
    if deque.top = deque.bottom then raise Exit
    else
      let x = deque.data.(deque.top) in
      deque.top <- deque.top + 1;
      x
end

(* The work-stealing queue is the candidate implementation. *)

module C (* Candidate *) = M

(* Define [element] as an alias for the concrete type [int]. Equip it with a
   deterministic generator of fresh elements. *)

let element = sequential ()

(* Declare an abstract type [stack]. *)

let stack = declare_abstract_type ()

(* Declare the operations. *)

let () =
  let spec = unit ^> stack in
  declare "create" spec R.create C.create;

  let spec = stack ^> element ^> unit in
  declare "push" spec R.push C.push;

  let spec = stack ^!> element in
  declare "pop" spec R.pop C.pop;

  let spec = stack ^!> element in
  declare "steal" spec R.steal C.steal

(* Start the engine! *)

let () =
  let fuel = bound in
  main fuel
