module Queue = Saturn_lockfree.Two_stack_queue

let () =
  let q = Queue.create () in
  Queue.push q 101;
  Queue.push q 42;
  assert (Queue.pop_opt q = Some 101);
  Queue.push q 76;
  assert (Queue.pop_opt q = Some 42);
  assert (Queue.pop_opt q = Some 76);
  assert (Queue.pop_opt q = None)

module Spec = struct
  type cmd = Push of int | Push_head of int | Pop_opt | Length

  let show_cmd = function
    | Push x -> "Push " ^ string_of_int x
    | Push_head x -> "Push_head " ^ string_of_int x
    | Pop_opt -> "Pop_opt"
    | Length -> "Length"

  module State = struct
    type t = int list * int list

    let push x (h, t) = if h == [] then ([ x ], []) else (h, x :: t)
    let push_head x (h, t) = (x :: h, t)
    let peek_opt (h, _) = match h with x :: _ -> Some x | [] -> None
    let length (h, t) = List.length h + List.length t

    let drop ((h, t) as s) =
      match h with [] -> s | [ _ ] -> (List.rev t, []) | _ :: h -> (h, t)
  end

  type state = State.t
  type sut = int Queue.t

  let arb_cmd _s =
    let open QCheck in
    [
      Gen.int_range 1 10000 |> Gen.map (fun x -> Push x);
      Gen.int_range 1 10000 |> Gen.map (fun x -> Push_head x);
      Gen.return Pop_opt;
      Gen.return Length;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = ([], [])
  let init_sut () = Queue.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push x -> State.push x s
    | Push_head x -> State.push_head x s
    | Pop_opt -> State.drop s
    | Length -> s

  let precond _ _ = true

  let run c d =
    let open STM in
    match c with
    | Push x -> Res (unit, Queue.push d x)
    | Push_head x -> Res (unit, Queue.push_head d x)
    | Pop_opt -> Res (option int, Queue.pop_opt d)
    | Length -> Res (int, Queue.length d)

  let postcond c (s : state) res =
    let open STM in
    match (c, res) with
    | Push _x, Res ((Unit, _), ()) -> true
    | Push_head _x, Res ((Unit, _), ()) -> true
    | Pop_opt, Res ((Option Int, _), res) -> res = State.peek_opt s
    | Length, Res ((Int, _), res) -> res = State.length s
    | _, _ -> false
end

let () =
  Stm_run.run ~count:1000 ~verbose:true ~name:"Two_stack_queue" (module Spec)
  |> exit
