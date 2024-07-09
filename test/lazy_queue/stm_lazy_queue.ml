open QCheck
open STM
module Queue = Saturn_lockfree.Lazy_queue

module Spec = struct
  type cmd = Push of int | Pop | Peek | Is_empty

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Peek -> "Peek"
    | Is_empty -> "Is_empty"

  type state = int list
  type sut = int Queue.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.return Pop;
           Gen.return Peek;
           Gen.return Is_empty;
         ])

  let init_state = []
  let init_sut () = Queue.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push i -> i :: s
    | Pop -> begin match List.rev s with [] -> s | _ :: s' -> List.rev s' end
    | Peek | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Queue.push d i)
    | Pop -> Res (option int, Queue.pop_opt d)
    | Peek -> Res (option int, Queue.peek_opt d)
    | Is_empty -> Res (bool, Queue.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | (Pop | Peek), Res ((Option Int, _), res) -> begin
        match List.rev s with [] -> res = None | j :: _ -> res = Some j
      end
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

let () =
  Stm_run.run ~count:500 ~verbose:true ~name:"Saturn_lockfree.Lazy_queue"
    (module Spec)
  |> exit
