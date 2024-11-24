open QCheck
open STM

(** Sequential and Parallel model-based tests of michael_scott_queue *)
module STM_ms_queue (Queue : Ms_queues.MS_queue_tests) = struct
  module Spec = struct
    type cmd = Push of int | Pop | Peek | Drop | Is_empty

    let show_cmd c =
      match c with
      | Push i -> "Push " ^ string_of_int i
      | Pop -> "Pop"
      | Peek -> "Peek"
      | Drop -> "Drop"
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
             Gen.return Drop;
             Gen.return Is_empty;
           ])

    let init_state = []
    let init_sut () = Queue.create ()
    let cleanup _ = ()

    let next_state c s =
      match c with
      | Push i -> i :: s
      | Pop | Drop -> (
          match List.rev s with [] -> s | _ :: s' -> List.rev s')
      | Peek | Is_empty -> s

    let precond _ _ = true

    let run c d =
      match c with
      | Push i -> Res (unit, Queue.push d i)
      | Pop -> Res (result int exn, protect Queue.pop_exn d)
      | Peek -> Res (result int exn, protect Queue.peek_exn d)
      | Drop -> Res (result unit exn, protect Queue.drop_exn d)
      | Is_empty -> Res (bool, Queue.is_empty d)

    let postcond c (s : state) res =
      match (c, res) with
      | Push _, Res ((Unit, _), _) -> true
      | (Pop | Peek), Res ((Result (Int, Exn), _), res) -> (
          match List.rev s with
          | [] -> res = Error Queue.Empty
          | j :: _ -> res = Ok j)
      | Drop, Res ((Result (Unit, Exn), _), res) -> (
          match List.rev s with
          | [] -> res = Error Queue.Empty
          | _ -> res = Ok ())
      | Is_empty, Res ((Bool, _), res) -> res = (s = [])
      | _, _ -> false
  end

  let run () = Stm_run.run ~name:("Saturn." ^ Queue.name) (module Spec)
end

let () =
  let module Safe = STM_ms_queue (Ms_queues.Michael_scott_queue) in
  let exit_code = Safe.run () in
  if exit_code <> 0 then exit exit_code
  else
    let module Unsafe = STM_ms_queue (Ms_queues.Michael_scott_queue_unsafe) in
    Unsafe.run () |> exit
