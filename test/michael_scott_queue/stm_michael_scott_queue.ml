(** Sequential and Parallel model-based tests of michael_scott_queue *)

open QCheck
open STM
module Ms_queue = Saturn.Queue

module MSQConf = struct
  type cmd = Push of int | Pop | Peek | Is_empty

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Peek -> "Peek"
    | Is_empty -> "Is_empty"

  type state = int list
  type sut = int Ms_queue.t

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
  let init_sut () = Ms_queue.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push i -> i :: s
    | Pop -> ( match List.rev s with [] -> s | _ :: s' -> List.rev s')
    | Peek | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Ms_queue.push d i)
    | Pop -> Res (result int exn, protect Ms_queue.pop d)
    | Peek -> Res (result int exn, protect Ms_queue.peek d)
    | Is_empty -> Res (bool, Ms_queue.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | (Pop | Peek), Res ((Result (Int, Exn), _), res) -> (
        match List.rev s with
        | [] -> res = Error Ms_queue.Empty
        | j :: _ -> res = Ok j)
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

module MSQ_seq = STM_sequential.Make (MSQConf)
module MSQ_dom = STM_domain.Make (MSQConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main
    [
      MSQ_seq.agree_test ~count
        ~name:"STM Saturn.Michael_scott_queue test sequential";
      MSQ_dom.agree_test_par ~count
        ~name:"STM Saturn.Michael_scott_queue test parallel";
    ]
