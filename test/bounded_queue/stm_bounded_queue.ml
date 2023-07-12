(** Sequential and Parallel model-based tests of michael_scott_bounded_queue *)

open QCheck
open STM
module Bounded_queue = Saturn.Bounded_queue

module MSQBConf = struct
  type cmd = Push of int | Pop | Is_empty

  (* possible operations *)
  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Is_empty -> "Is_empty"

  (* model state and system state *)
  type state = int list
  type sut = int Bounded_queue.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.return Pop;
           Gen.return Is_empty;
         ])

  let init_state = []
  let init_sut () = Bounded_queue.init ()
  let cleanup _ = ()

  (* next stage after performing operation on current state *)
  let next_state c s =
    match c with
    | Push i -> i :: s
    | Pop -> ( match List.rev s with [] -> s | _ :: s' -> List.rev s')
    | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Bounded_queue.unbounded_push d i)
    | Pop -> Res (option int, Bounded_queue.unbounded_pop d)
    | Is_empty -> Res (bool, Bounded_queue.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | Pop, Res ((Option Int, _), res) -> (
        match List.rev s with [] -> res = None | j :: _ -> res = Some j)
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

module MSQB_seq = STM_sequential.Make (MSQBConf)
module MSQB_dom = STM_domain.Make (MSQBConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main
    [
      MSQB_seq.agree_test ~count ~name:"STM Bounded_queue test sequential";
      MSQB_dom.agree_test_par ~count ~name:"STM Bounded_queue test parallel";
    ]
