(** Sequential and Parallel model-based tests of spsc_queue *)

open QCheck
open STM
open Util
module Spsc_queue = Saturn_lockfree.Single_prod_single_cons_queue

module SPSCConf = struct
  type cmd = Push of int | Pop | Peek

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Peek -> "Peek"

  type state = int * int list
  type sut = int Spsc_queue.t

  let producer_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd (Gen.map (fun i -> Push i) int_gen)

  let consumer_cmd _s =
    QCheck.make ~print:show_cmd (Gen.oneof [ Gen.return Pop; Gen.return Peek ])

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [ Gen.return Pop; Gen.return Peek; Gen.map (fun i -> Push i) int_gen ])

  let size_exponent = 4
  let max_size = Int.shift_left 1 size_exponent
  let init_state = (0, [])
  let init_sut () = Spsc_queue.create ~size_exponent
  let cleanup _ = ()

  let next_state c (n, s) =
    match c with
    | Push i -> if n = max_size then (n, s) else (n + 1, i :: s)
    | Pop -> (
        match List.rev s with [] -> (0, s) | _ :: s' -> (n - 1, List.rev s'))
    | Peek -> (n, s)

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (result unit exn, protect (fun d -> Spsc_queue.push d i) d)
    | Pop -> Res (result int exn, protect Spsc_queue.pop d)
    | Peek -> Res (result int exn, protect Spsc_queue.peek d)

  let postcond c ((n, s) : state) res =
    match (c, res) with
    | Push _, Res ((Result (Unit, Exn), _), res) -> (
        match res with
        | Error Spsc_queue.Full -> n = max_size
        | Ok () -> n < max_size
        | _ -> false)
    | (Pop | Peek), Res ((Result (Int, Exn), _), res) -> (
        match (res, List.rev s) with
        | Error Spsc_queue.Empty, [] -> true
        | Ok popped, x :: _ -> x = popped
        | _ -> false)
    | _, _ -> false
end

module SPSC_seq = STM_sequential.Make (SPSCConf)
module SPSC_dom = STM_domain.Make (SPSCConf)

(* [arb_cmds_par] differs in what each triple component generates:
   "Producer domain" cmds can't be [Pop], "consumer domain" cmds can only be [Pop]. *)
let arb_cmds_par =
  SPSC_dom.arb_triple 20 12 SPSCConf.producer_cmd SPSCConf.producer_cmd
    SPSCConf.consumer_cmd

(* A parallel agreement test - w/repeat and retries combined *)
let agree_test_par_asym ~count ~name =
  let rep_count = 20 in
  Test.make ~retries:10 ~count ~name arb_cmds_par (fun triple ->
      assume (SPSC_dom.all_interleavings_ok triple);
      repeat rep_count SPSC_dom.agree_prop_par_asym triple)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      SPSC_seq.agree_test ~count
        ~name:"STM Saturn_lockfree.Spsc_queue test sequential";
      agree_test_par_asym ~count
        ~name:"STM Saturn_lockfree.Spsc_queue test parallel";
      (* Note: this can generate, e.g., pop commands/actions in different threads, thus violating the spec. *)
      SPSC_dom.neg_agree_test_par ~count
        ~name:"STM Saturn_lockfree.Spsc_queue test parallel, negative";
    ]
