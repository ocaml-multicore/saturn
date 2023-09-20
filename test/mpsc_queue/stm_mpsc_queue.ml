(** Sequential and Parallel model-based tests of mpsc_queue *)

open QCheck
open STM
open Util
module Mpsc_queue = Saturn.Single_consumer_queue

module MPSCConf = struct
  type cmd = Push of int | Pop | Push_head of int | Is_empty | Close

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Push_head i -> "Push_head" ^ string_of_int i
    | Is_empty -> "Is_empty"
    | Close -> "Close"

  type state = bool * int list
  type sut = int Mpsc_queue.t

  let producer_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.return Is_empty;
           Gen.return Close;
         ])

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.return Pop;
           Gen.map (fun i -> Push_head i) int_gen;
           Gen.return Is_empty;
           Gen.return Close;
         ])

  let init_state = (false, [])
  let init_sut () = Mpsc_queue.create ()
  let cleanup _ = ()

  let next_state c (is_closed, s) =
    match c with
    | Push i ->
        (is_closed, if not is_closed then i :: List.rev s |> List.rev else s)
    | Push_head i -> (is_closed, if not (is_closed && s = []) then i :: s else s)
    | Is_empty -> (is_closed, s)
    | Pop -> (is_closed, match s with [] -> s | _ :: s' -> s')
    | Close -> (true, s)

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (result unit exn, protect (fun d -> Mpsc_queue.push d i) d)
    | Pop -> Res (result (option int) exn, protect Mpsc_queue.pop d)
    | Push_head i ->
        Res (result unit exn, protect (fun d -> Mpsc_queue.push_head d i) d)
    | Is_empty -> Res (result bool exn, protect Mpsc_queue.is_empty d)
    | Close -> Res (result unit exn, protect Mpsc_queue.close d)

  let postcond c ((is_closed, s) : state) res =
    match (c, res) with
    | Push _, Res ((Result (Unit, Exn), _), res) ->
        if is_closed then res = Error Mpsc_queue.Closed else res = Ok ()
    | Push_head _, Res ((Result (Unit, Exn), _), res) ->
        if is_closed && s = [] then res = Error Mpsc_queue.Closed
        else res = Ok ()
    | Pop, Res ((Result (Option Int, Exn), _), res) -> (
        match s with
        | [] ->
            if is_closed then res = Error Mpsc_queue.Closed else res = Ok None
        | x :: _ -> res = Ok (Some x))
    | Is_empty, Res ((Result (Bool, Exn), _), res) ->
        if is_closed && s = [] then res = Error Mpsc_queue.Closed
        else res = Ok (s = [])
    | Close, Res ((Result (Unit, Exn), _), res) ->
        if is_closed then res = Error Mpsc_queue.Closed else res = Ok ()
    | _, _ -> false
end

module MPSC_seq = STM_sequential.Make (MPSCConf)
module MPSC_dom = STM_domain.Make (MPSCConf)

(* [arb_cmds_par] differs in what each triple component generates:
   "Consumer domain" cmds can't be [Push] (but can be [Pop], [Is_empty], [Close] or [Push_head]),
   "producer domain" cmds can't be [Push_head] or [Pop] (but can be [Push], [Is_empty] or [Close]). *)
let arb_cmds_par =
  MPSC_dom.arb_triple 20 12 MPSCConf.arb_cmd MPSCConf.arb_cmd
    MPSCConf.producer_cmd

(* A parallel agreement test - w/repeat and retries combined *)
let agree_test_par_asym ~count ~name =
  let rep_count = 50 in
  Test.make ~retries:10 ~count ~name arb_cmds_par (fun triple ->
      assume (MPSC_dom.all_interleavings_ok triple);
      repeat rep_count MPSC_dom.agree_prop_par_asym triple)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      MPSC_seq.agree_test ~count ~name:"STM Saturn.Mpsc_queue test sequential";
      agree_test_par_asym ~count ~name:"STM Saturn.Mpsc_queue test parallel";
      MPSC_dom.neg_agree_test_par ~count
        ~name:"STM Saturn.Mpsc_queue test parallel, negative";
    ]
