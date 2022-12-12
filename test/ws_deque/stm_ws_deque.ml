(** Sequential and Parallel model-based tests of ws_deque *)

open QCheck
open STM
open Util
module Ws_deque = Lockfree.Ws_deque

module WSDConf =
struct
  type cmd =
    | Push of int
    | Pop
    | Steal

  let show_cmd c = match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Steal -> "Steal"

  type state = int list
  type sut = int Ws_deque.M.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.map (fun i -> Push i) int_gen;
          Gen.return Pop;
          (*Gen.return Steal;*) (* No point in stealing from yourself :-D *)
         ])
  let stealer_cmd _s =
    QCheck.make ~print:show_cmd (Gen.return Steal)

  let init_state  = []
  let init_sut () = Ws_deque.M.create ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Push i   -> i::s (*if i<>1213 then i::s else s*) (* an artificial fault *)
    | Pop      -> (match s with
        | []    -> s
        | _::s' -> s')
    | Steal    -> (match List.rev s with
        | []    -> s
        | _::s' -> List.rev s')

  let precond _ _ = true

  let run c d = match c with
    | Push i   -> Res (unit, Ws_deque.M.push d i)
    | Pop      -> Res (result int exn, protect Ws_deque.M.pop d)
    | Steal    -> Res (result int exn, protect Ws_deque.M.steal d)

  let postcond c (s : state) res = match c,res with
    | Push _, Res ((Unit,_),_) -> true
    | Pop,    Res ((Result (Int,Exn),_),res) ->
        (match s with
         | []   -> res = Error Exit
         | j::_ -> res = Ok j)
    | Steal,  Res ((Result (Int,Exn),_),res) ->
        (match List.rev s with
         | []   -> Result.is_error res
         | j::_ -> res = Ok j)
    | _,_ -> false
end

module WSDT_seq = STM_sequential.Make(WSDConf)
module WSDT_dom = STM_domain.Make(WSDConf)

(* The following definitions differ slightly from those in multicoretests:lib/STM.ml.
   This has to do with how work-stealing deques are supposed to be used according to spec:
   - [agree_prop_par] differs in that it only spawns one domain ("a stealer domain")
     in parallel with the original "owner domain" (it also uses [Semaphore.Binary]) *)
let agree_prop_par =
  (fun (seq_pref,owner,stealer) ->
    assume (WSDT_seq.cmds_ok WSDConf.init_state (seq_pref@owner));
    assume (WSDT_seq.cmds_ok WSDConf.init_state (seq_pref@stealer));
    let sut = WSDConf.init_sut () in
    let pref_obs = WSDT_dom.interp_sut_res sut seq_pref in
    let sema = Semaphore.Binary.make false in
    let stealer_dom = Domain.spawn (fun () -> Semaphore.Binary.release sema; WSDT_dom.interp_sut_res sut stealer) in
    while not (Semaphore.Binary.try_acquire sema) do Domain.cpu_relax() done;
    let own_obs = WSDT_dom.interp_sut_res sut owner in
    let stealer_obs = Domain.join stealer_dom in
    let res = WSDT_dom.check_obs pref_obs own_obs stealer_obs WSDConf.init_state in
    let () = WSDConf.cleanup sut in
    res ||
      Test.fail_reportf "  Results incompatible with linearized model:\n\n%s"
      @@ Util.print_triple_vertical ~center_prefix:false STM.show_res
           (List.map snd pref_obs,
            List.map snd own_obs,
            List.map snd stealer_obs))

(* [arb_cmds_par] differs in what each triple component generates:
   "Owner domain" cmds can't be [Steal], "stealer domain" cmds can only be [Steal]. *)
let arb_cmds_par = WSDT_dom.arb_triple 20 15 WSDConf.arb_cmd WSDConf.arb_cmd WSDConf.stealer_cmd

(* A parallel agreement test - w/repeat and retries combined *)
let agree_test_par ~count ~name =
  let rep_count = 50 in
  Test.make ~retries:10 ~count ~name
    arb_cmds_par (repeat rep_count agree_prop_par)

(* Note: this can generate, e.g., pop commands/actions in different threads, thus violating the spec. *)
let agree_test_par_negative ~count ~name = WSDT_dom.neg_agree_test_par ~count ~name

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main [
    WSDT_seq.agree_test     ~count ~name:"STM Lockfree.Ws_deque test sequential";
    agree_test_par          ~count ~name:"STM Lockfree.Ws_deque test parallel";
    agree_test_par_negative ~count ~name:"STM Lockfree.Ws_deque test parallel, negative";
  ]
