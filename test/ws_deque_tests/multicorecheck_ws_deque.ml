(** Sequential tests of ws_deque *)

open QCheck
open STM

module Ws_deque = Lockfree.Ws_deque

module WSDConf =
struct
  type cmd =
    | Push of int  (* use int for now *)
    | Pop
    | Steal [@@deriving show { with_path = false }]
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

module WSDT = STM.Make(WSDConf)


(*
;;
QCheck_runner.run_tests ~verbose:true [
    WSDT.agree_test     ~count:1_000 ~name:"sequential ws_deque test";
    WSDT.agree_test_par ~count:1_000 ~name:"parallel ws_deque test (w/repeat)";
    agree_test_par      ~count:1_000 ~name:"parallel ws_deque test (w/non_det module)";
  ]
 *)

let agree_prop_par =
  (fun (seq_pref,owner,stealer) ->
    assume (WSDT.cmds_ok WSDConf.init_state (seq_pref@owner));
    assume (WSDT.cmds_ok WSDConf.init_state (seq_pref@stealer));
    let sut = WSDConf.init_sut () in
    let pref_obs = WSDT.interp_sut_res sut seq_pref in
    let sema = Semaphore.Binary.make false in
    let stealer_dom = Domain.spawn (fun () -> Semaphore.Binary.release sema; WSDT.interp_sut_res sut stealer) in
    while not (Semaphore.Binary.try_acquire sema) do Domain.cpu_relax() done;
    let own_obs = WSDT.interp_sut_res sut owner in
    let stealer_obs = Domain.join stealer_dom in
    let res = WSDT.check_obs pref_obs own_obs stealer_obs WSDConf.init_state in
    let () = WSDConf.cleanup sut in
    res ||
      Test.fail_reportf "  Results incompatible with linearized model:\n\n%s"
      @@ print_triple_vertical ~center_prefix:false show_res
           (List.map snd pref_obs,
            List.map snd own_obs,
            List.map snd stealer_obs))

let shrink_triple =
  let (<+>) = Iter.(<+>) in
  (fun (seq,p1,p2) ->
    (Shrink.(triple list list list) (seq,p1,p2))
    <+> (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2))
    <+> (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s)))

let arb_triple =
  let seq_len,par_len = 20,15 in
  let seq_pref_gen = WSDT.gen_cmds_size WSDConf.init_state (Gen.int_bound seq_len) in
  let triple_gen = Gen.(seq_pref_gen >>= fun seq_pref ->
                        let spawn_state = List.fold_left (fun st c -> WSDConf.next_state c st) WSDConf.init_state seq_pref in
                        let owner_gen = WSDT.gen_cmds_size spawn_state (Gen.int_bound par_len) in
                        let stealer_gen = list_size (int_bound par_len) (WSDConf.stealer_cmd spawn_state).gen in
                        map2 (fun owner stealer -> (seq_pref,owner,stealer)) owner_gen stealer_gen) in
  make ~print:(print_triple_vertical ~center_prefix:false WSDConf.show_cmd) ~shrink:shrink_triple triple_gen

(* A parallel agreement test - w/repeat and retries combined *)
let agree_test_par ~count ~name =
  let rep_count = 50 in
  Test.make ~retries:10 ~count ~name
    arb_triple (STM.repeat rep_count agree_prop_par) (* 50 times each, then 50 * 10 times when shrinking *)

(* Note: this can generate, e.g., pop commands/actions in different threads, thus violating the spec. *)
let agree_test_par_negative ~count ~name = WSDT.agree_test_par ~count ~name

;;
set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1000 in [
    WSDT.agree_test         ~count ~name:"ws_deque test";
    agree_test_par          ~count ~name:"parallel ws_deque test";
    agree_test_par_negative ~count ~name:"ws_deque test, negative";
  ])
