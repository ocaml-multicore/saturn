(** Sequential and Parallel model-based tests of ws_deque *)

open QCheck
open STM
open Util
module Ws_deque = Saturn_lockfree.Work_stealing_deque

module Spec = struct
  type cmd = Push of int | Pop | Steal

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Steal -> "Steal"

  type state = int list
  type sut = int Ws_deque.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.return Pop;
           (*Gen.return Steal;*)
           (* No point in stealing from yourself :-D *)
         ])

  let stealer_cmd _s = QCheck.make ~print:show_cmd (Gen.return Steal)
  let init_state = []
  let init_sut () = Ws_deque.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push i ->
        i :: s
        (*if i<>1213 then i::s else s*)
        (* an artificial fault *)
    | Pop -> ( match s with [] -> s | _ :: s' -> s')
    | Steal -> ( match List.rev s with [] -> s | _ :: s' -> List.rev s')

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Ws_deque.push d i)
    | Pop -> Res (result int exn, protect Ws_deque.pop d)
    | Steal -> Res (result int exn, protect Ws_deque.steal d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | Pop, Res ((Result (Int, Exn), _), res) -> (
        match s with [] -> res = Error Exit | j :: _ -> res = Ok j)
    | Steal, Res ((Result (Int, Exn), _), res) -> (
        match List.rev s with [] -> Result.is_error res | j :: _ -> res = Ok j)
    | _, _ -> false
end

let () =
  let make_domain ~count ~name
      (module Dom : Stm_run.STM_domain
        with type Spec.cmd = Spec.cmd
         and type Spec.state = Spec.state
         and type Spec.sut = Spec.sut) =
    (* A parallel agreement test - w/repeat and retries combined *)
    let agree_test_par_asym ~count ~name =
      let rep_count = 20 in
      let seq_len, par_len = (20, 12) in
      Test.make ~retries:10 ~count ~name
        (* "Owner domain" cmds can't be [Steal], "stealer domain" cmds can only be [Steal]. *)
        (Dom.arb_triple_asym seq_len par_len Spec.arb_cmd Spec.arb_cmd
           Spec.stealer_cmd) (fun triple ->
          assume (Dom.all_interleavings_ok triple);
          repeat rep_count Dom.agree_prop_par_asym triple)
    in
    [
      agree_test_par_asym ~count ~name:(name ^ " parallel");
      (* Note: this can generate, e.g., pop commands/actions in different threads, thus violating the spec. *)
      Dom.neg_agree_test_par ~count ~name:(name ^ " parallel, negative");
    ]
  in
  Stm_run.run ~name:"Saturn_lockfree.Ws_deque" ~make_domain (module Spec)
  |> exit
