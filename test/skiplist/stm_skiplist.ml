(** Sequential and Parallel model-based tests of ws_deque *)

open QCheck
open STM
module Skiplist = Saturn.Skiplist

module WSDConf = struct
  type cmd = Mem of int | Add of int | Remove of int

  let show_cmd c =
    match c with
    | Mem i -> "Mem " ^ string_of_int i
    | Add i -> "Add " ^ string_of_int i
    | Remove i -> "Remove " ^ string_of_int i

  module Sint = Set.Make (struct
    type t = int

    let compare = compare
  end)

  type state = Sint.t
  type sut = int Skiplist.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Add i) int_gen;
           Gen.map (fun i -> Mem i) int_gen;
           Gen.map (fun i -> Remove i) int_gen;
         ])

  let init_state = Sint.empty
  let init_sut () = Skiplist.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add i -> Sint.add i s
    | Remove i -> Sint.remove i s
    | Mem _ -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Add i -> Res (bool, Skiplist.add d i)
    | Remove i -> Res (bool, Skiplist.remove d i)
    | Mem i -> Res (bool, Skiplist.mem d i)

  let postcond c (s : state) res =
    match (c, res) with
    | Add i, Res ((Bool, _), res) -> Sint.mem i s = not res
    | Remove i, Res ((Bool, _), res) -> Sint.mem i s = res
    | Mem i, Res ((Bool, _), res) -> Sint.mem i s = res
    | _, _ -> false
end

module WSDT_seq = STM_sequential.Make (WSDConf)
module WSDT_dom = STM_domain.Make (WSDConf)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      WSDT_seq.agree_test ~count ~name:"STM Lockfree.Skiplist test sequential";
      WSDT_dom.agree_test_par ~count ~name:"STM Lockfree.Skiplist test parallel";
    ]
