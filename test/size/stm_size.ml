module Linked_set = Linked_set.Make (Atomic) (Saturn_lockfree.Size)

module Spec = struct
  type cmd = Mem of int | Add of int | Remove of int | Length

  let show_cmd c =
    match c with
    | Mem i -> "Mem " ^ string_of_int i
    | Add i -> "Add " ^ string_of_int i
    | Remove i -> "Remove " ^ string_of_int i
    | Length -> "Length"

  module Sint = Set.Make (Int)

  type state = Sint.t
  type sut = int Linked_set.t

  let arb_cmd _s =
    QCheck.(
      make ~print:show_cmd
        (let int_gen = Gen.nat in
         Gen.oneof
           [
             Gen.map (fun i -> Add i) int_gen;
             Gen.map (fun i -> Mem i) int_gen;
             Gen.map (fun i -> Remove i) int_gen;
             Gen.return Length;
           ]))

  let init_state = Sint.empty
  let init_sut () = Linked_set.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add i -> Sint.add i s
    | Remove i -> Sint.remove i s
    | Mem _ -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    let open STM in
    match c with
    | Add i -> Res (bool, Linked_set.try_add d i)
    | Remove i -> Res (bool, Linked_set.try_remove d i)
    | Mem i -> Res (bool, Linked_set.mem d i)
    | Length -> Res (int, Linked_set.length d)

  let postcond c (s : state) res =
    let open STM in
    match (c, res) with
    | Add i, Res ((Bool, _), res) -> Sint.mem i s = not res
    | Remove i, Res ((Bool, _), res) -> Sint.mem i s = res
    | Mem i, Res ((Bool, _), res) -> Sint.mem i s = res
    | Length, Res ((Int, _), res) -> Sint.cardinal s = res
    | _, _ -> false
end

module Seq = STM_sequential.Make (Spec)
module Par = STM_domain.Make (Spec)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      Seq.agree_test ~count ~name:"STM Saturn_lockfree.Size test sequential";
      Par.agree_test_par ~count ~name:"STM Saturn_lockfree.Size test parallel";
    ]
