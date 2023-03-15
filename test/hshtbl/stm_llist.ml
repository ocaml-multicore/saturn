open QCheck
open STM
module Llist = Lockfree__.Htbl.Llist

module WSDConf = struct
  type cmd =
    | Add of int * int
    | Replace of int * int
    | Remove of int
    | Mem of int

  let show_cmd c =
    match c with
    | Add (k, v) -> "Add (" ^ string_of_int k ^ "," ^ string_of_int v ^ ")"
    | Remove k -> "Remove " ^ string_of_int k
    | Mem k -> "Mem " ^ string_of_int k
    | Replace (k, v) ->
        "Replace (" ^ string_of_int k ^ "," ^ string_of_int v ^ ")"

  module S = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type state = int S.t
  type sut = int Llist.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun k v -> Add (k, v)) int_gen int_gen;
           Gen.map2 (fun k v -> Replace (k, v)) int_gen int_gen;
           Gen.map (fun i -> Remove i) int_gen;
           Gen.map (fun i -> Mem i) int_gen;
         ])

  let init_state = S.empty
  let init_sut () = Llist.init ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add (k, v) -> if S.mem k s then s else S.add k v s
    | Replace (k, v) -> S.add k v s
    | Mem _ -> s
    | Remove k -> if S.mem k s then S.remove k s else s

  let precond _ _ = true

  let run c t =
    match c with
    | Add (k, v) -> Res (bool, Llist.add k (Llist.Regular v) t)
    | Replace (k, v) -> Res (unit, Llist.replace k (Llist.Regular v) t |> ignore)
    | Remove k -> Res (bool, Llist.remove k t)
    | Mem k -> Res (bool, Llist.mem k t)

  let postcond c (s : state) res =
    match (c, res) with
    | Add (k, _v), Res ((Bool, _), res) -> S.mem k s = not res
    | Replace (_k, _v), Res ((Unit, _), _) -> true
    | Mem k, Res ((Bool, _), res) -> S.mem k s = res
    | Remove k, Res ((Bool, _), res) -> S.mem k s = res
    | _, _ -> false
end

module WSDT_seq = STM_sequential.Make (WSDConf)
module WSDT_dom = STM_domain.Make (WSDConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main
    [
      WSDT_seq.agree_test ~count
        ~name:"STM Lockfree.Linked_list test sequential";
      WSDT_dom.agree_test_par ~count
        ~name:"STM Lockfree.Linked_list test parallel";
    ]
