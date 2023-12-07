open QCheck
open STM
module Llist = Saturn.Linked_list

module WSDConf = struct
  type cmd = Try_add of int * int | Try_remove of int | Mem of int

  (* | Replace of int * int
       | Find_opt of int*)

  let show_cmd c =
    match c with
    | Try_add (k, v) ->
        "Try_add (" ^ string_of_int k ^ "," ^ string_of_int v ^ ")"
    | Try_remove k -> "Try_remove " ^ string_of_int k
    | Mem k -> "Mem " ^ string_of_int k
  (* | Find_opt k -> "Find_opt " ^ string_of_int k
     | Replace (k, v) ->
         "Replace (" ^ string_of_int k ^ "," ^ string_of_int v ^ ")"*)

  module S = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type state = int S.t
  type sut = (int, int) Llist.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun k v -> Try_add (k, v)) int_gen int_gen;
           Gen.map (fun i -> Try_remove i) int_gen;
           Gen.map (fun i -> Mem i) int_gen;
           (* Gen.map2 (fun k v -> Replace (k, v)) int_gen int_gen;
                Gen.map (fun i -> Find_opt i) int_gen;*)
         ])

  let init_state = S.empty
  let init_sut () = Llist.create ~compare ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Try_add (k, v) -> if S.mem k s then s else S.add k v s
    | Try_remove k -> if S.mem k s then S.remove k s else s
    | Mem _ -> s
  (*| Replace (k, v) -> S.add k v s
      | Find_opt _ -> s*)

  let precond _ _ = true

  let run c t =
    match c with
    | Try_add (k, v) -> Res (bool, Llist.try_add t k v)
    | Try_remove k -> Res (bool, Llist.try_remove t k)
    | Mem k -> Res (bool, Llist.mem t k)
  (*| Replace (k, v) -> Res (unit, Llist.replace k v t |> ignore)
      | Find_opt k -> Res (option int, Llist.find_opt k t)*)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_add (k, _v), Res ((Bool, _), res) -> S.mem k s = not res
    | Try_remove k, Res ((Bool, _), res) -> S.mem k s = res
    | Mem k, Res ((Bool, _), res) -> S.mem k s = res
    (*| Replace (_k, _v), Res ((Unit, _), _) -> true
        | Find_opt k, Res ((Option Int, _), res) -> S.find_opt k s = res*)
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
