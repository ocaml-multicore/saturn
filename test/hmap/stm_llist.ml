open QCheck
open STM
module Llist = Saturn.Linked_list

module WSDConf = struct
  type cmd =
    | Add of int * int
    | Try_remove of int
    | Mem of int
    | Find_all of int
    | Replace of int * int
    | Find_opt of int
    | Length

  let show_cmd c =
    match c with
    | Add (k, v) -> "Add (" ^ string_of_int k ^ "," ^ string_of_int v ^ ")"
    | Try_remove k -> "Try_remove " ^ string_of_int k
    | Mem k -> "Mem " ^ string_of_int k
    | Length -> "Length"
    | Find_all k -> "Find_all " ^ string_of_int k
    | Find_opt k -> "Find_opt " ^ string_of_int k
    | Replace (k, v) ->
        "Replace (" ^ string_of_int k ^ "," ^ string_of_int v ^ ")"

  module S = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type state = int list S.t
  type sut = (int, int) Llist.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun k v -> Add (k, v)) int_gen int_gen;
           Gen.map (fun i -> Try_remove i) int_gen;
           Gen.map (fun i -> Mem i) int_gen;
           Gen.return Length;
           Gen.map (fun i -> Find_all i) int_gen;
           Gen.map2 (fun k v -> Replace (k, v)) int_gen int_gen;
           Gen.map (fun i -> Find_opt i) int_gen;
         ])

  let init_state = S.empty
  let init_sut () = Llist.create ~compare ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add (k, v) -> begin
        match S.find_opt k s with
        | None -> S.add k [ v ] s
        | Some vs ->
            let s = S.remove k s in
            S.add k (v :: vs) s
      end
    | Try_remove k -> begin
        match S.find_opt k s with
        | Some (_ :: []) -> S.remove k s
        | Some (_ :: vs) ->
            let s = S.remove k s in
            S.add k vs s
        | _ -> s
      end
    | Mem _ -> s
    | Length -> s
    | Find_all _ -> s
    | Find_opt _ -> s
    | Replace (k, v) -> begin
        match S.find_opt k s with
        | None -> S.add k [ v ] s
        | Some (_ :: xs) ->
            let s = S.remove k s in
            S.add k (v :: xs) s
        | _ -> assert false
      end

  let precond _ _ = true

  let run c t =
    match c with
    | Add (k, v) -> Res (unit, Llist.add t k v)
    | Try_remove k -> Res (bool, Llist.try_remove t k)
    | Mem k -> Res (bool, Llist.mem t k)
    | Length -> Res (int, Llist.length t)
    | Find_all k -> Res (list int, Llist.find_all t k)
    | Replace (k, v) -> Res (unit, Llist.replace t k v)
    | Find_opt k -> Res (option int, Llist.find_opt t k)

  let postcond c (s : state) res =
    match (c, res) with
    | Add (_k, _v), Res ((Unit, _), _res) -> true
    | Try_remove k, Res ((Bool, _), res) -> S.mem k s = res
    | Mem k, Res ((Bool, _), res) -> S.mem k s = res
    | Length, Res ((Int, _), res) -> res = S.cardinal s
    | Find_all k, Res ((List Int, _), res) -> (
        match S.find_opt k s with None -> res = [] | Some r -> r = res)
    | Find_opt k, Res ((Option Int, _), res) ->
        S.find_opt k s |> Option.map List.hd = res
    | Replace (_k, _v), Res ((Unit, _), _) -> true
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
