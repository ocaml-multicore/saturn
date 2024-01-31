open QCheck
open STM


module Hshtbl = Saturn_lockfree.Hshtbl2

module WSDConf = struct
  type cmd =
    | Add of int * int
    | Try_remove of int
    | Mem of int
    | Find_all of int
    | Length
    | Replace of int * int
    | Find_opt of int

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

  module Sint = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type state = int list Sint.t
  type sut = (int, int) Hshtbl.t

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

  let init_state = Sint.empty
  let init_sut () = Hshtbl.create ~size_exponent:8
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add (k, v) -> begin
        match Sint.find_opt k s with
        | None -> Sint.add k [ v ] s
        | Some vs ->
            let s = Sint.remove k s in
            Sint.add k (v :: vs) s
      end
    | Try_remove k -> begin
        match Sint.find_opt k s with
        | Some (_ :: []) -> Sint.remove k s
        | Some (_ :: vs) ->
            let s = Sint.remove k s in
            Sint.add k vs s
        | _ -> s
      end
    | Mem _ -> s
    | Length -> s
    | Find_all _ -> s
    | Find_opt _ -> s
    | Replace (k, v) -> begin
        match Sint.find_opt k s with
        | None -> Sint.add k [ v ] s
        | Some [] ->
            let s = Sint.remove k s in
            Sint.add k [ v ] s
        | Some (_ :: xs) ->
            let s = Sint.remove k s in
            Sint.add k (v :: xs) s
      end

  let precond _ _ = true

  let run c t =
    match c with
    | Add (k, v) -> Res (unit, Hshtbl.add t k v)
    | Try_remove k -> Res (bool, Hshtbl.try_remove t k)
    | Mem k -> Res (bool, Hshtbl.mem t k)
    | Length -> Res (int, Hshtbl.length t)
    | Find_all k -> Res (list int, Hshtbl.find_all t k)
    | Replace (k, v) -> Res (unit, Hshtbl.replace t k v)
    | Find_opt k -> Res (option int, Hshtbl.find_opt t k)

  let postcond c (s : state) res =
    match (c, res) with
    | Add (_k, _v), Res ((Unit, _), _res) -> true
    | Try_remove k, Res ((Bool, _), res) -> begin
        match Sint.find_opt k s with
        | None | Some [] -> res = false
        | Some _ -> res = true
      end
    | Mem k, Res ((Bool, _), res) -> (
        match Sint.find_opt k s with
        | None | Some [] -> res = false
        | Some _ -> res = true)
    | Length, Res ((Int, _), res) ->
        let bindings = Sint.bindings s in
        let len =
          List.filter
            (fun (_, binding) -> not @@ List.is_empty binding)
            bindings
          |> List.length
        in
        len = res
    | Find_all k, Res ((List Int, _), res) -> (
        match Sint.find_opt k s with None -> res = [] | Some r -> r = res)
    | Find_opt k, Res ((Option Int, _), res) -> begin
        match Sint.find_opt k s with
        | None -> res = None
        | Some [] -> res = None
        | Some (x :: _) -> Some x = res
      end
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
