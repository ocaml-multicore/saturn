(** Sequential and Parallel model-based tests of fine_list *)

open QCheck
open STM
module Fine_list = Saturn.Fine_list

module FLConf = struct
  type cmd = Add of int | Remove of int | Contains of int | Is_empty

  (* possible operations *)
  let show_cmd c =
    match c with
    | Add i -> "Add " ^ string_of_int i
    | Remove i -> "Remove " ^ string_of_int i
    | Contains i -> "Contains " ^ string_of_int i
    | Is_empty -> "Is_empty"

  (* model state and system state *)
  type state = int list
  type sut = int Fine_list.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Add i) int_gen;
           Gen.map (fun i -> Remove i) int_gen;
           Gen.map (fun i -> Contains i) int_gen;
           Gen.return Is_empty;
         ])

  let init_state = []
  let init_sut () = Fine_list.create 0
  let cleanup _ = ()

  (* next stage after performing operation on current state *)
  let next_state c s =
    match c with
    | Add i ->
        let rec sortlist l e =
          match l with
          | [] -> [ e ]
          | x :: ys ->
              if e = x then x :: ys
              else if e < x then e :: x :: ys
              else x :: sortlist ys e
        in
        sortlist s i
    | Remove i ->
        let rec sortlist l e =
          match l with
          | [] -> []
          | x :: ys -> if e = x then ys else x :: sortlist ys e
        in
        sortlist s i
    | Contains _ -> s
    | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Add i -> Res (bool, Fine_list.add d i)
    | Remove i -> Res (bool, Fine_list.remove d i)
    | Contains i -> Res (bool, Fine_list.contains d i)
    | Is_empty -> Res (bool, Fine_list.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Add ele, Res ((Bool, _), res) -> res = not (List.mem ele s)
    | Remove ele, Res ((Bool, _), res) -> res = List.mem ele s
    | Contains ele, Res ((Bool, _), res) -> res = List.mem ele s
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

module FL_seq = STM_sequential.Make (FLConf)
module FL_dom = STM_domain.Make (FLConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main
    [
      FL_seq.agree_test ~count ~name:"STM Fine_list test sequential";
      FL_dom.agree_test_par ~count ~name:"STM Fine_list test parallel";
    ]
