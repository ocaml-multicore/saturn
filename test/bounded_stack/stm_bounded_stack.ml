(** Sequential and Parallel model-based tests of bounded_queue *)

open QCheck
open STM
module Stack = Saturn_lockfree.Bounded_stack

module Spec = struct
  type cmd =
    | Try_push of int
    | Pop_opt
    | Pop_all
    | Peek_opt
    | Is_empty
    | Length

  let show_cmd c =
    match c with
    | Try_push i -> "Try_push " ^ string_of_int i
    | Pop_opt -> "Pop_opt"
    | Pop_all -> "Pop_all"
    | Peek_opt -> "Peek_opt"
    | Is_empty -> "Is_empty"
    | Length -> "Length"

  type state = int list
  type sut = int Stack.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Try_push i) int_gen;
           Gen.return Pop_opt;
           Gen.return Pop_all;
           Gen.return Is_empty;
           Gen.return Length;
           Gen.return Peek_opt;
           Gen.return Is_empty;
         ])

  let init_state = []
  let capacity = 5
  let init_sut () = Stack.create ~capacity ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Try_push i -> if List.length s >= capacity then s else i :: s
    | Pop_opt -> ( match s with [] -> s | _ :: s' -> s')
    | Pop_all -> []
    | Peek_opt -> s
    | Is_empty -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Try_push i -> Res (bool, Stack.try_push d i)
    | Pop_opt -> Res (option int, Stack.pop_opt d)
    | Pop_all -> Res (list int, Stack.pop_all d)
    | Peek_opt -> Res (option int, Stack.peek_opt d)
    | Is_empty -> Res (bool, Stack.is_empty d)
    | Length -> Res (int, Stack.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_push _, Res ((Bool, _), res) -> List.length s < capacity = res
    | Pop_opt, Res ((Option Int, _), res) -> (
        match s with [] -> res = None | j :: _ -> res = Some j)
    | Pop_all, Res ((List Int, _), res) -> res = s
    | Peek_opt, Res ((Option Int, _), res) -> (
        match s with [] -> res = None | j :: _ -> res = Some j)
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | Length, Res ((Int, _), res) -> res = List.length s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn_lockfree.Bounded_stack" (module Spec) |> exit
