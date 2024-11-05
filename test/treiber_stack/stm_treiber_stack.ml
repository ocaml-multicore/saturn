(** Sequential and Parallel model-based tests of treiber_stack *)

open QCheck
open STM
module Treiber_stack = Saturn.Stack

module Spec = struct
  type cmd = Push of int | Pop | Is_empty

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Is_empty -> "Is_empty"

  type state = int list
  type sut = int Treiber_stack.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.return Pop;
           Gen.return Is_empty;
         ])

  let init_state = []
  let init_sut () = Treiber_stack.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push i -> i :: s
    | Pop -> ( match s with [] -> s | _ :: s' -> s')
    | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Treiber_stack.push d i)
    | Pop -> Res (result int exn, protect Treiber_stack.pop d)
    | Is_empty -> Res (bool, Treiber_stack.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | Pop, Res ((Result (Int, Exn), _), res) -> (
        match s with
        | [] -> res = Error Treiber_stack.Empty
        | j :: _ -> res = Ok j)
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn.Treiber_stack" (module Spec) |> exit
