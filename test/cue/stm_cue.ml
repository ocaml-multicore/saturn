(** Sequential and Parallel model-based tests of (bounded queue) cue. *)

open QCheck
open STM
module Cue = Saturn.Cue

module Spec = struct
  type cmd = Try_push of int | Pop_opt | Peek_opt | Length | Is_empty

  let show_cmd c =
    match c with
    | Try_push i -> "Try_push " ^ string_of_int i
    | Pop_opt -> "Pop_opt"
    | Peek_opt -> "Peek_opt"
    | Length -> "Length"
    | Is_empty -> "Is_empty"

  type state = int * int * int list
  type sut = int Cue.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Try_push i) int_gen;
           Gen.return Pop_opt;
           Gen.return Peek_opt;
           Gen.return Length;
           Gen.return Is_empty;
         ])

  let init_state = (100, 0, [])
  let init_sut () = Cue.create ~capacity:100 ()
  let cleanup _ = ()

  let next_state c ((capacity, size, content) as s) =
    match c with
    | Try_push i ->
        if size = capacity then s else (capacity, size + 1, i :: content)
    | Pop_opt -> (
        match List.rev content with
        | [] -> s
        | _ :: content' -> (capacity, size - 1, List.rev content'))
    | Peek_opt -> s
    | Is_empty -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Try_push i -> Res (bool, Cue.try_push d i)
    | Pop_opt -> Res (option int, Cue.pop_opt d)
    | Peek_opt -> Res (option int, Cue.peek_opt d)
    | Is_empty -> Res (bool, Cue.is_empty d)
    | Length -> Res (int, Cue.length d)

  let postcond c ((capacity, size, content) : state) res =
    match (c, res) with
    | Try_push _, Res ((Bool, _), res) -> res = (size < capacity)
    | (Pop_opt | Peek_opt), Res ((Option Int, _), res) -> (
        match List.rev content with [] -> res = None | j :: _ -> res = Some j)
    | Is_empty, Res ((Bool, _), res) -> res = (content = [])
    | Length, Res ((Int, _), res) -> res = size
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn_lockfree.Cue" (module Spec) |> exit
