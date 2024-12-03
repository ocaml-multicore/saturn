open QCheck
open STM
module Bag = Saturn.Bag

(* Only check that the size of the bag stays consistent. *)

module Spec = struct
  type cmd = Push | Pop

  let show_cmd c = match c with Push -> "Push ()" | Pop -> "Pop"

  module Sint = Set.Make (Int)

  type state = int
  type sut = unit Bag.t

  let arb_cmd _s =
    QCheck.make ~print:show_cmd (Gen.oneof [ Gen.return Push; Gen.return Pop ])

  let init_state = 0
  let init_sut () = Bag.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with Push -> s + 1 | Pop -> if s > 0 then s - 1 else s

  let precond _ _ = true

  let run c d =
    match c with
    | Push -> Res (unit, Bag.push d ())
    | Pop -> Res (option unit, Bag.pop_opt d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push, Res ((Unit, _), _res) -> true
    | Pop, Res ((Option Unit, _), res) ->
        if s > 0 then res = Some () else res = None
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn.Bag" (module Spec) |> exit
