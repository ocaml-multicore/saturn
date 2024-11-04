open QCheck
open STM

module Skiplist = struct
  include Saturn.Skiplist

  type nonrec 'a t = ('a, unit) t

  let try_add s k = try_add s k ()
end

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
  type sut = int Skiplist.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Add i) int_gen;
           Gen.map (fun i -> Mem i) int_gen;
           Gen.map (fun i -> Remove i) int_gen;
           Gen.return Length;
         ])

  let init_state = Sint.empty
  let init_sut () = Skiplist.create ~compare:Int.compare ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add i -> Sint.add i s
    | Remove i -> Sint.remove i s
    | Mem _ -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Add i -> Res (bool, Skiplist.try_add d i)
    | Remove i -> Res (bool, Skiplist.try_remove d i)
    | Mem i -> Res (bool, Skiplist.mem d i)
    | Length -> Res (int, Skiplist.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Add i, Res ((Bool, _), res) -> Sint.mem i s = not res
    | Remove i, Res ((Bool, _), res) -> Sint.mem i s = res
    | Mem i, Res ((Bool, _), res) -> Sint.mem i s = res
    | Length, Res ((Int, _), res) -> Sint.cardinal s = res
    | _, _ -> false
end

let () = Stm_run.run ~name:"Lockfree.Skiplist" (module Spec) |> exit
