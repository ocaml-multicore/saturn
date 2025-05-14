open QCheck
open STM
module Priority_queue = Saturn.Priority_queue

module Spec = struct
  type cmd = Add of int * int | Remove_min
  (* | Find of int *)
  (* | Length *)

  let show_cmd c =
    match c with
    | Add (i, k) -> "Add (" ^ string_of_int i ^ ", " ^ string_of_int k ^ ")"
    | Remove_min -> "Remove_min"
  (* | Length -> "Length" *)
  (* | Find i -> "Find " ^ string_of_int i *)

  module Mint = Map.Make (Int)

  type state = int * int list Mint.t
  type sut = (int, int) Priority_queue.t

  let arb_cmd _s =
    let priority_gen = Gen.int_bound 5 in
    let val_gen = Gen.int_bound 20 in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun p i -> Add (p, i)) priority_gen val_gen;
           (* Gen.map (fun p -> Find p) priority_gen; *)
           Gen.return Remove_min;
           (* Gen.return Length; *)
         ])

  let init_state = (0, Mint.empty)
  let init_sut () = Priority_queue.create ~compare:Int.compare ()
  let cleanup _ = ()

  let next_state c ((len, s) : state) =
    match c with
    | Add (p, i) ->
        ( len + 1,
          begin
            match Mint.find_opt p s with
            | None -> Mint.add p [ i ] s
            | Some l -> Mint.add p (i :: List.rev l |> List.rev) s
          end )
    | Remove_min -> (
        let bindings = Mint.bindings s in
        match bindings with
        | [] -> (len, s)
        | (p, values) :: _ ->
            ( len - 1,
              let s = Mint.remove p s in
              match values with
              | [] -> assert false
              | [ _x ] -> s
              | _ :: values -> Mint.add p values s )
        (* | Find _ -> s *))
  (* | Length -> (len, s) *)

  let precond _ _ = true

  let run c d =
    match c with
    | Add (p, i) -> Res (unit, Priority_queue.add d p i)
    | Remove_min ->
        Res
          ( list int,
            match Priority_queue.remove_min_opt d with
            | None -> []
            | Some (p, v) -> [ p; v ] )
  (* | Find i -> Res (option int, Priority_queue.find_opt d i) *)
  (* | Length -> Res (int, Priority_queue.length d) *)

  let postcond c ((_, s) : state) res =
    match (c, res) with
    | Add (_, _), Res ((Unit, _), ()) -> true
    | Remove_min, Res ((List Int, _), res) -> begin
        match Mint.bindings s with
        | [] -> res = []
        | (p, values) :: _ -> begin
            match values with [] -> assert false | v :: _ -> res = [ p; v ]
          end
      end
    (* | Length, Res ((Int, _), res) -> len = res *)
    (* | Find p, Res ((Option Int, _), res) -> begin
        match Mint.find_opt p s with
        | None -> res = None
        | Some (x :: _) -> res = Some x
        | _ -> false
      end *)
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn.Priority_queue" (module Spec) |> exit
