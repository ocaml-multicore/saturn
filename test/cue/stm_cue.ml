(** Sequential and Parallel model-based tests of (bounded queue) cue. *)

open QCheck
open STM
module Cue = Saturn.Cue

module STM_cue (Cue : Cues.Cue_tests) = struct
  module Spec = struct
    type cmd =
      | Try_push of int
      | Pop_opt
      | Peek_opt
      | Drop_exn
      | Length
      | Is_empty
      | Is_full

    let show_cmd c =
      match c with
      | Try_push i -> "Try_push " ^ string_of_int i
      | Pop_opt -> "Pop_opt"
      | Peek_opt -> "Peek_opt"
      | Drop_exn -> "Drop_exn"
      | Length -> "Length"
      | Is_empty -> "Is_empty"
      | Is_full -> "Is_full"

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
             Gen.return Drop_exn;
             Gen.return Length;
             Gen.return Is_empty;
             Gen.return Is_full;
           ])

    let init_state = (100, 0, [])
    let init_sut () = Cue.create ~capacity:100 ()
    let cleanup _ = ()

    let next_state c ((capacity, size, content) as s) =
      match c with
      | Try_push i ->
          if size = capacity then s else (capacity, size + 1, i :: content)
      | Pop_opt | Drop_exn -> (
          match List.rev content with
          | [] -> s
          | _ :: content' -> (capacity, size - 1, List.rev content'))
      | Peek_opt -> s
      | Is_empty -> s
      | Is_full -> s
      | Length -> s

    let precond _ _ = true

    let run c d =
      match c with
      | Try_push i ->
          Res
            ( bool,
              match Cue.push_exn d i with
              | () -> true
              | exception Cue.Full -> true (*Cue.try_push d i*) )
      | Pop_opt -> Res (option int, Cue.pop_opt d)
      | Peek_opt -> Res (option int, Cue.peek_opt d)
      | Drop_exn -> Res (result unit exn, protect Cue.drop_exn d)
      | Is_empty -> Res (bool, Cue.is_empty d)
      | Is_full -> Res (bool, Cue.is_full d)
      | Length -> Res (int, Cue.length d)

    let postcond c ((capacity, size, content) : state) res =
      match (c, res) with
      | Try_push _, Res ((Bool, _), res) -> res = (size < capacity)
      | (Pop_opt | Peek_opt), Res ((Option Int, _), res) -> (
          match List.rev content with
          | [] -> res = None
          | j :: _ -> res = Some j)
      | Drop_exn, Res ((Result (Unit, Exn), _), res) -> (
          match List.rev content with
          | [] -> res = Error Cue.Empty
          | _ -> res = Ok ())
      | Is_empty, Res ((Bool, _), res) -> res = (content = [])
      | Is_full, Res ((Bool, _), res) -> res = (size = capacity)
      | Length, Res ((Int, _), res) -> res = size
      | _, _ -> false
  end

  let run () = Stm_run.run ~name:"Saturn_lockfree.Cue" (module Spec) |> exit
end

let () =
  (* Since Cue and Cue_unsafe share the same implementation, it is not necessary
     to test both of them. *)
  Random.self_init ();
  if Random.bool () then
    let module Safe = STM_cue (Cues.Cue) in
    Safe.run () |> exit
  else
    let module Unsafe = STM_cue (Cues.Cue_unsafe) in
    Unsafe.run () |> exit
