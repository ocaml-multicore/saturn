(** Sequential and Parallel model-based tests of bounded_queue *)

open QCheck
open STM
module Stack = Saturn.Bounded_stack

module Spec = struct
  type cmd =
    | Try_push of int
    (* push_exn uses the same function as try_push *)
    | Try_push_all of int list
    (* push_all_exn and add_seq_exn and try_add_seq uses the same function as
       try_push_all*)
    | Pop_opt
    (* peek_exn and drop_exn use the same function as pop_exn*)
    | Pop_all
    | Peek_opt
    (* peek_exn uses the same function as peek_exn *)
    | To_seq
    | Is_empty
    | Is_full
    | Length

  let string_of_int_list l =
    "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"

  let show_cmd c =
    match c with
    | Try_push i -> "Try_push " ^ string_of_int i
    | Try_push_all l -> "Try_push_all " ^ string_of_int_list l
    (* | Push_all_exn l -> "Push_all_exn " ^ string_of_int_list l *)
    | Pop_opt -> "Pop_opt"
    | Pop_all -> "Pop_all"
    | Peek_opt -> "Peek_opt"
    | To_seq -> "To_seq"
    | Is_empty -> "Is_empty"
    | Is_full -> "Is_full"
    | Length -> "Length"

  type state = int list
  type sut = int Stack.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Try_push i) int_gen;
           Gen.map (fun l -> Try_push_all l) (Gen.list int_gen);
           Gen.return Pop_opt;
           Gen.return Pop_all;
           Gen.return Peek_opt;
           Gen.return To_seq;
           Gen.return Is_empty;
           Gen.return Is_full;
           Gen.return Length;
         ])

  let init_state = []
  let capacity = 8
  let init_sut () = Stack.create ~capacity ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Try_push i -> if List.length s >= capacity then s else i :: s
    | Try_push_all l ->
        if List.length s + List.length l > capacity then s else List.rev l @ s
    (* | Push_all_exn l ->
        if List.length s + List.length l > capacity then s else List.rev l @ s *)
    | Pop_opt -> ( match s with [] -> s | _ :: s' -> s')
    | Pop_all -> []
    | Peek_opt -> s
    | To_seq -> s
    | Is_empty -> s
    | Is_full -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Try_push i -> Res (bool, Stack.try_push d i)
    | Try_push_all l -> Res (bool, Stack.try_push_all d l)
    | Pop_opt -> Res (option int, Stack.pop_opt d)
    | Pop_all -> Res (list int, Stack.pop_all d)
    | Peek_opt -> Res (option int, Stack.peek_opt d)
    | To_seq -> Res (seq int, Stack.to_seq d)
    | Is_empty -> Res (bool, Stack.is_empty d)
    | Is_full -> Res (bool, Stack.is_full d)
    | Length -> Res (int, Stack.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_push _, Res ((Bool, _), res) -> List.length s < capacity = res
    | Try_push_all l, Res ((Bool, _), res) ->
        List.length s + List.length l <= capacity = res
    | (Pop_opt | Peek_opt), Res ((Option Int, _), res) -> (
        match s with [] -> res = None | j :: _ -> res = Some j)
    | Pop_all, Res ((List Int, _), res) -> res = s
    | To_seq, Res ((Seq Int, _), res) -> List.of_seq res = s
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | Is_full, Res ((Bool, _), res) -> res = (List.length s = capacity)
    | Length, Res ((Int, _), res) -> res = List.length s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn.Bounded_stack" (module Spec) |> exit
