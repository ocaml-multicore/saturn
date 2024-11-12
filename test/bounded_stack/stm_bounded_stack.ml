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
    | Try_compare_and_pop of int
    | Try_compare_and_set of int * int
    | Set_exn of int
    (* try_set uses the same function as set_exn *)
    | To_seq
    | Is_empty
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
    | Try_compare_and_pop i -> "Try_compare_and_pop " ^ string_of_int i
    | Try_compare_and_set (i, j) ->
        "Try_compare_and_set (" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")"
    | To_seq -> "To_seq"
    | Set_exn i -> "Try_set " ^ string_of_int i
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
           Gen.map (fun l -> Try_push_all l) (Gen.list int_gen);
           Gen.return Pop_opt;
           Gen.return Pop_all;
           Gen.return Peek_opt;
           Gen.map (fun i -> Try_compare_and_pop i) int_gen;
           Gen.map2 (fun i j -> Try_compare_and_set (i, j)) int_gen int_gen;
           Gen.map (fun i -> Set_exn i) int_gen;
           Gen.return To_seq;
           Gen.return Is_empty;
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
    | Try_compare_and_pop i -> (
        match s with [] -> [] | hd :: tl -> if hd = i then tl else s)
    | Try_compare_and_set (i, j) -> (
        match s with [] -> [] | hd :: tl -> if hd = i then j :: tl else s)
    | Set_exn i -> ( match s with [] -> s | _ :: tl -> i :: tl)
    | To_seq -> s
    | Is_empty -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Try_push i -> Res (bool, Stack.try_push d i)
    | Try_push_all l -> Res (bool, Stack.try_push_all d l)
    | Pop_opt -> Res (option int, Stack.pop_opt d)
    | Pop_all -> Res (list int, Stack.pop_all d)
    | Peek_opt -> Res (option int, Stack.peek_opt d)
    | Try_compare_and_pop i -> Res (bool, Stack.try_compare_and_pop d i)
    | Try_compare_and_set (i, j) -> Res (bool, Stack.try_compare_and_set d i j)
    | Set_exn i -> Res (result int exn, protect (fun d -> Stack.set_exn d i) d)
    | To_seq -> Res (seq int, Stack.to_seq d)
    | Is_empty -> Res (bool, Stack.is_empty d)
    | Length -> Res (int, Stack.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_push _, Res ((Bool, _), res) -> List.length s < capacity = res
    | Try_push_all l, Res ((Bool, _), res) ->
        List.length s + List.length l <= capacity = res
    | (Pop_opt | Peek_opt), Res ((Option Int, _), res) -> (
        match s with [] -> res = None | j :: _ -> res = Some j)
    | Try_compare_and_pop i, Res ((Bool, _), res) -> (
        match s with [] -> res = false | hd :: _ -> res = (hd = i))
    | Try_compare_and_set (i, _), Res ((Bool, _), res) -> (
        match s with [] -> res = false | hd :: _ -> res = (hd = i))
    | Set_exn _, Res ((Result (Int, Exn), _), res) -> (
        match s with [] -> res = Error Stack.Empty | x :: _ -> res = Ok x)
    | Pop_all, Res ((List Int, _), res) -> res = s
    | To_seq, Res ((Seq Int, _), res) -> List.of_seq res = s
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | Length, Res ((Int, _), res) -> res = List.length s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn.Bounded_stack" (module Spec) |> exit
