(** Sequential and Parallel model-based tests of bounded_queue *)

open QCheck
open STM
module Stack = Saturn.Stack

module Spec = struct
  type cmd =
    | Push of int
    | Push_all of int list
    | Pop_opt
    (* peek_exn and drop_exn use the same function as pop_exn*)
    | Pop_all
    | Peek_opt (* peek_exn uses the same function as peek_exn *)
    | To_seq
    | Is_empty

  let string_of_int_list l =
    "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Push_all l -> "Push_all " ^ string_of_int_list l
    (* | Push_all_exn l -> "Push_all_exn " ^ string_of_int_list l *)
    | Pop_opt -> "Pop_opt"
    | Pop_all -> "Pop_all"
    | Peek_opt -> "Peek_opt"
    | To_seq -> "To_seq"
    | Is_empty -> "Is_empty"

  type state = int list
  type sut = int Stack.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.map (fun l -> Push_all l) (Gen.list int_gen);
           Gen.return Pop_opt;
           Gen.return Pop_all;
           Gen.return Peek_opt;
           Gen.return To_seq;
           Gen.return Is_empty;
         ])

  let init_state = []
  let init_sut () = Stack.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push i -> i :: s
    | Push_all l -> List.rev l @ s
    (* | Push_all_exn l ->
        if List.length s + List.length l > capacity then s else List.rev l @ s *)
    | Pop_opt -> ( match s with [] -> s | _ :: s' -> s')
    | Pop_all -> []
    | Peek_opt -> s
    | To_seq -> s
    | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Stack.push d i)
    | Push_all l -> Res (unit, Stack.push_all d l)
    | Pop_opt -> Res (option int, Stack.pop_opt d)
    | Pop_all -> Res (list int, Stack.pop_all d)
    | Peek_opt -> Res (option int, Stack.peek_opt d)
    | To_seq -> Res (seq int, Stack.to_seq d)
    | Is_empty -> Res (bool, Stack.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), ()) -> true
    | Push_all _, Res ((Unit, _), _) -> true
    | (Pop_opt | Peek_opt), Res ((Option Int, _), res) -> (
        match s with [] -> res = None | j :: _ -> res = Some j)
    | Pop_all, Res ((List Int, _), res) -> res = s
    | To_seq, Res ((Seq Int, _), res) -> List.of_seq res = s
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn.Treiber_stack" (module Spec) |> exit
