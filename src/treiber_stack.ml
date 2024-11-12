(** Treiber's Lock Free stack *)

type 'a t = 'a list Atomic.t

let create () = Atomic.make_contended []
let is_empty t = Atomic.get t == []
let of_list list = Atomic.make_contended list
let of_seq seq = Atomic.make_contended (List.of_seq seq)

(* *)

exception Empty

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let peek_as : type a r. a t -> (a, r) poly -> r =
 fun t poly ->
  match Atomic.get t with
  | [] -> begin match poly with Option -> None | Value -> raise Empty end
  | hd :: _ -> ( match poly with Option -> Some hd | Value -> hd)

let peek_exn t = peek_as t Value
let peek_opt t = peek_as t Option

type ('a, _) poly2 =
  | Option : ('a, 'a option) poly2
  | Value : ('a, 'a) poly2
  | Unit : ('a, unit) poly2

let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly2 -> r =
 fun t backoff poly ->
  match Atomic.get t with
  | [] -> begin
      match poly with Option -> None | Value | Unit -> raise Empty
    end
  | hd :: tail as before ->
      if Atomic.compare_and_set t before tail then
        match poly with Option -> Some hd | Value -> hd | Unit -> ()
      else pop_as t (Backoff.once backoff) poly

let pop_exn t = pop_as t Backoff.default Value
let pop_opt t = pop_as t Backoff.default Option
let drop_exn t = pop_as t Backoff.default Unit

let rec pop_all t backoff =
  match Atomic.get t with
  | [] -> []
  | old_head ->
      if Atomic.compare_and_set t old_head [] then old_head
      else pop_all t (Backoff.once backoff)

let pop_all t = pop_all t Backoff.default

let to_seq t =
  match Atomic.get t with [] -> Seq.empty | old_head -> List.to_seq old_head
(* *)

let rec push t value backoff =
  let before = Atomic.get t in
  let after = value :: before in
  if not (Atomic.compare_and_set t before after) then
    push t value (Backoff.once backoff)

let push t value = push t value Backoff.default

(**)

type ('a, _) poly3 = Value : ('a, 'a) poly3 | Bool : ('a, bool) poly3

let rec set_as : type v r. v t -> v -> Backoff.t -> (v, r) poly3 -> r =
 fun t value backoff poly ->
  match Atomic.get t with
  | [] -> ( match poly with Value -> raise Empty | Bool -> false)
  | hd :: tail as old_head ->
      if Atomic.compare_and_set t old_head @@ (value :: tail) then
        match poly with Value -> hd | Bool -> true
      else set_as t value (Backoff.once backoff) poly

let set_exn t value = set_as t value Backoff.default Value
let try_set t value = set_as t value Backoff.default Bool

(**)

let rec push_all_ t backoff values =
  match Atomic.get t with
  | [] ->
      if Atomic.compare_and_set t [] values then ()
      else push_all_ t (Backoff.once backoff) values
  | _ as old_head ->
      if Atomic.compare_and_set t old_head (values @ old_head) then ()
      else push_all_ t (Backoff.once backoff) values

let push_all t values =
  match values with
  | [] -> ()
  | _ -> push_all_ t Backoff.default (List.rev values)

let add_seq t seq = push_all_ t Backoff.default (List.of_seq seq |> List.rev)

(* *)

type op = Set | Pop

let try_compare_and_ t old_value new_value op =
  let rec aux backoff =
    match Atomic.get t with
    | [] -> false
    | hd :: tl as old_head ->
        if hd == old_value then
          if
            Atomic.compare_and_set t old_head
            @@ match op with Set -> new_value :: tl | Pop -> tl
          then true
          else aux (Backoff.once backoff)
        else false
  in
  aux Backoff.default

let try_compare_and_pop t value = try_compare_and_ t value value Pop

let try_compare_and_set t old_value new_value =
  try_compare_and_ t old_value new_value Set
