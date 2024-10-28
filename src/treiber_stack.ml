(** Treiber's Lock Free stack *)

type 'a node = Nil | Cons of { value : 'a; tail : 'a node }
type 'a t = 'a node Atomic.t

let create () = Atomic.make_contended Nil
let is_empty t = Atomic.get t == Nil

let of_list list =
  List.fold_left (fun acc elt -> Cons { value = elt; tail = acc }) Nil list
  |> Atomic.make_contended

let of_seq seq =
  Seq.fold_left (fun acc elt -> Cons { value = elt; tail = acc }) Nil seq
  |> Atomic.make_contended

(* *)

exception Empty

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let peek_as : type a r. a t -> (a, r) poly -> r =
 fun t poly ->
  match Atomic.get t with
  | Nil -> begin
      match poly with Option -> None | Value -> raise_notrace Empty
    end
  | Cons cons -> (
      match poly with Option -> Some cons.value | Value -> cons.value)

let peek_exn t = peek_as t Value
let peek_opt t = peek_as t Option

type ('a, _) poly2 =
  | Option : ('a, 'a option) poly2
  | Value : ('a, 'a) poly2
  | Unit : ('a, unit) poly2

let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly2 -> r =
 fun t backoff poly ->
  match Atomic.get t with
  | Nil -> begin
      match poly with Option -> None | Value | Unit -> raise_notrace Empty
    end
  | Cons cons_r as cons ->
      if Atomic.compare_and_set t cons cons_r.tail then
        match poly with
        | Option -> Some cons_r.value
        | Value -> cons_r.value
        | Unit -> ()
      else pop_as t (Backoff.once backoff) poly

let pop_exn t = pop_as t Backoff.default Value
let pop_opt t = pop_as t Backoff.default Option
let drop_exn t = pop_as t Backoff.default Unit

let rec pop_all t backoff =
  match Atomic.get t with
  | Nil -> []
  | old_head ->
      if Atomic.compare_and_set t old_head Nil then
        let[@tail_mod_cons] rec aux = function
          | Nil -> []
          | Cons cons -> cons.value :: aux cons.tail
        in
        aux old_head
      else pop_all t (Backoff.once backoff)

let pop_all t = pop_all t Backoff.default

let to_seq t =
  match Atomic.get t with
  | Nil -> Seq.empty
  | old_head ->
      let rec aux s () =
        match s with
        | Nil -> Seq.Nil
        | Cons cons -> Seq.Cons (cons.value, aux cons.tail)
      in
      aux old_head
(* *)

let rec push t value backoff =
  let tail = Atomic.get t in
  let cons = Cons { value; tail } in
  if not (Atomic.compare_and_set t tail cons) then
    push t value (Backoff.once backoff)

let push t value = push t value Backoff.default

(**)

type ('a, _) poly3 = Value : ('a, 'a) poly3 | Bool : ('a, bool) poly3

let rec set_as : type v r. v t -> v -> Backoff.t -> (v, r) poly3 -> r =
 fun t value backoff poly ->
  match Atomic.get t with
  | Nil -> ( match poly with Value -> raise_notrace Empty | Bool -> false)
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t old_head @@ Cons { cons_r with value } then
        match poly with Value -> cons_r.value | Bool -> true
      else set_as t value (Backoff.once backoff) poly

let set_exn t value = set_as t value Backoff.default Value
let try_set t value = set_as t value Backoff.default Bool

(**)

let rec push_all_ t backoff values =
  let rec build_node acc = function
    | [] -> acc
    | x :: xs -> build_node (Cons { tail = acc; value = x }) xs
  in
  match Atomic.get t with
  | Nil ->
      if Atomic.compare_and_set t Nil (build_node Nil values) then ()
      else push_all_ t (Backoff.once backoff) values
  | Cons _ as old_head ->
      if Atomic.compare_and_set t old_head @@ build_node old_head values then ()
      else push_all_ t (Backoff.once backoff) values

let push_all t values =
  match values with [] -> () | _ -> push_all_ t Backoff.default values

let add_seq t seq = push_all_ t Backoff.default (List.of_seq seq)

(* *)

type op = Set | Pop

let try_compare_and_ t old_value new_value op =
  let rec aux backoff =
    match Atomic.get t with
    | Nil -> false
    | Cons cons_r as old_head ->
        if cons_r.value == old_value then
          if
            Atomic.compare_and_set t old_head
            @@
            match op with
            | Set -> Cons { cons_r with value = new_value }
            | Pop -> cons_r.tail
          then true
          else aux (Backoff.once backoff)
        else false
  in
  aux Backoff.default

let try_compare_and_pop t value = try_compare_and_ t value value Pop

let try_compare_and_set t old_value new_value =
  try_compare_and_ t old_value new_value Set
