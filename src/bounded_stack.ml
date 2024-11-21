type 'a t = { head : (int * 'a list) Atomic.t; capacity : int }

(* *)
let create ?(capacity = Int.max_int) () =
  let head = Atomic.make_contended (0, []) in
  { head; capacity = max capacity 1 } |> Multicore_magic.copy_as_padded

let length t = fst (Atomic.get t.head)
let is_full t = t.capacity = length t
let is_empty t = Atomic.get t.head = (0, [])

exception Empty
exception Full

let of_list ?(capacity = Int.max_int) list =
  if capacity < List.length list then raise Full
  else
    let head = Atomic.make_contended (List.length list, List.rev list) in
    { head; capacity } |> Multicore_magic.copy_as_padded

let of_seq ?(capacity = Int.max_int) seq =
  if capacity < Seq.length seq then raise Full
  else
    let list = List.of_seq seq in
    let head = Atomic.make_contended (List.length list, List.rev list) in
    { head; capacity } |> Multicore_magic.copy_as_padded

(* *)

type ('a, _) poly1 = Option : ('a, 'a option) poly1 | Value : ('a, 'a) poly1

let peek_as : type a r. a t -> (a, r) poly1 -> r =
 fun t poly ->
  match Atomic.get t.head with
  | _, [] -> begin match poly with Option -> None | Value -> raise Empty end
  | _, value :: _ -> ( match poly with Option -> Some value | Value -> value)

let peek_exn t = peek_as t Value
let peek_opt t = peek_as t Option

type ('a, _) poly2 =
  | Option : ('a, 'a option) poly2
  | Value : ('a, 'a) poly2
  | Unit : ('a, unit) poly2

(* *)
let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly2 -> r =
 fun t backoff poly ->
  match Atomic.get t.head with
  | _, [] -> begin
      match poly with
      | Option -> None
      | Value -> raise Empty
      | Unit -> raise Empty
    end
  | (len, hd :: tl) as old_head ->
      if Atomic.compare_and_set t.head old_head (len - 1, tl) then
        match poly with Option -> Some hd | Value -> hd | Unit -> ()
      else pop_as t (Backoff.once backoff) poly

let pop_exn t = pop_as t Backoff.default Value
let pop_opt t = pop_as t Backoff.default Option
let drop_exn t = pop_as t Backoff.default Unit

let rec pop_all t backoff =
  match Atomic.get t.head with
  | _, [] -> []
  | (_, values) as old_head ->
      if Atomic.compare_and_set t.head old_head (0, []) then values
      else pop_all t (Backoff.once backoff)

let pop_all t = pop_all t Backoff.default

let to_seq t =
  match Atomic.get t.head with
  | _, [] -> Seq.empty
  | _, values -> List.to_seq values
(* *)

type _ mono = Unit : unit mono | Bool : bool mono

let rec push_as : type r. 'a t -> Backoff.t -> 'a -> r mono -> r =
 fun t backoff value mono ->
  match Atomic.get t.head with
  | (_, []) as old_head ->
      if Atomic.compare_and_set t.head old_head @@ (1, [ value ]) then
        match mono with Bool -> true | Unit -> ()
      else push_as t (Backoff.once backoff) value mono
  | (len, values) as old_head ->
      if len >= t.capacity then
        match mono with Bool -> false | Unit -> raise Full
      else
        let new_head = (len + 1, value :: values) in

        if Atomic.compare_and_set t.head old_head new_head then
          match mono with Bool -> true | Unit -> ()
        else push_as t (Backoff.once backoff) value mono

let push_exn t value = push_as t Backoff.default value Unit
let try_push t value = push_as t Backoff.default value Bool

let rec push_all_as : type r. 'a t -> Backoff.t -> 'a list -> r mono -> r =
 fun t backoff values mono ->
  let len = List.length values in
  if len = 0 then match mono with Unit -> () | Bool -> true
  else if len > t.capacity then
    match mono with Unit -> raise Full | Bool -> false
  else
    match Atomic.get t.head with
    | (_, []) as old_head ->
        if Atomic.compare_and_set t.head old_head (List.length values, values)
        then match mono with Bool -> true | Unit -> ()
        else push_all_as t (Backoff.once backoff) values mono
    | (curr_len, prev_values) as old_head ->
        if curr_len + len > t.capacity then
          match mono with Bool -> false | Unit -> raise Full
        else if
          Atomic.compare_and_set t.head old_head
            (curr_len + len, values @ prev_values)
        then match mono with Bool -> true | Unit -> ()
        else push_all_as t (Backoff.once backoff) values mono

let try_push_all t values = push_all_as t Backoff.default (List.rev values) Bool
let push_all_exn t values = push_all_as t Backoff.default (List.rev values) Unit
let add_seq_exn t seq = push_all_as t Backoff.default (List.of_seq seq) Unit
let try_add_seq t seq = push_all_as t Backoff.default (List.of_seq seq) Bool
