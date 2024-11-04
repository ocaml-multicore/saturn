(** Treiber's Lock Free stack *)

type 'a node = Nil | Cons of { value : 'a; tail : 'a node }
type 'a t = 'a node Atomic.t

let create () = Atomic.make Nil |> Multicore_magic.copy_as_padded
let is_empty t = Atomic.get t == Nil

let rec push t value backoff =
  let tail = Atomic.get t in
  let cons = Cons { value; tail } in
  if not (Atomic.compare_and_set t tail cons) then
    push t value (Backoff.once backoff)

let push t value = push t value Backoff.default

exception Empty

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly -> r =
 fun t backoff poly ->
  match Atomic.get t with
  | Nil -> begin match poly with Option -> None | Value -> raise Empty end
  | Cons cons_r as cons ->
      if Atomic.compare_and_set t cons cons_r.tail then
        match poly with Option -> Some cons_r.value | Value -> cons_r.value
      else pop_as t (Backoff.once backoff) poly

let pop t = pop_as t Backoff.default Value
let pop_opt t = pop_as t Backoff.default Option
