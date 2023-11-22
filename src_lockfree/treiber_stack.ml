(** Treiber's Lock Free stack *)

type 'a node = Nil | Next of 'a * 'a node
type 'a t = { head : 'a node Atomic.t }

let create () =
  let head = Nil in
  { head = Atomic.make head }

let is_empty q = match Atomic.get q.head with Nil -> true | Next _ -> false

let rec push backoff q v =
  let head = Atomic.get q.head in
  let new_node = Next (v, head) in
  if Atomic.compare_and_set q.head head new_node then ()
  else
    let backoff = Backoff.once backoff in
    push backoff q v

let push q v = push Backoff.default q v

exception Empty

let rec pop backoff q =
  let s = Atomic.get q.head in
  match s with
  | Nil -> raise Empty
  | Next (v, next) ->
      if Atomic.compare_and_set q.head s next then v
      else
        let backoff = Backoff.once backoff in
        pop backoff q

let pop q = pop Backoff.default q

let rec pop_opt backoff q =
  let s = Atomic.get q.head in
  match s with
  | Nil -> None
  | Next (v, next) ->
      if Atomic.compare_and_set q.head s next then Some v
      else
        let backoff = Backoff.once backoff in
        pop_opt backoff q

let pop_opt q = pop_opt Backoff.default q
