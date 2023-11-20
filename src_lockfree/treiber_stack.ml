(** Treiber's Lock Free stack *)

type 'a node = Nil | Next of 'a * 'a node
type 'a t = { head : 'a node Atomic.t }

let create () =
  let head = Nil in
  { head = Atomic.make head }

let is_empty q = match Atomic.get q.head with Nil -> true | Next _ -> false

let push q v =
  let head = Atomic.get q.head in
  let new_node = Next (v, head) in
  if Atomic.compare_and_set q.head head new_node then ()
  else
    let b = Backoff.create () |> Backoff.once in

    (* retry *)
    let rec loop b =
      let head = Atomic.get q.head in
      let new_node = Next (v, head) in
      if Atomic.compare_and_set q.head head new_node then ()
      else
        let b = Backoff.once b in
        loop b
    in
    loop b

exception Empty

let pop q =
  let rec loop b =
    let s = Atomic.get q.head in
    match s with
    | Nil -> raise Empty
    | Next (v, next) ->
        if Atomic.compare_and_set q.head s next then v
        else
          let b = Backoff.once b in
          loop b
  in

  let s = Atomic.get q.head in
  match s with
  | Nil -> raise Empty
  | Next (v, next) ->
      if Atomic.compare_and_set q.head s next then v
      else
        let b = Backoff.create () |> Backoff.once in
        loop b

let pop_opt q =
  let rec loop b =
    let s = Atomic.get q.head in
    match s with
    | Nil -> None
    | Next (v, next) ->
        if Atomic.compare_and_set q.head s next then Some v
        else
          let b = Backoff.once b in
          loop b
  in

  let s = Atomic.get q.head in
  match s with
  | Nil -> None
  | Next (v, next) ->
      if Atomic.compare_and_set q.head s next then Some v
      else
        let b = Backoff.create () |> Backoff.once in
        loop b
