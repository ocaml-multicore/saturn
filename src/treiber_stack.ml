(** Trieber's Lock Free stack *)

type 'a node = Nil | Next of 'a * 'a node Atomic.t
type 'a t = { head : 'a node Atomic.t }

let create () =
  let head = Nil in
  { head = Atomic.make head }

let is_empty q = match Atomic.get q.head with Nil -> true | Next _ -> false

let push q v =
  let rec loop b () =
    let head = Atomic.get q.head in
    let newnode = Next (v, Atomic.make head) in
    if Atomic.compare_and_set q.head head newnode then ()
    else (
      Backoff.once b;
      loop b ())
    (* This can fail we need to loop *)
  in

  let head = Atomic.get q.head in
  let newnode = Next (v, Atomic.make head) in
  if Atomic.compare_and_set q.head head newnode then ()
  else
    let b = Backoff.create () in
    Backoff.once b;
    loop b ()

let pop q =
  let rec loop b () =
    let s = Atomic.get q.head in
    match s with
    | Nil -> None
    | Next (v, next) ->
        if Atomic.compare_and_set q.head s (Atomic.get next) then Some v
        else (
          Backoff.once b;
          loop b ())
  in

  let s = Atomic.get q.head in
  match s with
  | Nil -> None
  | Next (v, next) ->
      if Atomic.compare_and_set q.head s (Atomic.get next) then Some v
      else
        let b = Backoff.create () in
        Backoff.once b;
        loop b ()
