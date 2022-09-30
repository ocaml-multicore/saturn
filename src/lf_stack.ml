(** Trieber's Lock Free stack *)

type 'a node =
  | Nil
  | Next of 'a * 'a node Atomic.t

type 'a t =
  { head : 'a node Atomic.t ;
    tail : 'a node Atomic.t }

let create () =
  let head = (Next (Obj.magic (), Atomic.make Nil)) in
  { head = Atomic.make head ; tail = Atomic.make head }

let is_empty q =
  match Atomic.get q.head with
  | Nil -> failwith "Stack.is_empty: impossible"
  | Next (_,x) ->
     ( match Atomic.get x with
       | Nil -> true
       | _ -> false )

let push q v =
  let b = Backoff.create () in
  let rec loop () =
    let head = Atomic.get q.head in
    let newnode = Next (v, Atomic.make head) in
    if (Atomic.compare_and_set q.head head newnode) then ()
    else (Backoff.once b; loop ()) (* This can fail we need to loop *)
  in loop ()

let pop q =
  let b = Backoff.create () in
  let rec loop () =
    let s = Atomic.get q.head in
    let newhead = match s with
      | Nil -> failwith "Stack.pop: impossible"
      | Next (_, x) -> Atomic.get x
    in match newhead with
       | Nil -> None
       | Next (v, _) when Atomic.compare_and_set q.head s newhead -> Some v
       | _ -> (Backoff.once b; loop ())
  in loop ()