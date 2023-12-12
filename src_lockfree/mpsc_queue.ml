exception Closed
exception Empty
(* A list whose the end indicates the queue is open or closed. *)
type 'a clist =
  | (::) of 'a * 'a clist
  | Open
  | Closed

(* [rev_append l1 l2] is like [rev l1 @ l2] *)
let rec rev_append l1 l2 =
  match l1 with
  | a :: l -> rev_append l (a :: l2)
  | Open -> l2
  | Closed -> assert false
    
let[@tail_mod_cons] rec ( @ ) l1 l2 =
  match l1 with
  | h1 :: tl -> h1 :: (tl @ l2)
  | Open -> l2
  | Closed -> assert false

(* The queue contains [head @ rev tail].
   If [tail] is non-empty then it ends in [Open]. *)
type 'a t = {
  mutable head : 'a clist;
  tail : 'a clist Atomic.t;
}

let rec push t x =
  match Atomic.get t.tail with
  | Closed -> raise Closed
  | before ->
    let after = x :: before in
    if not (Atomic.compare_and_set t.tail before after) then
      push t x

let push_head t x =
  match t.head with
  | Closed -> raise Closed
  | before -> t.head <- x :: before

let rec pop t =
  match t.head with
  | x :: xs -> t.head <- xs; x
  | Closed -> raise Closed
  | Open ->
    (* We know the tail is open because we just saw the head was open *)
    (* we can run concurrently with close *)
    match Atomic.exchange t.tail Open with
    | Closed -> pop t
    | Open -> raise Empty
    | tail ->
      t.head <- rev_append tail Open;
      pop t

let rec pop_opt t = 
  match t.head with
  | x :: xs -> t.head <- xs; Some x
  | Closed -> raise Closed
  | Open ->
    (* We know the tail is open because we just saw the head was open
       and we don't run concurrently with [close]. *)
    match Atomic.exchange t.tail Open with
    | Closed -> pop_opt t
    | Open -> None
    | tail ->
      t.head <- rev_append tail Open;
      pop_opt t

let peek t =
  match t.head with
  | x :: _ -> x
  | Closed -> raise Closed
  | Open ->
    match Atomic.get t.tail with
    | Closed -> assert false
    | Open -> raise Empty
    | tail -> begin 
      match rev_append tail Open with
      | x::_ -> x
      | Open -> assert false
      | Closed -> assert false
    end

let peek_opt t =
  match t.head with
  | x :: _ -> Some x
  | Closed -> raise Closed
  | Open ->
    match Atomic.get t.tail with
    | Closed -> assert false
    | Open -> None
    | tail -> begin 
      match rev_append tail Open with
      | x::_ -> Some x
      | Open -> assert false
      | Closed -> assert false
    end

let close t =
  match Atomic.exchange t.tail Closed with
  | Closed -> raise Closed
  | xs -> t.head <- t.head @ rev_append xs Closed

let rec is_empty t =
  match t.head with
  | _ :: _ -> false
  | Closed -> raise Closed
  | Open ->
    match Atomic.get t.tail with
    | _ :: _ -> false
    | Closed -> is_empty t
    | Open -> true

(* let create () = {
  head = Open;
  tail = Atomic.make Open;
} *)

let create () =
  let tail = Multicore_magic.copy_as_padded @@ Atomic.make Open in
  let head = Multicore_magic.copy_as_padded @@ Open in
  Multicore_magic.copy_as_padded { tail; head }