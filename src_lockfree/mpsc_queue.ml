exception Closed
exception Empty

(* A list whose the end indicates the queue is open or closed. *)
type 'a clist = ( :: ) of 'a * 'a clist | Open | Closed

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
type 'a t = { head : 'a clist ref; tail : 'a clist Atomic.t }

let create () =
  let tail = Multicore_magic.copy_as_padded @@ Atomic.make Open in
  let head = Multicore_magic.copy_as_padded @@ ref Open in
  Multicore_magic.copy_as_padded { tail; head }

(* can be used by all Consumers and Producers *)
let rec push t x =
  match Atomic.get t.tail with
  | Closed -> raise Closed
  | before ->
      let after = x :: before in
      if not (Atomic.compare_and_set t.tail before after) then push t x

let push_head t x =
  match !(t.head) with
  | Closed -> raise Closed (*when head is closed and empty*)
  | before -> t.head := x :: before

(* Only by the cosumers *)
let rec pop t =
  match !(t.head) with
  | x :: xs ->
      t.head := xs;
      x
  | Closed -> raise Closed (*implies closed and empty*)
  | Open -> (
      (* We know the tail is open because we just saw the head was open *)
      (* we cannot run concurrently with [close] *)
      match Atomic.exchange t.tail Open with
      | Closed ->
          failwith
            "This cannot happen, maybe you are running close concurrently with \
             pop"
      | Open -> raise Empty
      | tail ->
          t.head := rev_append tail Open;
          pop t)

(* to be closed only by the consumers since it alters the head *)
let close t =
  match Atomic.exchange t.tail Closed with
  | Closed -> raise Closed (*already closed*)
  | xs -> t.head := !(t.head) @ rev_append xs Closed

let rec pop_opt t =
  match !(t.head) with
  | x :: xs ->
      t.head := xs;
      Some x
  | Closed -> raise Closed
  | Open -> (
      (* We know the tail is open because we just saw the head was open
         and we don't run concurrently with [close]. *)
      (* assert false but close can be run with MP *)
      match Atomic.exchange t.tail Open with
      | Closed ->
          failwith
            "This cannot happen, maybe you are running close concurrently with \
             pop_opt"
      | Open -> None
      | tail ->
          t.head := rev_append tail Open;
          pop_opt t)

(* can be called concurrently with close. Might result in incorrect result if it is called concurrently with push or pop *)
let rec peek t =
  match !(t.head) with
  | x :: _ -> x
  | Closed -> raise Closed
  | Open -> (
      match Atomic.get t.tail with
      | Open -> raise Empty
      | Closed ->
          peek t
          (*might be called concurrently with closed so calling peek again to recheck - or is a failwith message better?*)
      | tail -> (
          match rev_append tail Open with
          | x :: _ -> x
          | Open -> assert false
          | Closed -> assert false))

let rec peek_opt t =
  match !(t.head) with
  | x :: _ -> Some x
  | Closed -> raise Closed
  | Open -> (
      match Atomic.get t.tail with
      | Open -> None
      | Closed ->
          peek_opt t
          (*might be called concurrently with closed so calling peek again to recheck - or is a failwith message better?*)
      | tail -> (
          match rev_append tail Open with
          | x :: _ -> Some x
          | Open -> assert false
          | Closed -> assert false))

let is_empty t =
  match !(t.head) with
  | _ :: _ -> false
  | Closed -> raise Closed (*closed and empty*)
  | Open -> (
      match Atomic.get t.tail with
      | _ :: _ -> false
      | Open -> (
          match !(t.head) with
          | Open -> true
          | _ :: _ -> false
          | Closed ->
              failwith
                "This cannot happen, maybe you are running close concurrently \
                 with [close]")
      | Closed ->
          failwith
            "This cannot happen, maybe you are running close concurrently with \
             [close]")
(* Incorrect reading of t.head if a pop function is called concurrently - false negative*)
(* Incorrect reading of t.tail if push is called concurrently - false positive*)
