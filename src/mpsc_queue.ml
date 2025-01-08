(* A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
   This makes a good data structure for a scheduler's run queue.

   Based on Vesa Karvonen's example at:
   https://github.com/ocaml-multicore/picos/blob/07d6c2d391e076b490098c0379d01208b3a9cc96/test/lib/foundation/mpsc_queue.ml
*)

exception Closed
exception Empty

(* A list where the end indicates whether the queue is closed. *)
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

(* *)

(* The queue contains [head @ rev tail].
   If [tail] is non-empty then it ends in [Open]. *)
type 'a t = { mutable head : 'a clist; tail : 'a clist Atomic.t }

let create () = { head = Open; tail = Atomic.make_contended Open }

let[@tail_mod_cons] rec append_list_to_clist l l' =
  match l with [] -> l' | List.(x :: xs) -> x :: append_list_to_clist xs l'

let of_list l =
  { head = append_list_to_clist l Open; tail = Atomic.make_contended Open }

(* *)

let is_empty t =
  match t.head with
  | _ :: _ -> false
  | Closed -> raise Closed
  | Open -> ( match Atomic.get t.tail with _ :: _ -> false | _ -> true)

let close t =
  match Atomic.exchange t.tail Closed with
  | Closed -> raise Closed
  | xs -> t.head <- t.head @ rev_append xs Closed

(* *)

let rec push t x =
  match Atomic.get t.tail with
  | Closed -> raise Closed
  | before ->
      let after = x :: before in
      if not (Atomic.compare_and_set t.tail before after) then push t x

let push_head t x =
  match t.head with Closed -> raise Closed | before -> t.head <- x :: before

let[@tail_mod_cons] rec append_list_to_clist l l' =
  match l with [] -> l' | List.(x :: xs) -> x :: append_list_to_clist xs l'

let rec push_all t values =
  match Atomic.get t.tail with
  | Closed -> raise Closed
  | before ->
      let after = append_list_to_clist (List.rev values) before in
      if not (Atomic.compare_and_set t.tail before after) then push_all t values

(* *)

type ('a, _) poly =
  | Option : ('a, 'a option) poly
  | Value : ('a, 'a) poly
  | Unit : ('a, unit) poly

let rec pop_as : type a r. a t -> (a, r) poly -> r =
 fun t poly ->
  match t.head with
  | x :: xs -> begin
      t.head <- xs;
      match poly with Option -> Some x | Value -> x | Unit -> ()
    end
  | Closed -> raise Closed
  | Open -> (
      (* We know the tail is open because we just saw the head was open
         and we don't run concurrently with [close]. *)
      match Atomic.exchange t.tail Open with
      | Closed -> assert false
      | Open -> (
          match poly with
          | Option -> None
          | Value | Unit -> raise Empty (* Optimise the common case *))
      | tail ->
          t.head <- rev_append tail Open;
          pop_as t poly)

(* *)

type ('a, _) poly2 = Option : ('a, 'a option) poly2 | Value : ('a, 'a) poly2

let rec peek_as : type a r. a t -> (a, r) poly2 -> r =
 fun t poly ->
  match t.head with
  | x :: _ -> ( match poly with Option -> Some x | Value -> x)
  | Closed -> raise Closed
  | Open -> (
      (* We know the tail is open because we just saw the head was open
         and we don't run concurrently with [close]. *)
      match Atomic.exchange t.tail Open with
      | Closed -> assert false
      | Open -> ( match poly with Option -> None | Value -> raise Empty)
      | tail ->
          t.head <- rev_append tail Open;
          peek_as t poly)

(* *)

let pop_opt t = pop_as t Option
let pop_exn t = pop_as t Value
let drop_exn t = pop_as t Unit
let peek_exn t = peek_as t Value
let peek_opt t = peek_as t Option
