(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  type 'a t;;
  (** The type of lock-free bag. *)

  val create : unit -> 'a t;;
  (** Create a new bag, which is initially empty. *)

  val is_empty : 'a t -> bool;;
  (** [is_empty b] returns empty if [b] is empty. *)

  val push : 'a t -> 'a -> unit;;
  (** [push b v] pushes [v] into the bag. *)

  val pop : 'a t -> 'a option;;
  (** [pop b] pops an element [e] from the bag and returns
      [Some v] if the bag is non-empty. Otherwise, returns [None]. *)

(*  val clean_until : 'a t -> ('a -> bool) -> unit;;
  (** [clean_until q f] drops the prefix of the queue until the element [e],
      where [f e] is [true]. If no such element exists, then the queue is
      emptied. *)

  type 'a cursor;;
  (** The type of cursor. *)

  val snapshot  : 'a t -> 'a cursor;;
  (** Obtain a snapshot of the queue. This is a constant time operation. *)

  val next      : 'a cursor -> ('a * 'a cursor) option;;
  (** [next c] returns [Some (e, c')] where [e] is the head of the queue and
      [c'] is the tail, if the queue is non-empty. Otherwise, returns [None]. *)
*)
end;;

module M : S;;
