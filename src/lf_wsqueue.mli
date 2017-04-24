(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  type 'a t;;
  (** The type of lock-free queue. *)

  val create : 'a -> 'a t;;
  (** Create a new queue, which is initially empty. *)

  val is_empty : 'a t -> bool;;
  (** [is_empty q] returns empty if [q] is empty. *)

  val size : 'a t -> int;;

  val push : 'a t -> 'a -> unit;;
  (** [push q v] pushes [v] to the back of the queue. *)

  val pop : 'a t -> 'a option;;
  (** [pop q] pops an element [e] from the front of the queue and returns
      [Some v] if the queue is non-empty. Otherwise, returns [None]. *)

  val steal : 'a t -> 'a option;;
end;;

module M : S;;
