(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  type 'a t;;
  (** The type of lock-free work stealing queue. *)

  val create : unit -> 'a t;;
  (** Create a new queue, which is initially empty. *)

  val is_empty : 'a t -> bool;;
  (** [is_empty q] returns empty if [q] is empty. *)

  val size : 'a t -> int;;
  (** [size q] returns the number of elements currently stored in the queue. *)

  val push : 'a t -> 'a -> unit;;
  (** [push q v] pushes [v] to the back of the queue.
      It should only be called by thread which owns the queue [q]. *)

  val pop : 'a t -> 'a option;;
  (** [pop q] pops an element [e] from the front of the queue and returns
      [Some v] if the queue is non-empty. Otherwise, returns [None].
      It should only be called by thread which owns the queue [q]. *)

  val steal : 'a t -> 'a option;;
  (** [steal q] steal an element from the back of the queue and returns
      [Some v] if the queue is non-empty. Otherwise, returns [None].
      It should only be called by thread which doesn't own the queue [q].*)
end;;

module M : S;;
