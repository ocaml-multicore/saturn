(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  (** The type of lock-free work stealing queue. *)
  type 'a t;;

  (** Create a new queue, which is initially empty. *)
  val create : unit -> 'a t;;

  (** [is_empty q] returns empty if [q] is empty. *)
  val is_empty : 'a t -> bool;;

  (** [size q] returns the number of elements currently stored in the queue. *)
  val size : 'a t -> int;;

  (** [push q v] pushes [v] to the back of the queue.
      It should only be called by thread which owns the queue [q]. *)
  val push : 'a t -> 'a -> unit;;
 
  (** [pop q] pops an element [e] from the front of the queue and returns
      [Some v] if the queue is non-empty. Otherwise, returns [None].
      It should only be called by thread which owns the queue [q]. *)
  val pop : 'a t -> 'a option;;

  (** [steal q] steal an element from the back of the queue and returns
      [Some v] if the queue is non-empty. Otherwise, returns [None].
      It should only be called by thread which doesn't own the queue [q].*)
  val steal : 'a t -> 'a option;;
end;;

module M : S;;
