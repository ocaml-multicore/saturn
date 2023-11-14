module type Ops = sig
  type 'a t
  type ('x, 'fn) fn

  val is_empty : ('x, 'a t -> bool) fn
  (** [is_empty s] determines whether the queue [q] is empty. *)

  val length : ('x, 'a t -> int) fn
  (** [length q] returns the length of the queue [q]. *)

  val clear : ('x, 'a t -> unit) fn
  (** [clear q] removes all elements from the queue [q]. *)

  val swap : ('x, 'a t -> 'a t -> unit) fn
  (** [swap q1 q2] exchanges the contents of the queues [q1] and [q2]. *)

  val to_seq : ('x, 'a t -> 'a Seq.t) fn
  (** [to_seq s] returns a domain safe sequence for iterating through the
      elements of the queue front to back.

      The sequence is based on a constant time, [O(1)], snapshot of the queue
      and modifications of the queue have no effect on the sequence. *)

  val add : ('x, 'a -> 'a t -> unit) fn
  (** [add x q] adds the element [x] at the end of the queue [q]. *)

  val push : ('x, 'a -> 'a t -> unit) fn
  (** [push] is a synonym for {!add}. *)

  val peek_opt : ('x, 'a t -> 'a option) fn
  (** [peek_opt q] returns the first element in queue [q], without removing it
      from the queue, or returns [None] if the queue is empty. *)

  val peek_blocking : ('x, 'a t -> 'a) fn
  (** [peek_blocking q] returns the first element in queue [q], without removing
      it from the queue, or blocks waiting for the queue to become non-empty. *)

  val take_blocking : ('x, 'a t -> 'a) fn
  (** [take_blocking q] removes and returns the first element in queue [q], or
      blocks waiting for the queue to become non-empty. *)

  val take_opt : ('x, 'a t -> 'a option) fn
  (** [take_opt q] removes and returns the first element in queue [q], or
      returns [None] if the queue is empty. *)

  val take_all : ('x, 'a t -> 'a Seq.t) fn
  (** [take_all q] removes and returns a domain safe sequence for iterating
      through all the elements that were in the queue front to back. *)
end
