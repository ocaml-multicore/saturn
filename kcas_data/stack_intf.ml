module type Ops = sig
  type 'a t
  type ('x, 'fn) fn

  val is_empty : ('x, 'a t -> bool) fn
  (** [is_empty s] determines whether the stack [s] is empty. *)

  val length : ('x, 'a t -> int) fn
  (** [length s] returns the length of the stack [s]. *)

  val clear : ('x, 'a t -> unit) fn
  (** [clear s] removes all elements from the stack [s]. *)

  val swap : ('x, 'a t -> 'a t -> unit) fn
  (** [swap s1 s2] exchanges the contents of the stacks [s1] and [s2]. *)

  val to_seq : ('x, 'a t -> 'a Seq.t) fn
  (** [to_seq s] returns a domain safe sequence for iterating through the
      elements of the stack top to bottom.

      The sequence is based on a constant time, [O(1)], snapshot of the stack
      and modifications of the stack have no effect on the sequence. *)

  val push : ('x, 'a -> 'a t -> unit) fn
  (** [push x s] adds the element [x] to the top of the stack [s]. *)

  val pop_opt : ('x, 'a t -> 'a option) fn
  (** [pop_opt s] removes and returns the topmost element of the stack [s], or
      [None] if the stack is empty. *)

  val pop_all : ('x, 'a t -> 'a Seq.t) fn
  (** [pop_all s] removes and returns a domain safe sequence for iterating
      through all the elements that were in the stack top to bottom. *)

  val pop_blocking : ('x, 'a t -> 'a) fn
  (** [pop_blocking s] removes and returns the topmost element of the stack [s],
      or blocks waiting for the queue to become non-empty. *)

  val top_opt : ('x, 'a t -> 'a option) fn
  (** [top_opt s] returns the topmost element in stack [s], or [None] if the
      stack is empty. *)

  val top_blocking : ('x, 'a t -> 'a) fn
  (** [top_blocking s] returns the topmost element in stack [s], or blocks
      waiting for the queue to become non-empty. *)
end
