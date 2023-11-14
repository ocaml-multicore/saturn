module type Ops = sig
  type t
  type ('x, 'fn) fn

  val add : ('x, t -> int -> unit) fn
  (** [add a n] increments the value of the accumulator [a] by [n].  [add]
      operations can be performed scalably in parallel. *)

  val incr : ('x, t -> unit) fn
  (** [incr a] is equivalent to [add a 1]. *)

  val decr : ('x, t -> unit) fn
  (** [decr a] is equivalent to [add a (-1)]. *)

  val get : ('x, t -> int) fn
  (** [get a] returns the current value of the accumulator.

      {b CAUTION}: Performing a [get] is expensive and can limit scalability. *)

  val set : ('x, t -> int -> unit) fn
  (** [set a n] sets the current value of the accumulator [a] to [n]. *)
end
