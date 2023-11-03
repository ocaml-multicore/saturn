module type Ops = sig
  type 'a t
  type ('x, 'fn) fn

  val is_empty : ('x, 'a t -> bool) fn
  (** [is_empty mv] determines whether the synchronizing variable [mv] contains
      a value or not. *)

  val put : ('x, 'a t -> 'a -> unit) fn
  (** [put mv x] fills the synchronizing variable [mv] with the value [v] or
      blocks until the variable becomes empty. *)

  val try_put : ('x, 'a t -> 'a -> bool) fn
  (** [try_put mv x] tries to fill the synchronizing variable [mv] with the
      value [v] and returns [true] on success or [false] in case the variable is
      full. *)

  val take : ('x, 'a t -> 'a) fn
  (** [take mv] removes and returns the current value of the synchronizing
      variable [mv] or blocks waiting until the variable is filled. *)

  val take_opt : ('x, 'a t -> 'a option) fn
  (** [take_opt mv] removes and returns the current value of the synchronizing
      variable [mv] or returns [None] in case the variable is empty. *)

  val peek : ('x, 'a t -> 'a) fn
  (** [peek mv] returns the current value of the synchronizing variable [mv] or
      blocks waiting until the variable is filled. *)

  val peek_opt : ('x, 'a t -> 'a option) fn
  (** [peek_opt mv] returns the current value of the synchronizing variable [mv]
      or returns [None] in case the variable is empty. *)
end
