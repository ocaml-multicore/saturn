module type Ops = sig
  type !+'a t
  type !-'a u
  type 'a or_exn
  type ('x, 'fn) fn

  val resolve : ('x, 'a u -> 'a -> unit) fn
  (** [resolve u v] resolves the promise corresponding to the resolver [u] to
      the value [v].  Any awaiters of the corresponding promise are then
      unblocked. *)

  val await : ('x, 'a t -> 'a) fn
  (** [await t] either immediately returns the resolved value of the promise [t]
      or blocks until the promise [t] is resolved. *)

  val peek : ('x, 'a t -> 'a option) fn
  (** [peek t] immediately returns either the resolved value of the promise [t]
      or [None] in case the promise hasn't yet been resolved. *)

  val is_resolved : ('x, 'a t -> bool) fn
  (** [is_resolved t] determines whether the promise [t] has already been
      resolved. *)

  (** {2 Result promises} *)

  val await_exn : ('x, 'a or_exn -> 'a) fn
  (** [await_exn t] is equivalent to [match await t with v -> v | exception e -> raise e]. *)

  val resolve_ok : ('x, ('a, 'b) result u -> 'a -> unit) fn
  (** [resolve_ok u v] is equivalent to [resolve u (Ok v)]. *)

  val resolve_error : ('x, ('a, 'b) result u -> 'b -> unit) fn
  (** [resolve_error u e] is equivalent to [resolve u (Error e)]. *)
end
