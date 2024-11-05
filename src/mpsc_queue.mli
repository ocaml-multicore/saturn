(** Lock-free multi-producer, single-consumer, domain-safe queue
    without support for cancellation.

    This makes a good data structure for a scheduler's run queue and
    is currently (September 2022) used for Eio's scheduler. *)

type 'a t
(** A queue of items of type ['a]. *)

exception Closed

val create : unit -> 'a t
(** [create ()] returns a new empty queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] is [true] if calling [pop] would return [None].

    @raise Closed if [q] is closed and empty. *)

val close : 'a t -> unit
(** [close q] marks [q] as closed, preventing any further items from
    being pushed by the producers (i.e. with {!push}).

    @raise Closed if [q] has already been closed. *)

val push : 'a t -> 'a -> unit
(** [push q v] adds the element [v] at the end of the queue [q]. This
    can be used safely by multiple producer domains, in parallel with
    the other operations.

    @raise Closed if [q] is closed. *)

(** {2 Consumer-only functions} *)

exception Empty
(** Raised when {!pop} or {!peek} is applied to an empty queue. *)

val pop : 'a t -> 'a
(** [pop q] removes and returns the first element in queue [q].

    @raise Empty if [q] is empty.

    @raise Closed if [q] is closed and empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt q] removes and returns the first element in queue [q] or
    returns [None] if the queue is empty.

    @raise Closed if [q] is closed and empty. *)

val peek : 'a t -> 'a
(** [peek q] returns the first element in queue [q].

    @raise Empty if [q] is empty.

    @raise Closed if [q] is closed and empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt q] returns the first element in queue [q] or
    returns [None] if the queue is empty.

    @raise Closed if [q] is closed and empty. *)

val push_head : 'a t -> 'a -> unit
(** [push_head q v] adds the element [v] at the head of the queue
    [q]. This can only be used by the consumer (if run in parallel
    with {!pop}, the item might be skipped).

    @raise Closed if [q] is closed and empty. *)
