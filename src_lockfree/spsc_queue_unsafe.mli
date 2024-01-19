(** Single producer single consumer queue. *)

type 'a t
(** Type of single-producer single-consumer non-resizable domain-safe
   queue that works in FIFO order. *)

val create : size_exponent:int -> 'a t
(** [create ~size_exponent:int] returns a new queue of maximum size
   [2^size_exponent] and initially empty. *)

val size : 'a t -> int
(** [size] returns the size of the queue. This method linearizes only when called
  from either consumer or producer domain. Otherwise, it is safe to call but
  provides only an *indication* of the size of the structure. *)

(** {1 Producer functions} *)

exception Full
(** Raised when {!push} is applied to a full queue. *)

val push : 'a t -> 'a -> unit
(** [push q v] adds the element [v] at the end of the queue [q]. This
    method can be used by at most one domain at the time.

   @raise Full if the queue is full.
*)

val try_push : 'a t -> 'a -> bool
(** [try_push q v] tries to add the element [v] at the end of the
    queue [q]. It fails it the queue [q] is full.  This method can be
    used by at most one domain at the time.
*)

(** {2 Consumer functions} *)

exception Empty
(** Raised when {!pop} or {!peek} is applied to an empty queue. *)

val pop : 'a t -> 'a
(** [pop q] removes and returns the first element in queue [q]. This
    method can be used by at most one domain at the time.

    @raise Empty if [q] is empty.
*)

val pop_opt : 'a t -> 'a option
(** [pop_opt q] removes and returns the first element in queue [q], or
    returns [None] if the queue is empty. This method can be used by
    at most one domain at the time. *)

val peek : 'a t -> 'a
(** [peek q] returns the first element in queue [q]. This method can be
    used by at most one domain at the time.

    @raise Empty if [q] is empty.
*)

val peek_opt : 'a t -> 'a option
(** [peek_opt q] returns the first element in queue [q], or [None]
    if the queue is empty. This method can be used by at most one
    domain at the time.
*)
