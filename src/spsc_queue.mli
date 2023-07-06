(** Single producer single consumer queue. *)

type 'a t
(** Type of single-producer single-consumer non-resizable domain-safe
   queue that works in FIFO order. *)

exception Full
(** Raised when {!push} is applied to a full queue. *)

val create : size_exponent:int -> 'a t
(** [create ~size_exponent:int] returns a new queue of maximum size
   [2^size_exponent] and initially empty. *)

val push : 'a t -> 'a -> unit
(** [push q v] adds the element [v] at the end of the queue [q]. This
    method can be used by at most one domain at the time.

   @raise [Full] if the queue is full.
*)

val pop : 'a t -> 'a option
(** [pop q] removes and returns the first element in queue [q], or
    returns [None] if the queue is empty. This method can be used by
    at most one domain at the time. *)

val size : 'a t -> int
(** [size] returns the size of the queue. This method linearizes only when called
  from either consumer or producer domain. Otherwise, it is safe to call but
  provides only an *indication* of the size of the structure. *)
