type 'a t
(** Type of single-producer single-consumer non-resizable thread-safe
   queue that works in FIFO order. *)

exception Full

val create : size_exponent:int -> 'a t
(** [create ~size_exponent:int] creates a empty queue of size
   [2^size_exponent].  *)

val try_push : 'a t -> 'a -> bool
(** [try_push q v] attempts to push [v] at the back of the queue
    and returns [true] on success, or [false] if the queue is full. *)

val push : rdv:unit Rendezvous.t -> 'a t -> 'a -> unit
(** [push ~rdv q v] pushes [v] at the back of the queue, potentially
    blocking using [rdv] while the queue is full. *)

val try_pop : 'a t -> 'a option
(** [try_pop q] removes an element from the head of the queue, if any. *)

val pop : rdv:unit Rendezvous.t -> 'a t -> 'a
(** [pop q] removes an element from the head of the queue, potentially
    blocking using [rdv] while the queue is empty. *)

val size : 'a t -> int
(** [size] returns the size of the queue. This method linearizes only when called
  from either enqueuer or dequeuer thread. Otherwise, it is safe to call but
  provides only an *indication* of the size of the structure. *)
