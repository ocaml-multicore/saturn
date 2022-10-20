type 'a t
(** Type of single-producer single-consumer non-resizable thread-safe
   queue that works in FIFO order. *)

exception Full

val create : size_exponent:int -> 'a t
(** [create ~size_exponent:int] creates a empty queue of size
   [2^size_exponent].  *)

val push : 'a t -> 'a -> unit
(** [push q v] pushes [v] at the back of the queue. 
    
   @raise [Full] if the queue is full.
*)

val pop : 'a t -> 'a option
(** [pop q] removes element from head of the queue, if any. This method can be used by
  at most 1 thread at the time. *)

val size : 'a t -> int
(** [size] returns the size of the queue. This method linearizes only when called
  from either enqueuer or dequeuer thread. Otherwise, it is safe to call but
  provides only an *indication* of the size of the structure. *)
