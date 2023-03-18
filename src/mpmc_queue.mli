(** A lock-free multi-producer, multi-consummer, thread-safe unbounded queue. *)

type 'a t
(** A queue of items of type ['a]. *)

val make : ?capacity:int -> dummy:'a -> unit -> 'a t
(** [make ~dummy ()] creates a new empty queue.

    - The [dummy] element is a placeholder for ['a] values.
    - The optional parameter [?capacity] defaults to 512 and is used to size the
      internal buffers of the queue: Choosing a small number lower the pause
      durations caused by allocations, but a larger capacity can provide overall
      faster operations.
*)

val push : 'a t -> 'a -> unit
(** [push t x] adds [x] to the tail of the queue. *)

val pop : 'a t -> 'a option
(** [pop t] removes the head item from [t] and returns it.
    Returns [None] if [t] is currently empty. *)