(** Lock-free single-producer, multi-consumer dynamic-size double-ended queue (deque).

    The main strength of deque in a typical work-stealing setup with per-core structure
    is efficient work distribution. Owner uses [push] and [pop] method to operate at
    one end of the deque, while other (free) cores can efficiently steal work on the
    other side.

    This approach is great for throughput. Stealers and owner working on different sides
    reduces contention in work distribution. Further, local LIFO order runs related tasks
    one after one improves locality.

    On the other hand, the local LIFO order does not offer any fairness guarantees.
    Thus, it is not the best choice when tail latency matters.
*)

type 'a t
(** Type of work-stealing queue *)

val create : unit -> 'a t
(** [create ()] returns a new empty work-stealing queue. *)

exception Empty

(** {1 Queue owner functions} *)

val push : 'a t -> 'a -> unit
(** [push t v] adds [v] to the front of the queue [q].
      It should only be invoked by the domain which owns the queue [q]. *)

val pop_exn : 'a t -> 'a
(** [pop_exn q] removes and returns the first element in queue
      [q].It should only be invoked by the domain which owns the queue
      [q].

      @raise Exit if the queue is empty.
      *)

val pop_opt : 'a t -> 'a option
(** [pop_opt q] removes and returns the first element in queue [q], or
    returns [None] if the queue is empty.  *)

(** {1 Stealers function} *)

val steal_exn : 'a t -> 'a
(** [steal_exn q] removes and returns the last element from queue
      [q]. It should only be invoked by domain which doesn't own the
      queue [q].

      @raise Exit if the queue is empty.
  *)

val steal_opt : 'a t -> 'a option
(** [steal_opt q] removes and returns the last element from queue
      [q], or returns [None] if the queue is empty. It should only be
      invoked by domain which doesn't own the queue [q]. *)
