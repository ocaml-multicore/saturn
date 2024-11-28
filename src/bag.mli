(** Randomized lock-free bag implemented using a hash table *)

(** {1 API} *)

type !'v t
(** Represents a lock-free bag of elements of type 'v *)

val create : unit -> 'v t
(** [create ()] creates a new empty lock-free bag. *)

val push : 'v t -> 'v -> unit
(** [push bag elt] adds [elt] to the [bag]. *)

exception Empty
(** Raised when {!pop_exn} is applied to an empty bag. *)

val pop_exn : 'v t -> 'v
(** [pop_exn bag] removes and returns a random element of the [bag].
 
  @raises Empty if the [bag] is empty. *)
