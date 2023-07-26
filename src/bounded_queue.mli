(**
    Michael-Scott 2-Lock Bounded-Queue.

    The push functions waits till the queue is not full and 
    the pop function waits tilll the queue is not empty. The
    implementation is inspired from section 10.3 in art of
    multiprocessor programming.
*)

type 'a t
(** The type of 2-lock queue *)

val create : ?max_size:int -> unit -> 'a t
(** new queue with dummy node *)

val push : 'a t -> 'a -> unit
(** [push q ele] pushes [ele] to tail of [q] *)

val pop : 'a t -> 'a
(** [pop q] from head of [q] *)

val is_empty : 'a t -> bool
(** check if [q] is empty or not *)

val peek : 'a t -> 'a option
(** [peek q] return element at head of [q] *)

val unbounded_push : 'a t -> 'a -> unit
(** [unbounded_push q ele] pushes [ele] to tail of [q] irrespective of capacity, made for stm tests *)

val unbounded_pop : 'a t -> 'a option
(** [unbounded_pop q] from head of [q], None if empty, made for stm tests *)
