type 'a t
(** The type of 2-lock queue *)

val init : unit -> 'a t
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

val unbounded_pop: 'a t -> 'a option
(** [unbounded_pop q] from head of [q], None if empty, made for stm tests *)