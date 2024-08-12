type !'a t
(** *)

val create : unit -> 'a t
(** *)

val push : 'a t -> 'a -> unit
(** *)

val push_head : 'a t -> 'a -> unit
(** *)

exception Empty
(** Raised by {!pop_exn} in case the queue is empty. *)

val pop_exn : 'a t -> 'a
(** *)

val pop_opt : 'a t -> 'a option
(** *)

val length : 'a t -> int
(** *)
