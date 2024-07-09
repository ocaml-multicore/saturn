(** *)

type !'a t
(** *)

val create : unit -> 'a t
(** *)

val is_empty : 'a t -> bool
(** *)

val push : 'a t -> 'a -> unit
(** *)

exception Empty
(** *)

val pop_exn : 'a t -> 'a
(** *)

val pop_opt : 'a t -> 'a option
(** *)

val peek_exn : 'a t -> 'a
(** *)

val peek_opt : 'a t -> 'a option
(** *)
