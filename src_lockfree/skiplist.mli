(** Skiplist TODO

    [key] values are compared with [=] and thus should not be functions or
    objects.
*)

type 'a t
(** The type of lock-free skiplist. *)

val create : ?max_height:int -> unit -> 'a t
(** [create ~max_height ()] returns a new empty skiplist. [~max_height] is the
    number of level used to distribute nodes. Its default value is 10 by default
    and can not be less than 1. *)

val add : 'a t -> 'a -> bool
(** [add s v] adds [v] to [s] if [v] is not already in [s] and returns
    [true]. If [v] is already in [s], it returns [false] and [v] is unchanged. *)

val remove : 'a t -> 'a -> bool
(** [remove s v] removes [v] of [s] if [v] is in [s] and returns [true]. If [v]
    is not in [s], it returns [false] and [v] is unchanged. *)

val mem : 'a t -> 'a -> bool
(** [mem s v] returns [true] if v is in s and [false] otherwise. *)
