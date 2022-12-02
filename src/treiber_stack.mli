(** Classic multi-producer multi-consumer Treiber stack. Robust and flexible. 
    Recommended starting point when needing LIFO structure. *)

type 'a t
(** Type of Treiber stack holding items of type [t]. *)

val create : unit -> 'a t
(** [create] creates an empty Treiber stack. *)

val is_empty : 'a t -> bool
(** [is_empty] checks whether stack is empty. *)

val push : 'a t -> 'a -> unit
(** [push] inserts element into the stack. *)

val pop : 'a t -> 'a option
(** [pop] removes the youngest element from the stack (if any). *)
