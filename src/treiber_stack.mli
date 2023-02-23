(** Classic multi-producer multi-consumer Treiber stack.

    All function are lockfree. It is the recommended starting point
    when needing LIFO structure. *)

type 'a t
(** Type of Treiber stack holding items of type [t]. *)

val create : unit -> 'a t
(** [create ()] returns a new and empty Treiber stack. *)

val is_empty : 'a t -> bool
(** [is_empty s] checks whether stack [s] is empty. *)

val push : 'a t -> 'a -> unit
(** [push s v] adds the element [v] at the top of stack [s]. *)

val pop : 'a t -> 'a option
(** [pop a] removes and returns the topmost element in the
    stack [s], or returns [None] if the stack is empty. *)
