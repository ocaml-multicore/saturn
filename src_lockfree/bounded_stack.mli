type 'a t

val create : ?capacity:int -> unit -> 'a t
val length : 'a t -> int
val is_empty : 'a t -> bool
val peek : 'a t -> 'a
val peek_opt : 'a t -> 'a option
val pop : 'a t -> 'a
val pop_opt : 'a t -> 'a option
val push : 'a t -> 'a -> unit
val try_push : 'a t -> 'a -> bool
