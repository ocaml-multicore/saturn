
type 'a t

val create : unit -> 'a t

val is_empty : 'a t -> bool

val push : 'a t -> 'a -> unit

val pop : 'a t -> 'a option