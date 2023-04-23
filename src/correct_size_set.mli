type 'a t

val create : unit -> 'a t
val insert : 'a t -> int -> 'a -> bool
val delete : 'a t -> int -> 'a -> bool
val contains : 'a t -> 'a -> bool
val size : 'a t -> int
