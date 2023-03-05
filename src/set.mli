type 'a t

val create : unit -> 'a t

val insert : 'a t -> 'a -> unit

val delete : 'a t -> 'a -> bool

val search : 'a t -> 'a -> bool
