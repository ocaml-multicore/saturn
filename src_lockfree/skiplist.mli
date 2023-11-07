type 'a t

val create : ?max_height:int -> unit -> 'a t
val mem : 'a t -> 'a -> bool
val add : 'a t -> 'a -> bool
val remove : 'a t -> 'a -> bool
