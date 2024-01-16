type ('k, 'v) t

val create : size_exponent:int -> ('k, 'v) t
val length : (_, _) t -> int
val add : ('k, 'v) t -> 'k -> 'v -> unit
val mem : ('k, 'v) t -> 'k -> bool
val find_all : ('k, 'v) t -> 'k -> 'v list
val try_remove : ('k, 'v) t -> 'k -> bool
val replace : ('k, 'v) t -> 'k -> 'v -> unit
