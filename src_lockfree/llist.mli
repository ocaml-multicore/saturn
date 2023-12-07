type (!'k, !'v) t

val create : compare:('k -> 'k -> int) -> unit -> ('k, 'v) t
val length : ('k, 'v) t -> int
val try_add : ('k, 'v) t -> 'k -> 'v -> bool
val try_remove : ('k, 'v) t -> 'k -> bool
val mem : ('k, 'v) t -> 'k -> bool
