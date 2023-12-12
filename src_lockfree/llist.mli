type (!'k, !'v) t

val create : compare:('k -> 'k -> int) -> unit -> ('k, 'v) t
val length : ('k, 'v) t -> int
val add : ('k, 'v) t -> 'k -> 'v -> unit
val replace : ('k, 'v) t -> 'k -> 'v -> unit
val try_remove : ('k, 'v) t -> 'k -> bool
val mem : ('k, 'v) t -> 'k -> bool
val find : ('k, 'v) t -> 'k -> 'v
val find_opt : ('k, 'v) t -> 'k -> 'v option
val find_all : ('k, 'v) t -> 'k -> 'v list
