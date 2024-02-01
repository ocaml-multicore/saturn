(* type ('k, 'v) t *)

(* val create : size_exponent:int -> ('k, 'v) t *)
(* val length : (_, _) t -> int *)
(* val add : ('k, 'v) t -> 'k -> 'v -> unit *)
(* val mem : ('k, 'v) t -> 'k -> bool *)
(* val find_all : ('k, 'v) t -> 'k -> 'v list *)
(* val find_opt : ('k, 'v) t -> 'k -> 'v option *)
(* val try_remove : ('k, 'v) t -> 'k -> bool *)
(* val replace : ('k, 'v) t -> 'k -> 'v -> unit *)

type (_, 'v) t

val create : size_exponent:int -> (int, 'v) t
val length : (_, _) t -> int
val add : (int, 'v) t -> int -> 'v -> unit
val mem : (int, 'v) t -> int -> bool
val find_opt : (int, 'v) t -> int -> 'v option
val find_all : (int, 'v) t -> int -> 'v list
val try_remove : (int, 'v) t -> int -> bool
val replace : (int, 'v) t -> int -> 'v -> unit
