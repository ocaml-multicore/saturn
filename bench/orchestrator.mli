(** Helper library that ensures all workers have started before any 
  starts making progress on the benchmark. *)

type t

val init : total_domains:int -> t
val worker : t -> (unit -> unit) -> unit
val run : t -> ?drop_first:bool -> int -> float List.t
