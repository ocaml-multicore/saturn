(** Helper library that ensures all workers have started before any 
  starts making progress on the benchmark. *)

type t

val init : total_domains:int -> rounds:int -> t
val worker : t -> (unit -> unit) -> unit
val run : ?drop_first:bool -> t -> float List.t