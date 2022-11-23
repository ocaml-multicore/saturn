(** Truncated exponential backoff. 

  Generally, a backoff mechanism adjusts time waited between retries to ensure the retries 
  will not put too much pressure on some underlying system. This particular implementation 
  is built for reducing contention in lockfree algorithms. 
  
  Under the hood, it uses relevant pause instruction to avoid adverse 
  microarchitectural effects present in naive busy-looping. 
  
  *)

type t
(** [t] type of a backoff object. *)

val create : ?min_wait:int -> ?max_wait:int -> unit -> t
(** [create] creates a new instance of backoff. [max_wait], [min_wait] override 
  the upper and lower bound on the number of spins executed by [once]. *)

val once : t -> unit
(** [once] executes one wait, whose length increases for every consecutive attempt 
    (until [max] is reached). *)

val reset : t -> unit
(** [reset] resets the attempt counter in [t]. *)