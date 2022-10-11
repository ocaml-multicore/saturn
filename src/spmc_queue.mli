(* add lifo slot? *)

type 'a t = private {
  head : int Atomic.t;
  tail : int Atomic.t;
  mask : int Atomic.t;
  array : 'a option Atomic.t Array.t Atomic.t;
}

val init : size_exponent:int -> unit -> 'a t

module Local : sig
  val enqueue : 'a t -> 'a -> bool
  val dequeue : 'a t -> 'a option
  val resize : 'a t -> unit
  val enqueue_with_resize : 'a t -> 'a -> unit
  val steal : from:'a t -> 'a t -> int

  val is_empty : 'a t -> bool
  (** [is_empty] thoroughly checks if there's any elements in the array.
    Call before [steal], which assumes there's room in the queue. *)
end

(** [indicative_size] gives some idea about the size of the queue. 
    It's actual queue size when called from enqueuer thread (or while 
    enqueuer is not inserting elements). *)
val indicative_size : 'a t -> int