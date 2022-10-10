(** A lock-free multi-producer, multi-consumer, thread-safe queue. 
    
    Note: in extreme cases the structure may occassionaly violate FIFO 
    order. 
*)

type 'a t = private {
  array : 'a Option.t Atomic.t Array.t;
  head : int Atomic.t;
  tail : int Atomic.t;
  mask : int;
}
(** A queue of items of type ['a]. Implementation exposed for testing. *)

val create : size_exponent:int -> unit -> 'a t
(** [create ~size_exponent:int] creates an empty queue of size
    [2^size_exponent].  *)

val push : 'a t -> 'a -> bool
(** [push t x] adds [x] to the tail of the queue.
    Returns [false] if [t] is currently full. *)

val pop : 'a t -> 'a option
(** [pop t] removes the head item from [t] and returns it.
    Returns [None] if [t] is currently empty. *)

module CAS_interface : sig 
    (* Alternative interface, which may perform better on architectures without 
       FAD instructions (e.g. AArch). 

       CAS_interface should not be the default choice. It may be a little faster 
       on ARM, but it is going to be a few times slower than standard on x86.
       *)

    val push : 'a t -> 'a -> bool 
    val pop : 'a t -> 'a option
end