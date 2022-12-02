(** 
    A lock-free multi-producer, multi-consumer, thread-safe, relaxed-FIFO queue. 

    It exposes two interfaces: [Spin] and [Not_lockfree]. [Spin] is lock-free 
    formally, but the property is achieved in a fairly counterintuitive way - 
    - by using the fact that lock-freedom does not impose any constraints on
    partial methods. In simple words, an invocation of function that cannot 
    logically terminate (`push` on full queue, `pop` on empty queue), it is 
    allowed to *busy-wait* until the precondition is meet. 

    Above interface is impractical outside specialized applications. Thus, 
    [Mpmc_relaxed_queue] also exposes [Not_lockfree] interface. [Not_lockfree] 
    contains non-lockfree paths. While formally a locked algorithm, it will 
    often be the more practical solution as it allows having an overflow 
    queue, etc. 
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


module Spin : sig 
    (** [Spin] exposes a formally lock-free interface as per the [A lock-free relaxed 
    concurrent queue for fast work distribution] paper. Functions here busy-wait if 
    the action cannot be completed (i.e. [push] on full queue, [pop] on empty queue). *)

    val push : 'a t -> 'a -> unit 
    (** [push t x] adds [x] to the tail of the queue. If the queue is full, [push] 
       busy-waits until another thread removes an item. *)

    val pop : 'a t -> 'a 
    (** [pop t] removes an item from the head of the queue. If the queue is empty, 
       [pop] busy-waits until an item appear. *)
end


module Not_lockfree : sig     
    (** [Non_lockfree] exposes an interface that contains non-lockfree paths, i.e. threads 
    may need to cooperate to terminate. It is often more practical than [Spin], in 
    particular when using a fair OS scheduler. *)

    val push : 'a t -> 'a -> bool
    (** [push t x] adds [x] to the tail of the queue.
        Returns [false] if [t] is currently full. *)

    val pop : 'a t -> 'a option
    (** [pop t] removes the head item from [t] and returns it.
        Returns [None] if [t] is currently empty. *)

    module CAS_interface : sig 
        (** Alternative interface, which may perform better on architectures without 
        FAD instructions (e.g. AArch). 

        CAS_interface should not be the default choice. It may be a little faster 
        on ARM, but it is going to be a few times slower than standard on x86.
        *)

        val push : 'a t -> 'a -> bool 
        val pop : 'a t -> 'a option
    end
end