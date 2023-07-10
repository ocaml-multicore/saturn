(**
    A multi-producer, multi-consumer, thread-safe, bounded relaxed-FIFO queue.

    This interface is a subset of the one in `Saturn.Relaxed_queue`
    that exposes a formally lock-free interface as per the [A
    lock-free relaxed concurrent queue for fast work distribution]
    paper.

    [push] and [pop] are said to be `lock-free formally`, because the
    property is achieved in a fairly counterintuitive way - by using
    the fact that lock-freedom does not impose any constraints on
    partial methods. In simple words, an invocation of function that
    cannot logically terminate (`push` on full queue, `pop` on empty
    queue), it is allowed to *busy-wait* until the precondition is
    meet.
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

val push : 'a t -> 'a -> unit
(** [push t x] adds [x] to the tail of the queue. If the queue is full, [push]
    busy-waits until another thread removes an item. *)

val pop : 'a t -> 'a
(** [pop t] removes an item from the head of the queue. If the queue is empty,
    [pop] busy-waits until an item appear. *)
