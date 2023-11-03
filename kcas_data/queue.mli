open Kcas

(** First-In First-Out (FIFO) queue.

    The interface provides a subset of the OCaml [Stdlib.Queue] module.
    [transfer] and [add_seq] are not provided at all.  Compositional versions of
    {!iter}, {!fold}, {!peek}, {!top}, and {!take} are not provided.

    The queue implementation is designed to avoid contention between a producer
    and a consumer operating concurrently.  The implementation is also designed
    to avoid starvation.  Performance in most concurrent use cases should be
    superior to what can be achieved with one or two locks. *)

(** {1 Common interface} *)

type !'a t
(** The type of queues containing elements of type ['a]. *)

exception Empty
(** Raised when {!take} or {!peek} is applied to an empty queue. *)

val create : unit -> 'a t
(** [create ()] returns a new empty queue. *)

val copy : 'a t -> 'a t
(** [copy q] returns a copy of the queue [q]. *)

(** {1 Compositional interface} *)

module Xt :
  Queue_intf.Ops
    with type 'a t := 'a t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on queues. *)

(** {1 Non-compositional interface} *)

include Queue_intf.Ops with type 'a t := 'a t with type ('x, 'fn) fn := 'fn

val peek : 'a t -> 'a
(** [peek q] returns the first element in queue [s], or raises {!Empty} if the
    queue is empty. *)

val top : 'a t -> 'a
(** [top] is a synonym for {!peek}. *)

val take : 'a t -> 'a
(** [take s] removes and returns the first element in queue [q], or raises
    {!Empty} if the queue is empty. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f s] is equivalent to [Seq.iter f (to_seq s)]. *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold f s] is equivalent to [Seq.fold_left f a (to_seq s)]. *)
