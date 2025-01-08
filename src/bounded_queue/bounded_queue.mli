(** Lock-free bounded Queue.

    This module implements a lock-free bounded queue based on Michael-Scott's
    queue algorithm. Adding a capacity to this algorithm adds a general overhead
    to the operations, and thus, it is recommended to use the unbounded queue
    {!Saturn.Queue} if you don't need it. *)

include Bounded_queue_intf.BOUNDED_QUEUE
