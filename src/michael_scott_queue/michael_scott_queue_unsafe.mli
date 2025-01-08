(** Optimized Michael-Scott lock-free multi-producer multi-consumer queue.

    All functions are lockfree. It is the recommended starting point when
    needing FIFO structure. It is inspired by
    {{:https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf} Simple,
     Fast, and Practical Non-Blocking and Blocking Concurrent Queue Algorithms}.

    If you need a [length] function, you can use the bounded queue
    {!Saturn.Bounded_queue} instead with maximun capacity (default value).
    However, this adds a general overhead to the operation. *)

include Michael_scott_queue_intf.MS_QUEUE
