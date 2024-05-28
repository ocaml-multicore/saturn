module type SPSC_queue = sig
  (** Single producer single consumer queue. *)

  type 'a t
  (** Type of single-producer single-consumer non-resizable domain-safe queue that
    works in FIFO order. *)

  val create : size_exponent:int -> 'a t
  (** [create ~size_exponent:int] returns a new queue of maximum size
    [2^size_exponent] and initially empty. *)

  val size : 'a t -> int
  (** [size] returns the size of the queue.  This method linearizes only when
    called from either consumer or producer domain.  Otherwise, it is safe to
    call but provides only an *indication* of the size of the structure. *)

  (** {1 Producer functions} *)

  exception Full
  (** Raised when {!push_exn} is applied to a full queue. 

    This exception is meant to avoid allocations required by an option type.  As
    such, it does not register backtrace information and it is recommended to
    use the following pattern to catch it.
    {[
    match push_exn q v with 
     | () -> (* ... *)
     | exception Full -> (* ... *)
    ]} *)

  val push_exn : 'a t -> 'a -> unit
  (** [push q v] adds the element [v] at the end of the queue [q].  This method
    can be used by at most one domain at a time.

    @raise Full if the queue is full. *)

  val try_push : 'a t -> 'a -> bool
  (** [try_push q v] tries to add the element [v] at the end of the queue [q].  It
    fails it the queue [q] is full.  This method can be used by at most one
    domain at a time. *)

  (** {2 Consumer functions} *)

  exception Empty
  (** Raised when {!pop_exn} or {!peek_exn} is applied to an empty queue. 

    This exception is meant to avoid allocations required by an option type.  As
    such, it does not register backtrace information and it is recommended to
    use the following pattern to catch it.
    {[
    match pop_exn q with 
     | value -> (* ... *)
     | exception Empty -> (* ... *)
    ]} *)

  val pop_exn : 'a t -> 'a
  (** [pop_exn q] removes and returns the first element in queue [q].  This method
    can be used by at most one domain at a time.

    @raise Empty if [q] is empty. *)

  val pop_opt : 'a t -> 'a option
  (** [pop_opt q] removes and returns the first element in queue [q], or returns
    [None] if the queue is empty.  This method can be used by at most one domain
    at a time. *)

  val peek_exn : 'a t -> 'a
  (** [peek_exn q] returns the first element in queue [q].  This method can be
    used by at most one domain at a time.

    @raise Empty if [q] is empty. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt q] returns the first element in queue [q], or [None] if the queue
    is empty.  This method can be used by at most one domain at a time. *)
end
