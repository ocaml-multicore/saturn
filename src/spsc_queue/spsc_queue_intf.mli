module type SPSC_queue = sig
  (** Single producer single consumer queue. *)

  (** {1 API} *)

  type 'a t
  (** Represent single-producer single-consumer non-resizable queue
        that works in FIFO order. *)

  val create : size_exponent:int -> 'a t
  (** [create ~size_exponent:int] create a new single-producer single-consumer
     queue of maximum size [2^size_exponent] and initially empty. *)

  val size : 'a t -> int
  (** [size] returns the size of the queue.  This method linearizes only when
        called from either consumer or producer domain.  Otherwise, it is safe to
        call but provides only an *indication* of the size of the structure. *)

  (** {2 Producer functions} *)

  exception Full
  (** Raised when {!push_exn} is applied to a full queue. *)

  val push_exn : 'a t -> 'a -> unit
  (** [push qeueu elt] adds the element [elt] at the end of the [queue].
      This method can be used by at most one domain at a time.
  
        @raise Full if the [queue] is full. *)

  val try_push : 'a t -> 'a -> bool
  (** [try_push qeueue elt] tries to add the element [elt] at the end of the 
      [queue]. If the queue [q] is full, [false] is returned. This method can be
      used by at most one domain at a time. *)

  (** {2 Consumer functions} *)

  exception Empty
  (** Raised when {!pop_exn} or {!peek_exn} is applied to an empty queue. *)

  val pop_exn : 'a t -> 'a
  (** [pop_exn queue] removes and returns the first element in [queue]. This
     method can be used by at most one domain at a time.
  
        @raise Empty if the [queue] is empty. *)

  val pop_opt : 'a t -> 'a option
  (** [pop_opt queue] removes and returns [Some] of the first element of the
      [queue], or [None] if the queue is empty. This method can be used by at most
      one domain at a time. *)

  val peek_exn : 'a t -> 'a
  (** [peek_exn queue] returns the first element in [queue] without removing it.
     This method can be used by at most one domain at a time.
  
        @raise Empty if the [queue] is empty. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt q] returns [Some] of the first element in queue [q], or [None] 
      if the queue is empty.  This method can be used by at most one domain at a
      time. *)
end
