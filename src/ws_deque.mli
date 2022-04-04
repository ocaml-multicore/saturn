module type S = sig

  type 'a t
  (** Type of work-stealing queue *)

  val create : unit -> 'a t
  (** Returns a fresh empty work-stealing queue *)

  val is_empty : 'a t -> bool
  (** [is_empty q] returns empty if [q] is empty. *)

  val size : 'a t -> int
  (** [size q] returns the number of elements currently stored in the queue.
      It should only be invoked by the domain which owns the queue [q]. *)

  val push : 'a t -> 'a -> unit
  (** [push q v] pushes [v] to the back of the queue.
      It should only be invoked by the domain which owns the queue [q]. *)

  val pop : 'a t -> 'a
  (** [pop q] removes an element [e] from the front of the queue and returns
      it. It should only be invoked by the domain which owns the queue [q].

      @raise [Exit] if the queue is empty.
      *)

  val steal : 'a t -> 'a
  (** [steal q] removes an element from the back of the queue and returns
      it. It should only be invoked by domain which doesn't own the queue [q].

      @raise [Exit] if the queue is empty.
      *)

    end

module M : S;;
