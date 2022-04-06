module type S = sig

  type 'a t
  (** Type of work-stealing queue *)

  val create : unit -> 'a t
  (** Returns a fresh empty work-stealing queue *)

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
