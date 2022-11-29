(** A lock-free single-producer, multi-consumer dynamic-size double-ended queue (deque). 

    The main strength of deque in a typical work-stealing setup with per-core structure
    is efficient work distribution. Owner uses [push] and [pop] method to operate at 
    one end of the deque, while other (free) cores can efficiently steal work on the 
    other side. 
    
    This approach is great for throughput. Stealers and owner working on different sides 
    reduces contention in work distribution. Further, local LIFO order runs related tasks 
    one after one improves locality. 

    On the other hand, the local LIFO order does not offer any fairness guarantees. 
    Thus, it is not the best choice when tail latency matters. 
*)

module type S = sig
  type 'a t
  (** Type of work-stealing queue *)

  val create : unit -> 'a t
  (** Returns a fresh empty work-stealing queue *)

  val push : 'a t -> 'a -> unit
  (** [push q v] pushes [v] to the front of the queue.
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

module M : S
