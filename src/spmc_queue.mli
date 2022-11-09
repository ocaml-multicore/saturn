(* add lifo slot? *)

type 'a t = {
  head : int Atomic.t;
  mask : int;
  size_exponent : int;
  array : 'a option Atomic.t Array.t;
  tail : int Atomic.t;
}

val create : size_exponent:int -> unit -> 'a t

(* [Local] functions here can only be called from the thread owning given queue
  (at a given time). In the case of [steal], [from] parameter is unrestricted.    

  This restriction improves performance as [push], [pop], [resize] need to 
  linearize only with [steal]. 
*)
module Local : sig
  val push : 'a t -> 'a -> bool
  val pop : 'a t -> 'a option
  val steal : from:'a t -> 'a t -> int
  val steal_one : 'a t -> 'a

  val is_empty : 'a t -> bool
  (** [is_empty] thoroughly checks if there's any elements in the array.
    Call before [steal], which assumes there's room in the queue. *)  
end

module Resizable : sig 
  type 'a t 
  
  val create : size_exponent:int -> unit -> 'a t

  (* [Local] functions to be called only from the owner thread. *)
  module Local : sig 
    val push : 'a t -> 'a -> bool
    val pop : 'a t -> 'a option
    val steal : from:'a t -> 'a t -> int
    val steal_one : 'a t -> 'a

    val is_empty : 'a t -> bool
    (** [is_empty] thoroughly checks if there's any elements in the array.
      Call before [steal], which assumes there's room in the queue. *)  

    val resize : 'a t -> unit
    val push_with_autoresize : 'a t -> 'a -> unit
  end
end

(** [indicative_size] gives some idea about the size of the queue. 
    It's actual queue size when called from enqueuer thread (or while 
    enqueuer is not inserting elements). *)
val indicative_size : 'a t -> int

(* [M] is the interface for Domainslib *)
module M : sig 
  include Ws_deque.S with type 'a t := 'a Resizable.t
end