(* add lifo slot? *)

type 'a t = private {
  head : int Atomic.t;
  tail : int Atomic.t;
  mask : int Atomic.t;
  array : 'a option Atomic.t Array.t Atomic.t;
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
  val resize : 'a t -> unit
  val push_with_autoresize : 'a t -> 'a -> unit
  val steal : from:'a t -> 'a t -> int

  val is_empty : 'a t -> bool
  (** [is_empty] thoroughly checks if there's any elements in the array.
    Call before [steal], which assumes there's room in the queue. *)
end

(** [indicative_size] gives some idea about the size of the queue. 
    It's actual queue size when called from enqueuer thread (or while 
    enqueuer is not inserting elements). *)
val indicative_size : 'a t -> int

(* [M] is the interface for Domainslib *)
module M : Ws_deque.S 