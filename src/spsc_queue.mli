type 'a t 

(* [create] initializes a single-producer single-consumer thread-safe queue. *)
val create : size_exponent:int -> 'a t

(* [enqueue] insert element into the queue. This method can be used by at most 1 
  thread at the time. *)
val enqueue : 'a t -> 'a -> bool 


(* [dequeue] removes element from the queue, if any. This method can be used by 
  at most 1 thread at the time. *)
val dequeue : 'a t -> 'a option

(* [size] returns the size of the queue. This method linearizes only when called 
  from either enqueuer or dequeuer thread. Otherwise, it is safe to call but 
  provides only an *indication* of the size of the structure. 
*)
val size : 'a t -> int