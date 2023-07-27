(** 
    Lockfree Priority Queue implementation based on skiplist. The references include 
    chapter 14 & 15 in art of multiprocessor programming as well as the following {
    {:http://people.csail.mit.edu/shanir/publications/Priority_Queues.pdf} research paper} 
*)

type t
(** the type of priority queue *)

val create : ?max_height:int -> unit -> t
(** create new pq with given height *)

val push : t -> int -> unit
(** [push pq ele] adds [ele] to it's sorted position in [pq] *)

val pop : t -> int
(** [pop pq] removes smallest elements from [pq] *)

val contains : t -> int -> bool
(** [contains pq ele] checks if [ele] exists in [pq] *)