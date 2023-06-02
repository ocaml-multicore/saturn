(* A lock-free single-producer multi-consumer queue. It has been written
   with work-stealing scheduling in mind.

   The functions whose names start with local_ cannot be invoked by a domain
   different to owner. That's because [local_push], [local_pop] linearize
   with [steal_one] but not with each other. This assumption helps improve
   performance but when broken the structure will misbehave in unexpected
   ways. For a multi-producer multi-consumer FIFO structure see
   Michael-Scott Queue.

   [local_push] and [local_pop] are wait-free. [steal_one] is lock-free.
*)
type 'a t

(* Create queue of size 2^size_exponent. *)
val create : size_exponent:int -> unit -> 'a t

(* [local_push t v] insert item [v] into the queue. To be called by owner
    domain only. *)
val local_push : 'a t -> 'a -> bool

(* [local_pop t] pops an item from the queue. To be called by owner domain
   only. *)
val local_pop : 'a t -> 'a option

(* [steal_one t] pops one item from the queue. *)
val steal_one : 'a t -> 'a option
