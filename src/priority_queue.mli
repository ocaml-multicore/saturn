(** A lock-free priority queue.

    This is based on a fixed-sized skiplist implementation:
    - add operations have an average logarithmic complexity as long as the
      number of elements in it is less than `2^max_height`. Passed that,
      performance will decrease.

    - remove operations (with `remove_min_opt`) have a o(1) complexity. *)

type (!'p, !'v) t
(** The type of a lock-free priority queue containing elements of type [`v]
    bound to priority of type ['p]. *)

val create : ?max_height:int -> compare:('p -> 'p -> int) -> unit -> ('p, 'v) t
(** [create ~compare ()] creates a new empty priority queue where prioriries are
    ordered based on the given [compare] function.

    Note that the polymorphic [Stdlib.compare] function has relatively high
    overhead and it is usually better to use a type specific [compare] function
    such as [Int.compare] or [String.compare].

    The implementation is based on a skiplist. The optional [max_height]
    argument determines the maximum height of nodes in the skiplist and directly
    affects the performance of the data structure. The current implementation
    does not adjust height automatically. *)

val max_height_of : ('p, 'v) t -> int
(** [max_height_of pq] returns the maximum height of nodes of the priority queue
    [pq] as specified to {!create}.

    The maximum number of elements that the prioriry queue can contain before,
    statistically, the performance decreases is `2^(max_height)`. *)

val add : ('p, 'v) t -> 'p -> 'v -> unit
(** [add pq p v] adds a new element [v] with priority [p] into the priority
    queue [pq] skiplist [s] and returns [true] on success. *)

val remove_min_opt : ('p, 'v) t -> ('p * 'v) option
(** [remove_min_opt s] removes and returns [Some] of the element with the
    smallest priority from the priority queue [pq]. If the priority queue [pq]
    was empty [None] is returned. The elements with the same priority are
    removed in fifo order. *)

val length : ('k, 'v) t -> int
(** [length pq] computes and returns the number of elements in the priority
    queue [pq].

    This function is not linearizable and should only be used to get an
    indication of the size of [pq]. *)
