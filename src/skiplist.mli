(** A lock-free skiplist. *)

(** {1 API}*)

type (!'k, !'v) t
(** The type of a lock-free skiplist containing bindings of keys of type ['k] to
    values of type ['v]. *)

val create : ?max_height:int -> compare:('k -> 'k -> int) -> unit -> ('k, 'v) t
(** [create ~compare ()] creates a new empty skiplist where keys are ordered
    based on the given [compare] function.

    Note that the polymorphic [Stdlib.compare] function has relatively high
    overhead and it is usually better to use a type specific [compare] function
    such as [Int.compare] or [String.compare].

    The optional [max_height] argument determines the maximum height of nodes in
    the skiplist and directly affects the performance of the skiplist.  The
    current implementation does not adjust height automatically.
    [max_height] can be more than 30. *)

val max_height_of : ('k, 'v) t -> int
(** [max_height_of s] returns the maximum height of nodes of the skiplist [s] as
    specified to {!create}. *)

val length : ('k, 'v) t -> int
(** [length s] computes the number of bindings in the skiplist [s]. *)

(** {2 Looking up bindings} *)

val find_opt : ('k, 'v) t -> 'k -> 'v option
(** [find_opt s k] tries to find a binding of [k] to [v] from the skiplist [s]
    and returns [Some v] in case such a binding was found or return [None] in
    case no such binding was found. *)

val mem : ('k, 'v) t -> 'k -> bool
(** [mem s k] determines whether the skiplist [s] contained a binding of [k]. *)

(** {2 Adding bindings} *)

val try_add : ('k, 'v) t -> 'k -> 'v -> bool
(** [try_add s k v] tries to add a new binding of [k] to [v] into the skiplist
    [s] and returns [true] on success.  Otherwise the skiplist already contained
    a binding of [k] and [false] is returned. *)

val try_remove : ('k, 'v) t -> 'k -> bool
(** [try_remove s k] tries to remove a binding of [k] from the skiplist and
    returns [true] on success.  Otherwise the skiplist did not contain a binding
    of [k] and [false] is returned. *)

(** {1 Examples} *)

(** {2 Sequential example} 

{[
    # open Saturn.Skiplist
    # let t = create ~compare:Int.compare ()
    val t : (int, '_weak1) t = <abstr>
    # try_add t 42 "The answer"
    - : bool = true

    # try_add t 101 "Basics"
    - : bool = true

    # find_opt t 42
    - : string option = Some "The answer"

    # try_add t 101 "The basics"
    - : bool = false

    # try_remove t 101
    - : bool = true
]}
*)

(** {2 Multicore example}

  **Note**: The barrier is used in this example solely to make the results more
  interesting by increasing the likelihood of parallelism. Spawning a domain is 
  a costly operation, especially compared to the relatively small amount of work
  being performed here. In practice, using a barrier in this manner is unnecessary.

{[
    # open Saturn.Skiplist
    # let t : (int, int) t= create ~compare:Int.compare ()
    val t : (int, int) t = <abstr>
    # Random.self_init ()
    - : unit = ()
    # let barrier = Atomic.make 2
    val barrier : int Atomic.t = <abstr>

    # let work () = 
        Atomic.decr barrier;
        while Atomic.get barrier > 0 do () done;
        for i = 0 to 10 do
            if Random.bool () then 
                try_add t i i |> ignore
            else 
                try_remove t i |> ignore
        done
    val work : unit -> unit = <fun>

    # let d1 = Domain.spawn work
    val d1 : unit Domain.t = <abstr>
    # let d2 = Domain.spawn work
    val d2 : unit Domain.t = <abstr>
    # Domain.join d1; Domain.join d2
    - : unit = ()
]}
*)
