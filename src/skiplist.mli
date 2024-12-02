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
(** [find_opt sl key] returns [Some] of the current binding of [key] in the 
    skiplist [sl] or [None] if it does not exist. *)

val find_exn : ('k, 'v) t -> 'k -> 'v
(** [find_exn sl key] returns the current binding of [key] in the skiplist
    [sl] or raises {!Not_found} if no such binding exists.

    @raise Not_found if no binding of [key] exists in the skiplist [sl]. *)

val mem : ('k, 'v) t -> 'k -> bool
(** [mem sl k] determines whether the skiplist [sl] contained a binding of [k]. *)

(** {2 Adding bindings} *)

val try_add : ('k, 'v) t -> 'k -> 'v -> bool
(** [try_add sk key value] tries to add a new binding of [key] to [value] to
  the skiplist [sl]. Returns [true] on success and [false] if the skiplist 
  already contains a binding for [key]. *)

val try_remove : ('k, 'v) t -> 'k -> bool
(** [try_remove sl key] tries to remove a binding of [key] from the skiplist [sl].
    Returns [true] on success and [false] if the skiplist does not contain a 
    binding for [key]. *)

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
