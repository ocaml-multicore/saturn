(** Lock-free bounded stack.

    This module implements a lock-free bounded stack based on Treiber's stack
    algorithm. Adding a capacity to this algorithm adds a general overhead to
    the operations, and thus, it is recommended to use the unbounded stack
    {!Saturn.Stack} if neither the capacity nor the {!length} function is
    needed. *)

(** {1 API} *)

type 'a t
(** Represents a lock-free bounded stack holding elements of type ['a]. *)

val create : ?capacity:int -> unit -> 'a t
(** [create ~capacity ()] creates a new empty bounded stack with a maximum
    capacity of [capacity]. The default [capacity] value is [Int.max_int]. *)

val of_list_exn : ?capacity:int -> 'a list -> 'a t
(** [of_list_exn list] creates a new Treiber stack from a list.

    @raise Full if the [list] is longer than the capacity of the stack.

    🐌 This is a linear-time operation.
    {[
      # open Saturn.Bounded_stack
      # let t : int t = of_list_exn [1;2;3;4]
      val t : int t = <abstr>
      # pop_opt t
      - : int option = Some 4
      # pop_opt t
      - : int option = Some 3
      # length t
      - : int = 2
    ]} *)

val length : 'a t -> int
(** [length stack] returns the number of elements currently in the [stack]. *)

val is_empty : 'a t -> bool
(** [is_empty stack] returns [true] if the [stack] is empty, otherwise [false].
*)

val is_full : 'a t -> bool
(** [is_full stack] returns [true] if the [stack] has reached capacity,
    otherwise [false]. *)

(** {2 Consumer functions} *)

exception Empty
(** Raised when {!pop_exn}, {!peek_exn}, or {!drop_exn} is applied to an empty
    stack. *)

val peek_exn : 'a t -> 'a
(** [peek_exn stack] returns the top element of the [stack] without removing it.

    @raise Empty if the [stack] is empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt stack] returns [Some] of the top element of the [stack] without
    removing it, or [None] if the [stack] is empty. *)

val pop_exn : 'a t -> 'a
(** [pop_exn stack] removes and returns the top element of the [stack].

    @raise Empty if the [stack] is empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt stack] removes and returns [Some] of the top element of the
    [stack], or [None] if the [stack] is empty. *)

val drop_exn : 'a t -> unit
(** [drop_exn stack] removes the top element of the [stack].

    @raise Empty if the [stack] is empty. *)

val pop_all : 'a t -> 'a list
(** [pop_all stack] removes and returns all elements of the [stack] in LIFO
    order.

    {[
      # open Saturn.Bounded_stack
      # let t : int t = create ()
      val t : int t = <abstr>
      # try_push t 1
      - : bool = true
      # try_push t 2
      - : bool = true
      # try_push t 3
      - : bool = true
      # pop_all t
      - : int list = [3; 2; 1]
    ]} *)

(** {2 Producer functions} *)

exception Full
(** Raised when {!push_exn} or {!push_all_exn} is applied to a full stack. *)

val push_exn : 'a t -> 'a -> unit
(** [push_exn stack element] adds [element] to the top of the [stack].

    @raise Full if the [stack] is full. *)

val try_push : 'a t -> 'a -> bool
(** [try_push stack element] tries to add [element] to the top of the [stack].
    Returns [true] if the element was successfully added, or [false] if the
    stack is full. *)

val push_all_exn : 'a t -> 'a list -> unit
(** [push_all_exn stack elements] adds all [elements] to the top of the [stack].

    @raise Full if the [stack] is full.

    🐌 This is a linear-time operation on the size of [elements]. *)

val try_push_all : 'a t -> 'a list -> bool
(** [try_push_all stack elements] tries to add all [elements] to the top of the
    [stack]. Returns [true] if the elements were successfully added, or [false]
    if the stack is full.

    🐌 This is a linear-time operation on the size of [elements].

    {[
      # let t : int t = create ()
      val t : int t = <abstr>
      # try_push_all t [1; 2; 3; 4]
      - : bool = true
      # pop_opt t
      - : int option = Some 4
      # pop_opt t
      - : int option = Some 3
      # pop_all t
      - : int list = [2; 1]
    ]} *)

(** {2 With Sequences}*)
val to_seq : 'a t -> 'a Seq.t
(** [to_seq stack] takes a snapshot of [stack] and returns its value from top to
    bottom.

    🐌 This is a linear time operation. *)

val of_seq : ?capacity:int -> 'a Seq.t -> 'a t
(** [of_seq seq] creates a stack from a [seq]. It must be finite.

    @raise Full if the [seq] is longer than the capacity of the stack.

    🐌 This is a linear-time operation. *)

val add_seq_exn : 'a t -> 'a Seq.t -> unit
(** [add_seq_exn stack seq] adds all elements of [seq] to the top of the
    [stack]. [seq] must be finite.

    @raise Full if the [seq] is too long to fit in the stack.

    🐌 This is a linear-time operation on the size of [seq]. *)

val try_add_seq : 'a t -> 'a Seq.t -> bool
(** [try_add_seq stack seq] tries to add all elements of [seq] to the top of the
    [stack]. Returns [true] if the elements were successfully added, or [false]
    if the [seq] is too long to fit in the stack.

    🐌 This is a linear-time operation. *)

(** {1 Examples} *)

(** {2 Sequential example}
    An example top-level session:
    {[
      # open Saturn.Bounded_stack
      # let t : int t = create ()
      val t : int t = <abstr>
      # try_push t 42
      - : bool = true
      # push_exn t 1
      - : unit = ()
      # pop_exn t
      - : int = 1
      # peek_opt t
      - : int option = Some 42
      # pop_opt t
      - : int option = Some 42
      # pop_opt t
      - : int option = None
      # pop_exn t
      Exception: Saturn__Bounded_stack.Empty.
    ]} *)

(** {2 Multicore example}
    Note: The barrier is used in this example solely to make the results more
    interesting by increasing the likelihood of parallelism. Spawning a domain
    is a costly operation, especially compared to the relatively small amount of
    work being performed here. In practice, using a barrier in this manner is
    unnecessary.

    {@ocaml non-deterministic=command[
      # open Saturn.Bounded_stack
      # let t :int t = create ()
      val t : int t = <abstr>
      # let barrier =  Atomic.make 2
      val barrier : int Atomic.t = <abstr>

      # let pusher () =
        Atomic.decr barrier;
        while Atomic.get barrier != 0 do Domain.cpu_relax () done;
        try_push_all t [1;2;3] |> ignore;
        push_exn t 42;
        push_exn t 12
      val pusher : unit -> unit = <fun>

      # let popper () =
        Atomic.decr barrier;
        while Atomic.get barrier != 0 do Domain.cpu_relax () done;
        List.init 6 (fun i -> Domain.cpu_relax (); pop_opt t)
      val popper : unit -> int option list = <fun>

      # let domain_pusher = Domain.spawn pusher
      val domain_pusher : unit Domain.t = <abstr>
      # let domain_popper = Domain.spawn popper
      val domain_popper : int option list Domain.t = <abstr>
      # Domain.join domain_pusher
      - : unit = ()
      # Domain.join domain_popper
      - : int option list = [Some 42; Some 3; Some 2; Some 1; None; Some 12]
    ]} *)
