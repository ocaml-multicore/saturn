(** Lock-free bounded stack. *)

(** {1 API} *)

type 'a t
(** Represents a lock-free bounded stack holding elements of type ['a]. *)

val create : ?capacity:int -> unit -> 'a t
(** [create ~capacity ()] creates a new empty bounded stack with a maximum capacity of [capacity]. Default [capacity] value is [Int.max_int].
*)

val length : 'a t -> int
(** [length stack] returns the number of elements currently in the [stack]. *)

val is_empty : 'a t -> bool
(** [is_empty stack] returns [true] if the [stack] is empty, otherwise [false]. *)

(** {2 Consumer functions} *)
val peek : 'a t -> 'a
(** [peek stack] returns the top element of the [stack] without removing it. If the stack is empty, the domain will be suspended until another domain successfully push another element in the stack with [push] or [push_opt] . *)

val peek_opt : 'a t -> 'a option
(** [peek_opt stack] returns [Some] of the top element of the [stack] without
    removing it, or [None] if the stack is empty. *)

val pop : 'a t -> 'a
(** [pop stack] removes and returns the top element of the [stack].
    Raises an exception if the stack is empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt stack] removes and returns [Some] of the top element of the [stack],
    or [None] if the stack is empty. *)

(** {2 Producer functions} *)

val push : 'a t -> 'a -> unit
(** [push stack element] adds [element] to the top of the [stack].
    Raises an exception if the stack is full. *)

val try_push : 'a t -> 'a -> bool
(** [try_push stack element] tries to add [element] to the top of the [stack].
    Returns [true] if the element was successfully added, or [false] if the
    stack is full. *)

(** {1 Examples}
    An example top-level session:
    {[
      # let t : int Saturn.Bounded_stack.t =
        Saturn.Bounded_stack.create ()
      val t : int Saturn.Bounded_stack.t = <abstr>
      # Saturn.Bounded_stack.try_push t 42
      - : bool = true
      # Saturn.Bounded_stack.add t 1
      - : unit = ()
      # Saturn.Bounded_stack.pop t
      - : int = 1
      # Saturn.Bounded_stack.peek_opt t
      - : int option = Some 42
      # Saturn.Bounded_stack.pop_opt t
      - : int option = Some 42 
      # Saturn.Bounded_stack.pop_opt t
      - : int option = None ]}

    A multicore example:
    {[
    # open Saturn
    # let t : int Saturn.Bounded_stack.t = 
         Saturn.Bounded_stack.create ~capacity:8 ()
    val t : int Saturn.Bounded_stack.t = <abstr>
    # let p = Domain.spawn 
        (fun () -> for i = 0 to 10 do 
                     Bounded_stack.push t i;
                     Printf.printf "Producer: %d\n" i
                    done)
    Producer: 0
    Producer: 1
    Producer: 2
    Producer: 3
    Producer: 4
    Producer: 5
    Producer: 6
    Producer: 7
    val p : unit Domain.t = <abstr>
    # let c = Domain.spawn 
        (fun () -> for i = 0 to 10 do
                     Printf.printf "Consumer: %d\n" (Bounded_stack.pop t)
                   done)
    Consumer: 4
    Consumer: 3
    Consumer: 2
    Consumer: 1
    Consumer: 0
    val c : unit Domain.t = <abstr>
    # Domain.join p; Domain.join c
    - : unit = () ]} 

{[
    let test () = 
        let s = Bounded_stack.create ~capacity:8 () in
        let p = Domain.spawn (fun () -> for i = 0 to 10 do 
                                        Bounded_stack.push s i;
                                        Printf.printf "Producer: %d\n" i
                                      done) in
        let c = Domain.spawn (fun () -> for i = 0 to 10 do
                                    Printf.printf "Consumer: %d\n" (Bounded_stack.pop s)
                                    done) in
         Domain.join p; Domain.join c


    ]}
    *)
