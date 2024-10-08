(** Lock-free bounded stack. *)

(** {1 API} *)

type 'a t
(** Represents a lock-free bounded stack holding elements of type ['a]. *)

val create : ?capacity:int -> unit -> 'a t
(** [create ~capacity ()] creates a new empty bounded stack with a maximum 
capacity of [capacity]. Default [capacity] value is [Int.max_int].
*)

val length : 'a t -> int
(** [length stack] returns the number of elements currently in the [stack]. *)

val is_empty : 'a t -> bool
(** [is_empty stack] returns [true] if the [stack] is empty, otherwise [false]. *)

(** {2 Consumer functions} *)
val peek : 'a t -> 'a
(** [peek stack] returns the top element of the [stack] without removing it. If 
the stack is empty, the domain will be suspended until another domain successfully 
push another element in the stack with [push] or [push_opt] . *)

val peek_opt : 'a t -> 'a option
(** [peek_opt stack] returns [Some] of the top element of the [stack] without
    removing it, or [None] if the stack is empty. *)

val pop : 'a t -> 'a
(** [pop stack] removes and returns the top element of the [stack].
    Raises an exception if the stack is empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt stack] removes and returns [Some] of the top element of the [stack],
    or [None] if the stack is empty. *)

val pop_all : 'a t -> 'a list
(** [pop_all stack] removes and returns all elements of the [stack] in the reverse 
order they were pushed. *)

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
      # Saturn.Bounded_stack.push t 1
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
    open Saturn_lockfree
    open Picos_std_structured

    let main () =
      let st = Bounded_stack.create ~capacity:2 () in
      let popped = Bounded_stack.create ~capacity:Int.max_int () in
      Flock.join_after
        begin
          fun () ->
            for i = 0 to 2 lsl 5 - 1 do
              Flock.fork @@ fun () ->
              begin
                if i/4 mod 2 = 0 then (* 4 pushes followed by 4 pops *)
                  let id = (Domain.self () :> int) in
                  Bounded_stack.push st id
                else Bounded_stack.pop st |> Bounded_stack.push popped 
               (* stores the result of pop in popped *)
              end;
              Unix.sleepf (Random.float 0.1)
            done
        end;
    assert (Bounded_stack.pop_all st = []);
    Bounded_stack.pop_all popped |> List.rev

    let run () =
      Picos_mux_multififo.run_on ~n_domains:4 main
    ]}
    This example uses Picos' prefined {{:https://ocaml-multicore.github.io/picos/doc/picos_mux/Picos_mux_multififo/index.html}multi-threaded scheduler}
    to run four domains that are alternatively pushing their ids and popping in
    a shared stack with a capacity of 2 elements. The returned list is the 
    pusher's ids in order. Note that with this scheduler, a maximum of 4 domains 
    can be used in parallel and each domain can spawn multiple fibers, meaning 
    even if run in a single domain, this example will not block indefinitely.

    {[ 
    # run ();;
    - : int list =
    [5; 4; 6; 6; 4; 5; 4; 0; 6; 5; 6; 4; 0; 5; 4; 5; 0; 5; 6; 5; 5; 0; 5; 4;
     4; 0; 0; 0; 0; 0; 0; 0]
    ]}
    *)
