(* Copyright (c) 2023-2024, Vesa Karvonen <vesa.a.j.k@gmail.com>
   Copyright (c) 2024, Carine Morel <carine.morel.pro@gmail.com>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

module type BOUNDED_QUEUE = sig
  (** Lock-free bounded Queue. 

    This module implements a lock-free bounded queue based on Michael-Scott's queue 
    algorithm. Adding a capacity to this algorithm adds a general overhead to the
     operations, and thus, it is recommended to use the unbounded queue 
     {!Saturn.Queue} if you don't need it.
  *)

  (** {1 API} *)

  type 'a t
  (** Represents a lock-free bounded queue holding elements of type ['a]. *)

  val create : ?capacity:int -> unit -> 'a t
  (** [create ~capacity ()] creates a new empty bounded queue with a maximum 
    capacity of [capacity]. The default [capacity] value is [Int.max_int].*)

  val of_list_exn : ?capacity:int -> 'a list -> 'a t
  (** [of_list_exn ~capacity list] creates a new queue from a list.
    
    @raises Full if the length of [list] is greater than [capacity]. 
  
    🐌 This is a linear-time operation.
      
    {[
      # open Saturn.Bounded_queue
      # let t : int t = of_list_exn [1;2;3;4]
      val t : int t = <abstr>
      # pop_opt t
      - : int option = Some 1
      # pop_opt t 
      - : int option = Some 2
      # length t
      - : int = 2
    ]}
    *)

  val length : 'a t -> int
  (** [length queue] returns the number of elements currently in the [queue]. *)

  val capacity_of : 'a t -> int
  (** [capacity_of queue] returns the maximum number of elements that the [queue]
      can hold. *)

  val is_empty : 'a t -> bool
  (** [is_empty queue] returns [true] if the [queue] is empty, otherwise [false]. *)

  val is_full : 'a t -> bool
  (** [is_full queue] returns [true] if the [queue] is full, otherwise [false]. *)

  (** {2 Consumer functions} *)

  exception Empty
  (** Raised when {!pop_exn}, {!peek_exn}, or {!drop_exn} is applied to an empty
   stack. *)

  val peek_exn : 'a t -> 'a
  (** [peek_exn queue] returns the first element of the [queue] without removing it.
      
    @raises Empty if the [queue] is empty. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt queue] returns [Some] of the first element of the [queue] without
      removing it, or [None] if the [queue] is empty. *)

  val pop_exn : 'a t -> 'a
  (** [pop_exn queue] removes and returns the first element of the [queue].
   
    @raises Empty if the [queue] is empty. *)

  val pop_opt : 'a t -> 'a option
  (** [pop_opt queue] removes and returns [Some] of the first element of the [queue],
      or [None] if the [queue] is empty. *)

  val drop_exn : 'a t -> unit
  (** [drop_exn queue] removes the top element of the [queue]. 
  
    @raises Empty if the [queue] is empty. *)

  (** {2 Producer functions} *)

  exception Full
  (** Raised when {!push_exn} or {!push_all_exn} is applied to a full queue. *)

  val push_exn : 'a t -> 'a -> unit
  (** [push_exn queue element] adds [element] at the end of the [queue].
      
    @raises Full if the [queue] is full. *)

  val try_push : 'a t -> 'a -> bool
  (** [try_push queue element] tries to add [element] at the end of the [queue].
      Returns [true] if the element was successfully added, or [false] if the
      queue is full. *)
end

(** {1 Examples}
    An example top-level session:
    {[
      # open Saturn.Bounded_queue
      # let t : int t = create ~capacity:3 ()
      val t : int t = <abstr>
      # try_push t 1
      - : bool = true
      # try_push t 2
      - : bool = true
      # push_exn t 3
      - : unit = ()
      # push_exn t 4
      Exception: Saturn__Bounded_queue.Full.
      # try_push t 4 
      - : bool = false
      # pop_exn t
      - : int = 1
      # peek_opt t
      - : int option = Some 2
      # drop_exn t
      - : unit = ()
      # pop_opt t
      - : int option = Some 3
      # pop_opt t
      - : int option = None
      # pop_exn t
      Exception: Saturn__Bounded_queue.Empty.]}

    A multicore example: 
    {@ocaml non-deterministic[
      # open Saturn.Bounded_queue
      # let t :int t = create ~capacity:4 ()
      val t : int t = <abstr>

      # let barrier =  Atomic.make 2
      val barrier : int Atomic.t = <abstr>

      # let pusher () = 
          Atomic.decr barrier;
          while Atomic.get barrier != 0 do Domain.cpu_relax () done;
          List.init 8 (fun i -> i)
          |> List.map (fun i -> Domain.cpu_relax (); try_push t i)
      val pusher : unit -> bool list = <fun>

      # let popper () = 
          Atomic.decr barrier;
          while Atomic.get barrier != 0 do Domain.cpu_relax () done; 
          List.init 8 (fun i -> Domain.cpu_relax (); pop_opt t)
      val popper : unit -> int option list = <fun>

      # let domain_pusher = Domain.spawn pusher
      val domain_pusher : bool list Domain.t = <abstr>

      # let domain_popper = Domain.spawn popper
      val domain_popper : int option list Domain.t = <abstr>

      # Domain.join domain_pusher
      - : bool list = [true; true; true; true; true; false; true; true]
      # Domain.join domain_popper
      - : int option list = [None; None; Some 0; None; Some 1; Some 2; Some 3; Some 4]
      ]}
 
    *)
