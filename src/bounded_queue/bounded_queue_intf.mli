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
      
    {[
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
