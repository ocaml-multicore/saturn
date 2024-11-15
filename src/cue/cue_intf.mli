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

module type CUE = sig
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
capacity of [capacity]. The default [capacity] value is [Int.max_int].
*)

  val of_list : ?capacity:int -> 'a list -> 'a t
  (** [of_list list] creates a new queue from a list.
  
  @raises Full is the lenght of [list] is greater tant [capacity]. *)

  val length : 'a t -> int
  (** [length queue] returns the number of elements currently in the [queue]. *)

  val capacity_of : 'a t -> int
  (** [capacity_of queue] returns the maximum number of elements that the [queue]
    can hold. *)

  val is_empty : 'a t -> bool
  (** [is_empty queue] returns [true] if the [queue] is empty, otherwise [false]. *)

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

  (*
  val try_compare_and_pop : 'a t -> 'a -> bool
  (** [try_compare_and_pop stack before] tries to remove the top element of the 
  [stack] if it is equal to [before]. Returns [true] on success and [false] in 
  case the stack is empty or if the top element is not equal to [before].
    ℹ️ The values are compared using physical equality, i.e., the [==] operator. *)
*)

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

  (*
  val push_all_exn : 'a t -> 'a list -> unit
  (** [push_all_exn queue elements] adds all [elements] at the end of the [queue].
    
  @raises Full if the [stack] is full. *)

  val try_push_all : 'a t -> 'a list -> bool
  (** [try_push_all stack elements] tries to add all [elements] to the top of the 
    [stack]. Returns [true] if the elements were successfully added, or [false] if 
    the stack is full. 
    
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
  ]}
    *)
*)

  (*

  (** {3 Updating bindings} *)

  val try_set : 'a t -> 'a -> bool
  (** [try_set stack value] tries to update the top element of the [stack] to
    [value]. Returns [true] on success and [false] if the [stack] is empty.
    *)

  val try_compare_and_set : 'a t -> 'a -> 'a -> bool
  (** [try_compare_and_set stack before after] tries to update the top element of 
the [stack] from the [before] value to the [after] value. Returns [true] on 
success and [false] if the [stack] is empty or the top element is not equal 
to [before].
    ℹ️ The values are compared using physical equality, i.e., the [==]
    operator. *)

  val set_exn : 'a t -> 'a -> 'a
  (** [set_exn stack after] tries to update the top element of [stack] from some 
[before] value to the [after] value. Returns the [before] value on success.
    @raises Empty if the [stack] is empty. *)

  (** {2 With Sequences }*)
  val to_seq : 'a t -> 'a Seq.t
  (** [to_seq stack] takes a snapshot of [stack] and returns its value from top to 
bottom.
  🐌 This is a linear time operation. *)

  val of_seq : ?capacity:int -> 'a Seq.t -> 'a t
  (** [of_seq seq] creates a stack from a [seq]. It must be finite. *)

  val add_seq_exn : 'a t -> 'a Seq.t -> unit
  (** [add_seq_exn stack seq] adds all elements of [seq] to the top of the 
[stack]. [seq] must be finite. 
@raises Full if the [seq] is too long to fit in the stack. *)

  val try_add_seq : 'a t -> 'a Seq.t -> bool
  (** [try_add_seq stack seq] tries to add all elements of [seq] to the top of the
[stack]. Returns [true] if the elements were successfully added, or [false] if 
the [seq] is too long to fit in the stack.  *)
*)
end
