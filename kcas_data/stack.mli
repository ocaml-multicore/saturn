open Kcas

(** Last-In First-Out (LIFO) stack.

    The interface provides a subset of the OCaml [Stdlib.Stack] module.
    [add_seq] is not provided at all.  Compositional versions of {!iter},
    {!fold}, {!pop}, and {!top} are not provided.

    The implementation is essentially a
    {{:https://en.wikipedia.org/wiki/Treiber_stack}Treiber stack} with
    randomized exponential backoff and support for constant time {!length}. *)

(** {1 Common interface} *)

type !'a t
(** The type of stacks containing elements of type ['a]. *)

exception Empty
(** Raised when {!pop} or {!top} is applied to an empty stack. *)

val create : unit -> 'a t
(** [create ()] returns a new empty stack. *)

val copy : 'a t -> 'a t
(** [copy s] returns a copy of the stack [s]. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq xs] creates a stack from the sequence [xs]. *)

(** {1 Compositional interface} *)

module Xt :
  Stack_intf.Ops
    with type 'a t := 'a t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on stacks. *)

(** {1 Non-compositional interface} *)

include Stack_intf.Ops with type 'a t := 'a t with type ('x, 'fn) fn := 'fn

val pop : 'a t -> 'a
(** [pop s] removes and returns the topmost element in stack [s], or raises
    {!Empty} if the stack is empty. *)

val top : 'a t -> 'a
(** [top s] returns the topmost element in stack [s], or raises {!Empty} if the
    stack is empty. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f s] is equivalent to [Seq.iter f (to_seq s)]. *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold f s] is equivalent to [Seq.fold_left f a (to_seq s)]. *)
