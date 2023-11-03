open Kcas

(** Synchronizing variable.

    A synchronizing variable is essentially equivalent to a ['a option Loc.t]
    with blocking semantics on both {!take} and {!put}.

    {b NOTE}: The current implementation is not guaranteed to be fair or
    scalable.  In other words, when multiple producers block on {!put} or
    multiple consumers block on {!take} the operations are not queued and it is
    possible for a particular producer or consumer to starve. *)

(** {1 Common interface} *)

type !'a t
(** The type of a synchronizing variable that may contain a value of type
    ['a]. *)

val create : 'a option -> 'a t
(** [create x_opt] returns a new synchronizing variable that will either be
    empty when [x_opt] is [None] or full when [x_opt] is [Some x]. *)

(** {1 Compositional interface} *)

module Xt :
  Mvar_intf.Ops
    with type 'a t := 'a t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction passing on synchronizing variables. *)

(** {1 Non-compositional interface} *)

include Mvar_intf.Ops with type 'a t := 'a t with type ('x, 'fn) fn := 'fn
