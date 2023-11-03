open Kcas

(** Scalable accumulator.

    A scalable accumulator can be used to scalably accumulate an integer value
    in parallel as long as the accumulated value is read infrequently. *)

(** {1 Common interface} *)

type t
(** The type of a scalable accumulator. *)

val make : ?n_way:int -> int -> t
(** [make n] returns a new accumulator whose initial value is [n].

    The optional [n_way] argument can be used to specify a desired level of
    parallelism, i.e. maximum number of non-interfering parallel updates.  The
    default value is chosen to strike a balance between scalability and memory
    use and a given value may be adjusted by the implementation. *)

val n_way_of : t -> int
(** [n_way_of a] returns the maximum number of non-interfering parallel updates
    supported by the accumulator [a].

    {b NOTE}: The returned value may not be the same as given to {!make}. *)

(** {1 Compositional interface} *)

module Xt :
  Accumulator_intf.Ops
    with type t := t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on accumulators. *)

(** {1 Non-compositional interface} *)

include Accumulator_intf.Ops with type t := t with type ('x, 'fn) fn := 'fn
