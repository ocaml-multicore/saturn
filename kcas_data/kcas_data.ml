(** This package provides a collection of compositional lock-free
    data structures with in-place modification.

    All data structure implementations in this library should strive to provide
    the following guarantees:

    - Provided operations are {i strictly serializable} (i.e. both
      {{:https://en.wikipedia.org/wiki/Linearizability}linerizable} and
      {{:https://en.wikipedia.org/wiki/Serializability}serializable}).
    - Provided operations are efficient, either
      ({{:https://en.wikipedia.org/wiki/Amortized_analysis}amortized}) constant
      time, [O(1)], or logarithmic time, [O(log(n))].
    - Provided operations are
      {{:https://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedom}lock-free}
      and designed to avoid
      {{:https://en.wikipedia.org/wiki/Starvation_(computer_science)}starvation}
      under moderate contention.
    - Provided read-only operations scale perfectly when only read-only
      operations are performed in parallel.

    Unobvious exceptions to the above guarantees should be clearly and
    explicitly documented.

    The main feature of these data structure implementations is their
    compositionality.  For example, one can easily compose a transaction to
    atomically transfer a value from a {!Queue} to a {!Stack}:

    {[
      let tx ~xt =
        match Queue.Xt.take_opt ~xt queue with
        | None -> ()
        | Some value ->
          Stack.Xt.push ~xt stack value
      in
      Xt.commit { tx }
    ]}

    If your application does not need compositionality, then other domain safe
    data structure libraries may potentially offer better performance. *)

(** {1 [Stdlib] style data structures}

    The data structures in this section are designed to closely mimic the
    corresponding unsynchronized data structures in the OCaml [Stdlib].  Each of
    these provide a non-compositional, but domain safe, interface that is close
    to the [Stdlib] equivalent.  Additionally, compositional transactional
    interfaces are provided for some operations.

    These implementations will use more space than the corresponding [Stdlib]
    data structures.  Performance, when accessed concurrently, should be
    competitive or superior compared to naïve locking. *)

module Hashtbl = Hashtbl
module Queue = Queue
module Stack = Stack

(** {1 Communication and synchronization primitives}  *)

module Mvar = Mvar
module Promise = Promise

(** {1 Linked data structures} *)

module Dllist = Dllist

(** {1 Utilities} *)

module Accumulator = Accumulator
