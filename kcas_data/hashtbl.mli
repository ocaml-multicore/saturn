open Kcas

(** Hash table.

    The interface provides a subset of the OCaml [Stdlib.Hashtbl] module with
    some changes:

    - The functorial interface of the [Stdlib.Hashtbl] is not provided.
      Instead the constructor functions, {!create}, {!of_seq}, and {!rebuild},
      take an optional [HashedType] module as an argument.  By default {!create}
      returns a randomized hash table.
    - The [add_seq] and [replace_seq] operations are not provided at all.
    - Non-instance specific operations related to randomization (e.g.
      [randomize], [is_randomized]) are not provided.
    - Non-instance specific operations related to hashing (e.g. [hash],
      [seeded_hash], [hash_param], [seeded_hash_param]) are not provided.

    Compositional versions of {!find}, {!to_seq}, {!to_seq_keys},
    {!to_seq_values}, {!rebuild}, {!copy}, {!iter}, {!filter_map_inplace},
    {!fold}, and {!stats} are not provided.

    Please note that the design is intentionally based on [Stdlib.Hashtbl] and
    copies its semantics as accurately as possible.  Some of the operations come
    with warnings.

    The hash table implementation is designed to avoid starvation.  Read-only
    accesses can generally proceed in parallel without interference.  Write
    accesses that do not change the number of bindings can proceed in parallel
    as long as they hit different internal buckets.  Write accesses that change
    the number of bindings use a scalable {!Accumulator} and only make
    infrequent random checks to determine whether the hash table should be
    resized. *)

(** {1 Common interface} *)

type (!'k, !'v) t
(** The type of hash tables from type ['k] to type ['v]. *)

type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)
(** First-class [HashedType] module type abbreviation. *)

val create :
  ?hashed_type:'k hashed_type ->
  ?min_buckets:int ->
  ?max_buckets:int ->
  ?n_way:int ->
  unit ->
  ('k, 'v) t
(** [create ()] returns a new empty hash table.

    - The default [hash] is computed as [Stdlib.Hashtbl.hash (Random.bits ())].
    - The default [equal] is [(=)].
    - The default [min_buckets] is unspecified and a given [min_buckets] may be
      adjusted by the implementation.
    - The default [max_buckets] is the minimum of [1 lsl 30] and suitably
      adjusted [Sys.max_array_length] and a given [max_buckets] may be adjusted
      by the implementation.
    - The [n_way] argument is passed to the internal {!Accumulator} used to keep
      track of the number of bindings.

    Hash tables are automatically internally resized. *)

val hashed_type_of : ('k, 'v) t -> 'k hashed_type
(** [hashed_type_of t] returns a copy of the hashed type used when the hash
    table [t] was {!create}d. *)

val min_buckets_of : ('k, 'v) t -> int
(** [min_buckets_of t] returns the minimum number of buckets of the hash table
    [t].

    {b NOTE}: The returned value may not be the same as given to {!create}. *)

val max_buckets_of : ('k, 'v) t -> int
(** [max_buckets_of t] returns the maximum number of buckets of the hash table
    [t].

    {b NOTE}: The returned value may not be the same as given to {!create}. *)

val n_way_of : ('k, 'v) t -> int
(** [n_way_of t] returns the maximum number of non-interfering parallel updates
    allowed by the internal {!Accumulator} used to keep track of the number of
    bindings in the hash table [t]. *)

val of_seq :
  ?hashed_type:'k hashed_type ->
  ?min_buckets:int ->
  ?max_buckets:int ->
  ?n_way:int ->
  ('k * 'v) Seq.t ->
  ('k, 'v) t
(** [of_seq assoc] creates a new hash table from the given association sequence
    [assoc].  The associations are added in the same order as they appear in the
    sequence, using {!replace}, which means that if two pairs have the same key,
    only the latest one will appear in the table.  See {!create} for the
    optional arguments.

    ⚠️ [of_seq (to_seq t)] does not necessarily copy the bindings of a hash table
    correctly. *)

(** {1 Compositional interface} *)

module Xt :
  Hashtbl_intf.Ops
    with type ('k, 'v) t := ('k, 'v) t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on hash tables. *)

(** {1 Non-compositional interface} *)

include
  Hashtbl_intf.Ops
    with type ('k, 'v) t := ('k, 'v) t
    with type ('x, 'fn) fn := 'fn

val find : ('k, 'v) t -> 'k -> 'v
(** [find t k] returns the current binding of [k] in hash table [t], or raises
    [Not_found] if no such binding exists. *)

val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t
(** [to_seq t] takes a snapshot of the keys and values in the hash table and
    returns them as an association sequence.  Bindings of each individual key
    appear in the sequence in reverse order of their introduction.

    ⚠️ [of_seq (to_seq t)] does not necessarily copy the bindings of a hash table
    correctly. *)

val to_seq_keys : ('k, 'v) t -> 'k Seq.t
(** [to_seq_keys t] is equivalent to [to_seq t |> Seq.map fst].

    ⚠️ The sequence may include duplicates. *)

val to_seq_values : ('k, 'v) t -> 'v Seq.t
(** [to_seq_values t] is equivalent to [to_seq t |> Seq.map snd].

    ⚠️ The sequence may include values of bindings that are hidden. *)

val rebuild :
  ?hashed_type:'k hashed_type ->
  ?min_buckets:int ->
  ?max_buckets:int ->
  ?n_way:int ->
  ('k, 'v) t ->
  ('k, 'v) t
(** [rebuild t] returns a copy of the given hash table [t] optionally rehashing
    all of the bindings.

    See {!create} for descriptions of the optional arguments.  Unlike {!create},
    [rebuild] uses the given hash table [t] as a template to get defaults for
    the optional arguments. *)

val copy : ('k, 'v) t -> ('k, 'v) t
(** [copy t] is equivalent to [rebuild t].  In other words, the returned hash
    table uses the same {!hashed_type} (and other parameters) as the given hash
    table [t]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [iter f t] is equivalent to [Seq.iter (fun (k, v) -> f k v) (to_seq t)]. *)

val filter_map_inplace : ('k -> 'v -> 'v option) -> ('k, 'v) t -> unit
(** [filter_map_inplace f t] applies [f] to all bindings in the hash table [t]
    and updates each binding depending on the result of [f].  If [f] returns
    [None], the binding is discarded.  If [f] returns [Some new_value], the
    binding is updated to associate the key to the [new_value].

    ⚠️ The given [f] may be called multiple times for the same bindings from
    multiple domains in parallel. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [fold f t a] is equivalent to [Seq.fold_left (fun a (k, v) -> f k v a) a (to_seq t)]. *)

val stats : ('a, 'b) t -> Stdlib.Hashtbl.statistics
(** [stats t] returns statistics about the hash table [t]. *)
