(** Wait-free size counter for lock-free data structures

    This is inspired by the paper
    {{:https://arxiv.org/pdf/2209.07100.pdf} Concurrent Size} by Gal Sela and
    Erez Petrank and users may find the paper and, in particular, the figure 3
    of a transformed data structure in the paper enlightening.

    The algorithm used by this module differs from
    {{:https://arxiv.org/pdf/2209.07100.pdf} the paper} in some important ways.
    First of all, unlike in the paper, the algorithm does not require the number
    of threads to be limited and given unique integer indices to ensure
    correctness. Instead, the algorithm uses a lock-free transactional approach
    to performing the counter {{!update_once} updates at most once}. Another
    difference is that the algorithm is also designed to give correct answer in
    case of internal counter overflow.

    Consider the following singly linked list representation and internal
    [try_find] operation:

    {[
      type 'a node =
        | Null
        | Node of { next : 'a node Atomic.t; datum : 'a }
        | Mark of { node : 'a node }

      type 'a t = { head : 'a node Atomic.t }

      let rec try_find t prev datum = function
        | Mark _ -> try_find t t.head datum (Atomic.get t.head)
        | Null -> Null
        | Node r as node -> begin
            match Atomic.get r.next with
            | Mark r ->
                if Atomic.compare_and_set prev node r.node then
                  try_find t prev datum r.node
                else try_find t prev datum (Atomic.get prev)
            | (Null | Node _) as next ->
                if r.datum == datum then node else try_find t r.next datum next
          end
    ]}

    To enhance the list with size, a [size] counter is added to the list, an
    [incr] update is added to nodes, a [decr] update is added to marked links
    from nodes to be removed, and [try_find] is enhanced to perform the updates
    once after witnessing the updates while traversing the data structure:

    {[
      type 'a node =
        | Null
        | Node of {
            next : 'a node Atomic.t;
            datum : 'a;
            mutable incr : Size.once; (* ADDED *)
          }
        | Mark of { node : 'a node; decr : Size.once (* ADDED *) }

      type 'a t = { head : 'a node Atomic.t; size : Size.t (* ADDED *) }

      let rec try_find t prev datum = function
        | Mark _ -> try_find t t.head datum (Atomic.get t.head)
        | Null -> Null
        | Node r as node -> begin
            match Atomic.get r.next with
            | Mark r ->
                Size.update_once t.size r.decr;
                (* ADDED *)
                if Atomic.compare_and_set prev node r.node then
                  try_find t prev datum r.node
                else try_find t prev datum (Atomic.get prev)
            | (Null | Node _) as next ->
                if r.datum == datum then begin
                  if r.incr != Size.used_once then begin
                    Size.update_once t.size r.incr;
                    (* ADDED *)
                    r.incr <- Size.used_once
                  end;
                  node
                end
                else try_find t r.next datum next
          end
    ]}

    Notice how the mutable [incr] field is tested against and overwritten with
    {!used_once} after being performed. This can improve performance as nodes
    are potentially witnessed many times over their lifetime unlike the marked
    links which are removed as soon as possible.

    All operations that witness a particular node or the removal of a node must
    perform the updates of the size counter. This ensures that the commit point
    of the operations becomes the update of the size counter. This approach is
    general enough to enhance many kinds of lock-free data structures with a
    correct (linearizable) size. *)

type t
(** The type of a size counter. *)

val create : unit -> t
(** [create ()] allocates a new size counter. The initial value of the size
    counter will be [0]. *)

type once
(** The type of an at most once update of a size counter. *)

val used_once : once
(** [used_once] is a constant for an at most {!once} update that has already
    been used. *)

type update [@@immediate]
(** The type of an update on a size counter. *)

val decr : update
(** [decr] is an update that decrements a size counter. *)

val incr : update
(** [incr] is an update that increments a size counter. *)

val new_once : t -> update -> once
(** [new_once size update] creates a new at most {!once} update that, when
    passed to {!update_once}, will perform the [update] on the [size] counter.

    ⚠️ When calling {!update_once} the same [size] counter must be used. *)

val update_once : t -> once -> unit
(** [update_once size once] performs the update, increment or decrement, of the
    [size] counter at most [once].

    ⚠️ The [once] update must be either {!used_once} or must have been created by
    {!new_once} with the same [size] counter. *)

val max_value : int
(** [max_value] is the maximum value of a counter. *)

val get : t -> int
(** [get size] computes and returns the current value of the size counter. The
    value will always be a non-negative value between [0] and [max_value].

    The computation is done in a wait-free manner, which means that parallel
    updates of the size counter cannot force [get size] to starve nor can
    parallel computations of the size force counter updates to starve. *)
