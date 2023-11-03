open Kcas

(** Doubly-linked list.

    The interface provides a subset of the operations of the doubly-linked list
    data structure provided by the
    {{:https://opam.ocaml.org/packages/lwt-dllist/}lwt-dllist} package with some
    omissions:

    - The sequence iterators, e.g. [iter_l], [iter_node_l], [fold_l],
      [find_node_opt_l], and [find_node_l], are not provided.
    - The [length] operation is not provided.
    - The [set] operation is not provided.

    A non-compositional {!take_all} operation is added for {{:
    https://en.wikipedia.org/wiki/Privatization_(computer_programming)}privatization}
    as well as conversions to a list of nodes ({!to_nodes_l} and {!to_nodes_r})
    and to a list of values ({!to_list_l} and {!to_list_r}).

    Probably the main reason to use a doubly-linked list like this rather than
    e.g. a ['a list Loc.t] is the ability to remove a node without having to
    potentially iterate through the list:

    {[
      let node_of_x = add_l x list in
      (* ... and then later somewhere else ... *)
      remove node_of_x
    ]}

    A doubly-linked list can also be used as a deque or double-ended queue, but
    a deque implementation that doesn't allow individual nodes to be removed is
    likely to be faster. *)

(** {1 Common interface} *)

type !'a t
(** Type of a doubly-linked list containing {!node}s of type ['a]. *)

type !'a node
(** Type of a node containing a value of type ['a]. *)

val get : 'a node -> 'a
(** [get node] returns the value stored in the {!node}. *)

val create : unit -> 'a t
(** [create ()] return a new doubly-linked list. *)

(** {1 Compositional interface} *)

module Xt :
  Dllist_intf.Ops
    with type 'a t := 'a t
    with type 'a node := 'a node
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on doubly-linked lists. *)

(** {1 Non-compositional interface} *)

include
  Dllist_intf.Ops
    with type 'a t := 'a t
    with type 'a node := 'a node
    with type ('x, 'fn) fn := 'fn

val take_all : 'a t -> 'a t
(** [take_all l] removes all nodes of the doubly-linked list [l] and returns a
    new doubly-linked list containing the removed nodes. *)

exception Empty
(** Raised when {!take_l} or {!take_r} is applied to an empty doubly-linked
    list. *)

val take_l : 'a t -> 'a
(** [take_l l] removes and returns the value of the leftmost node of the
    doubly-linked list [l], or raises {!Empty} if the list is empty. *)

val take_r : 'a t -> 'a
(** [take_r l] removes and returns the value of the rightmost node of the
    doubly-linked list [l], or raises {!Empty} if the list is empty. *)
