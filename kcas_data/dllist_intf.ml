module type Ops = sig
  type 'a t
  type 'a node
  type ('x, 'fn) fn

  val remove : ('x, 'a node -> unit) fn
  (** [remove n] removes the node [n] from the doubly-linked list it is part of.
      [remove] is idempotent. *)

  val move_l : ('x, 'a node -> 'a t -> unit) fn
  (** [move_l n l] removes the node [n] from the doubly-linked list it is part
      of and then adds it to the left of the list [l]. *)

  val move_r : ('x, 'a node -> 'a t -> unit) fn
  (** [move_r n l] removes the node [n] from the doubly-linked list it is part
      of and then adds it to the right of the list [l]. *)

  val is_empty : ('x, 'a t -> bool) fn
  (** [is_empty l] determines whether the doubly-linked list [l] is empty or
      not. *)

  val add_l : ('x, 'a -> 'a t -> 'a node) fn
  (** [add_l v l] creates and returns a new node with the value [v] and adds the
      node to the left of the doubly-linked list [l]. *)

  val add_r : ('x, 'a -> 'a t -> 'a node) fn
  (** [add_r v l] creates and returns a new node with the value [v] and adds the
      node to the right of the doubly-linked list [l]. *)

  val take_opt_l : ('x, 'a t -> 'a option) fn
  (** [take_opt_l l] removes and returns the value of leftmost node of the
      doubly-linked list [l], or return [None] if the list is empty. *)

  val take_opt_r : ('x, 'a t -> 'a option) fn
  (** [take_opt_r l] removes and returns the value of rightmost node of the
      doubly-linked list [l], or return [None] if the list is empty. *)

  val take_blocking_l : ('x, 'a t -> 'a) fn
  (** [take_blocking_l l] removes and returns the value of leftmost node of the
      doubly-linked list [l], or blocks waiting for the list to become
      non-empty. *)

  val take_blocking_r : ('x, 'a t -> 'a) fn
  (** [take_blocking_r l] removes and returns the value of rightmost node of the
      doubly-linked list [l], or blocks waiting for the list to become
      non-empty. *)

  val swap : ('x, 'a t -> 'a t -> unit) fn
  (** [swap l1 l2] exchanges the nodes of the doubly-linked lists [l1] and
      [l2]. *)

  val transfer_l : ('x, 'a t -> 'a t -> unit) fn
  (** [transfer_l l1 l2] removes all nodes of [l1] and adds them to the left of
      [l2]. *)

  val transfer_r : ('x, 'a t -> 'a t -> unit) fn
  (** [transfer_r l1 l2] removes all nodes of [l1] and adds them to the right of
      [l2]. *)

  val to_list_l : ('x, 'a t -> 'a list) fn
  (** [to_list_l l] collects the values of the nodes of the doubly-linked list
      [l] to a list in left-to-right order.

      {b NOTE}: This operation is linear time, [O(n)], and should typically be
      avoided unless the list is privatized, e.g. by using {!take_all}. *)

  val to_list_r : ('x, 'a t -> 'a list) fn
  (** [to_list_r l] collects the values of the nodes of the doubly-linked list
      [l] to a list in right-to-left order.

      {b NOTE}: This operation is linear time, [O(n)], and should typically be
      avoided unless the list is privatized, e.g. by using {!take_all}. *)

  val to_nodes_l : ('x, 'a t -> 'a node list) fn
  (** [to_nodes_l l] collects the nodes of the doubly-linked list [l] to a list
      in left-to-right order.

      {b NOTE}: This operation is linear time, [O(n)], and should typically be
      avoided unless the list is privatized, e.g. by using {!take_all}. *)

  val to_nodes_r : ('x, 'a t -> 'a node list) fn
  (** [to_nodes_r l] collects the nodes of the doubly-linked list [l] to a list
      in right-to-left order.

      {b NOTE}: This operation is linear time, [O(n)], and should typically be
      avoided unless the list is privatized, e.g. by using {!take_all}. *)
end
