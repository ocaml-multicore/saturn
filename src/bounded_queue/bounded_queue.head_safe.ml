type ('a, _) node =
  | Null : ('a, [> `Null ]) node
  | Node : {
      next : 'a link Atomic.t;
      mutable value : 'a;
      mutable capacity : int;
      mutable counter : int;
    }
      -> ('a, [> `Node ]) node

and 'a link = Link : ('a, [< `Null | `Node ]) node -> 'a link [@@unboxed]

let[@inline] make_node ~value ~capacity ~counter next =
  Node { next = Atomic.make (Link next); value; capacity; counter }

let[@inline] link_as_node (Link n) : (_, [< `Node ]) node =
  match n with Null -> assert false | Node _ as node -> node

let[@inline] get_next (Node node : (_, [< `Node ]) node) = Atomic.get node.next

let[@inline] fenceless_get_next (Node node : (_, [< `Node ]) node) =
  Atomic.get node.next

let[@inline] compare_and_set_next (Node node : (_, [< `Node ]) node) before
    after =
  Atomic.compare_and_set node.next before after

let fenceless_get = Atomic.get
