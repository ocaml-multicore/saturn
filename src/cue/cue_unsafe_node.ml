module Atomic = Multicore_magic.Transparent_atomic

type ('a, _) node =
  | Null : ('a, [> `Null ]) node
  | Node : {
      mutable _next : 'a link;
      mutable value : 'a;
      mutable capacity : int;
      mutable counter : int;
    }
      -> ('a, [> `Node ]) node

and 'a link = Link : ('a, [< `Null | `Node ]) node -> 'a link [@@unboxed]

external link_as_node : 'a link -> ('a, [ `Node ]) node = "%identity"

external next_as_atomic : ('a, [< `Node ]) node -> 'a link Atomic.t
  = "%identity"

let[@inline] make_node ~value ~capacity ~counter next =
  Node { _next = Link next; value; capacity; counter }
