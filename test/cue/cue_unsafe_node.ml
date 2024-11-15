open Multicore_magic_dscheck
module Atomic = Multicore_magic.Transparent_atomic

type ('a, _) node =
  | Null : ('a, [> `Null ]) node
  | Node : {
      _next : 'a link Atomic.t;
      mutable value : 'a;
      mutable capacity : int;
      mutable counter : int;
    }
      -> ('a, [> `Node ]) node

and 'a link = Link : ('a, [< `Null | `Node ]) node -> 'a link [@@unboxed]

external link_as_node : 'a link -> ('a, [ `Node ]) node = "%identity"

let[@inline] next_as_atomic : ('a, [< `Node ]) node -> 'a link Atomic.t =
 fun (Node r : ('a, [< `Node ]) node) -> r._next

let[@inline] make_node ~value ~capacity ~counter next =
  Node { _next = Atomic.make (Link next); value; capacity; counter }
