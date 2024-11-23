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

let[@inline] make_node ~value ~capacity ~counter next =
  Node { _next = Link next; value; capacity; counter }

external link_as_node : 'a link -> ('a, [ `Node ]) node = "%identity"

external next_as_atomic : ('a, [< `Node ]) node -> 'a link Atomic.t
  = "%identity"

let[@inline] get_next node = Atomic.get (next_as_atomic node)

let[@inline] fenceless_get_next node =
  Atomic.fenceless_get (next_as_atomic node)

let[@inline] compare_and_set_next node before after =
  Atomic.compare_and_set (next_as_atomic node) before after

let fenceless_get = Atomic.fenceless_get
