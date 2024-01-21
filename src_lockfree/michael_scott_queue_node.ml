module Atomic = Transparent_atomic

type ('a, _) t =
  | Nil : ('a, [> `Nil ]) t
  | Next : {
      next : ('a, [ `Nil | `Next ]) t Atomic.t;
      mutable value : 'a;
    }
      -> ('a, [> `Next ]) t

let[@inline] make value = Next { next = Atomic.make Nil; value }
let[@inline] as_atomic (Next r : ('a, [ `Next ]) t) = r.next
