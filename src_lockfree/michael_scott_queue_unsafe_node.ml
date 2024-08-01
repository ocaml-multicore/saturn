module Atomic = Multicore_magic.Transparent_atomic

type ('a, _) t =
  | Nil : ('a, [> `Nil ]) t
  | Next : {
      mutable next : ('a, [ `Nil | `Next ]) t;
      mutable value : 'a;
    }
      -> ('a, [> `Next ]) t

let[@inline] make value = Next { next = Nil; value }

external as_atomic : ('a, [ `Next ]) t -> ('a, [ `Nil | `Next ]) t Atomic.t
  = "%identity"
