module Atomic = Multicore_magic.Transparent_atomic

type ('a, _) t =
  | Nil : ('a, [> `Nil ]) t
  | Next : {
      mutable next : ('a, [ `Nil | `Next ]) t;
      mutable value : 'a;
    }
      -> ('a, [> `Next ]) t

let[@inline] make value = Next { next = Nil; value }

let node_of_list values =
  let (Next tail_node as tail) : ('a, [ `Next ]) t =
    Next { value = Obj.magic (); next = Nil }
  in
  let rec build_next = function
    | [ x ] ->
        tail_node.value <- x;
        tail
    | hd :: tl -> Next { next = build_next tl; value = hd }
    | [] -> assert false
  in
  (tail, build_next values)

external as_atomic : ('a, [ `Next ]) t -> ('a, [ `Nil | `Next ]) t Atomic.t
  = "%identity"
