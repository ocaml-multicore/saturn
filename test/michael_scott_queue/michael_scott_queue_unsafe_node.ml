open Multicore_magic_dscheck
module Atomic = Multicore_magic.Transparent_atomic

type ('a, _) t =
  | Nil : ('a, [> `Nil ]) t
  | Next : {
      next : ('a, [ `Nil | `Next ]) t Atomic.t;
      mutable value : 'a;
    }
      -> ('a, [> `Next ]) t

let[@inline] make value = Next { next = Atomic.make Nil; value }

let node_of_list values =
  let (Next tail_node as tail) : ('a, [ `Next ]) t =
    Next { value = Obj.magic (); next = Atomic.make Nil }
  in
  let rec build_next = function
    | [ x ] ->
        tail_node.value <- x;
        tail
    | value :: tl -> Next { value; next = Atomic.make @@ build_next tl }
    | [] -> assert false
  in
  (tail, Atomic.make @@ build_next values)

let[@inline] as_atomic (Next r : ('a, [ `Next ]) t) = r.next
