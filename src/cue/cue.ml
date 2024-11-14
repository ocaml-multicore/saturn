(* Copyright (c) 2023-2024, Vesa Karvonen <vesa.a.j.k@gmail.com>
   Copyright (c) 2024, Carine Morel <carine.morel.pro@gmail.com>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

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

let[@inline] get_capacity (Node r : (_, [< `Node ]) node) = r.capacity

let[@inline] set_capacity (Node r : (_, [< `Node ]) node) value =
  r.capacity <- value

let[@inline] get_counter (Node r : (_, [< `Node ]) node) = r.counter

let[@inline] set_counter (Node r : (_, [< `Node ]) node) value =
  r.counter <- value

let[@inline] get_next node = Atomic.get (next_as_atomic node)

let[@inline] fenceless_get_next node =
  Atomic.fenceless_get (next_as_atomic node)

let[@inline] compare_and_set_next node before after =
  Atomic.compare_and_set (next_as_atomic node) before after

type 'a t = {
  head : ('a, [ `Node ]) node Atomic.t;
  capacity : int;
  tail : ('a, [ `Node ]) node Atomic.t;
}

let create ?(capacity = Int.max_int) () =
  let value = Obj.magic () in
  let node = Node { _next = Link Null; value; capacity; counter = 0 } in
  let head = Atomic.make node |> Multicore_magic.copy_as_padded
  and tail = Atomic.make node |> Multicore_magic.copy_as_padded in
  { head; capacity; tail } |> Multicore_magic.copy_as_padded

let capacity_of t = t.capacity

let is_empty t =
  let head = Atomic.get t.head in
  fenceless_get_next head == Link Null

let rec snapshot t =
  let head = Atomic.get t.head in
  let tail = Atomic.fenceless_get t.tail in
  match fenceless_get_next tail with
  | Link (Node _ as node) ->
      Atomic.compare_and_set t.tail tail node |> ignore;
      snapshot t
  | Link Null -> if Atomic.get t.head != head then snapshot t else (head, tail)

let length t =
  let head, tail = snapshot t in
  get_counter tail - get_counter head

(* *)

let rec peek_opt t =
  let head = Atomic.get t.head in
  match fenceless_get_next head with
  | Link Null -> None
  | Link (Node r) ->
      let value = r.value in
      if Atomic.get t.head != head then peek_opt t else Some value

(* *)

let rec pop_opt t backoff =
  let old_head = Atomic.get t.head in
  match fenceless_get_next old_head with
  | Link Null -> None
  | Link (Node node as new_head) ->
      if Atomic.compare_and_set t.head old_head new_head then begin
        let value = node.value in
        node.value <- Obj.magic ();
        Some value
      end
      else pop_opt t (Backoff.once backoff)

(* *)

let rec fix_tail tail new_tail =
  let old_tail = Atomic.get tail in
  if
    get_next new_tail == Link Null
    && not (Atomic.compare_and_set tail old_tail new_tail)
  then fix_tail tail new_tail

(* *)

let rec try_push t new_node old_tail =
  let capacity = get_capacity old_tail in
  if capacity = 0 then
    let old_head = Atomic.get t.head in
    let length = get_counter old_tail - get_counter old_head in
    let capacity = t.capacity - length in
    0 < capacity
    && begin
         set_capacity old_tail capacity;
         try_push t new_node old_tail
       end
  else begin
    set_capacity new_node (capacity - 1);
    set_counter new_node (get_counter old_tail + 1);
    if not (compare_and_set_next old_tail (Link Null) (Link new_node)) then
      try_push t new_node (link_as_node (get_next old_tail))
    else begin
      if not (Atomic.compare_and_set t.tail old_tail new_node) then
        fix_tail t.tail new_node;
      true
    end
  end

(* *)

let[@inline] pop_opt t = pop_opt t Backoff.default

let[@inline] try_push t value =
  let new_node = Node { _next = Link Null; value; capacity = 0; counter = 0 } in
  try_push t new_node (Atomic.get t.tail)
