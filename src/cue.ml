(* Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>

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

external fenceless_get : 'a Atomic.t -> 'a = "%field0"

type 'a node =
  | Null
  | Node of {
      mutable _next : 'a node;
      mutable value : 'a;
      mutable capacity : int;
      mutable counter : int;
    }

type 'a record = {
  mutable _next : 'a node;
  mutable value : 'a;
  mutable capacity : int;
  mutable counter : int;
}

external next_as_atomic : 'a node -> 'a node Atomic.t = "%identity"

let[@inline] get_next node = Atomic.get (next_as_atomic node)
let[@inline] fenceless_get_next node = fenceless_get (next_as_atomic node)

external as_record : 'a node -> 'a record = "%identity"

let[@inline] get_capacity node = (as_record node).capacity
let[@inline] set_capacity node value = (as_record node).capacity <- value
let[@inline] get_counter node = (as_record node).counter
let[@inline] set_counter node value = (as_record node).counter <- value

let[@inline] compare_and_set_next node before after =
  Atomic.compare_and_set (next_as_atomic node) before after

type 'a t = {
  head : 'a node Atomic.t;
  head_waiters : (unit -> unit) list Atomic.t;
  capacity : int;
  tail_waiters : (unit -> unit) list Atomic.t;
  tail : 'a node Atomic.t;
}

let create ?(capacity = Int.max_int) () =
  let value = Obj.magic () in
  let node = Node { _next = Null; value; capacity; counter = 0 } in
  let head = Atomic.make node
  and head_waiters = Atomic.make []
  and tail_waiters = Atomic.make []
  and tail = Atomic.make node in
  { head; head_waiters; capacity; tail_waiters; tail }

let capacity_of t = t.capacity

let is_empty t =
  let head = Atomic.get t.head in
  fenceless_get_next head == Null

let rec snapshot t =
  let head = Atomic.get t.head in
  let tail = fenceless_get t.tail in
  match fenceless_get_next tail with
  | Node _ as node ->
      Atomic.compare_and_set t.tail tail node |> ignore;
      snapshot t
  | Null -> if Atomic.get t.head != head then snapshot t else (head, tail)

let length t =
  let head, tail = snapshot t in
  get_counter tail - get_counter head

(* *)

let rec release_all waiters =
  let releases = fenceless_get waiters in
  if releases != [] then
    if Atomic.compare_and_set waiters releases [] then
      List.iter (fun release -> release ()) releases
    else release_all waiters

(* *)

let rec peek t =
  let old_head = Atomic.get t.head in
  match fenceless_get_next old_head with
  | Null ->
      let dla = Domain_local_await.prepare_for_await () in
      let releases = Atomic.get t.tail_waiters in
      if Atomic.compare_and_set t.tail_waiters releases (dla.release :: releases)
      then (
        if old_head != Atomic.get t.tail then release_all t.tail_waiters
        else
          try dla.await ()
          with exn ->
            release_all t.tail_waiters;
            raise exn);
      peek t
  | Node r as node ->
      let value = r.value in
      if Atomic.get t.head != node then peek t else value

let[@inline] peek t = peek t

(* *)

let rec peek_opt t =
  let head = Atomic.get t.head in
  match fenceless_get_next head with
  | Null -> None
  | Node r as node ->
      let value = r.value in
      if Atomic.get t.head != node then peek_opt t else Some value

let[@inline] peek_opt t = peek_opt t

(* *)

let rec pop backoff t =
  let old_head = Atomic.get t.head in
  match fenceless_get_next old_head with
  | Null ->
      let dla = Domain_local_await.prepare_for_await () in
      let releases = Atomic.get t.tail_waiters in
      if Atomic.compare_and_set t.tail_waiters releases (dla.release :: releases)
      then (
        if old_head != Atomic.get t.tail then release_all t.tail_waiters
        else
          try dla.await ()
          with exn ->
            release_all t.tail_waiters;
            raise exn);
      pop backoff t
  | Node node as new_head ->
      if Atomic.compare_and_set t.head old_head new_head then (
        let value = node.value in
        node.value <- Obj.magic ();
        release_all t.head_waiters;
        value)
      else pop (Backoff.once backoff) t

let[@inline] pop t = pop Backoff.default t

(* *)

let rec pop_opt backoff t =
  let old_head = Atomic.get t.head in
  match fenceless_get_next old_head with
  | Null -> None
  | Node node as new_head ->
      if Atomic.compare_and_set t.head old_head new_head then (
        let value = node.value in
        node.value <- Obj.magic ();
        release_all t.head_waiters;
        Some value)
      else pop_opt (Backoff.once backoff) t

let[@inline] pop_opt t = pop_opt Backoff.default t

(* *)

let rec fix_tail tail new_tail =
  let old_tail = Atomic.get tail in
  if
    get_next new_tail == Null
    && not (Atomic.compare_and_set tail old_tail new_tail)
  then fix_tail tail new_tail

(* *)

let rec push t new_node old_tail =
  let capacity = get_capacity old_tail in
  if capacity = 0 then (
    let old_head = Atomic.get t.head in
    let length = get_counter old_tail - get_counter old_head in
    let capacity = t.capacity - length in
    if 0 < capacity then (
      set_capacity old_tail capacity;
      push t new_node old_tail)
    else
      let dla = Domain_local_await.prepare_for_await () in
      let releases = Atomic.get t.head_waiters in
      if Atomic.compare_and_set t.head_waiters releases (dla.release :: releases)
      then (
        if old_head != Atomic.get t.head then release_all t.head_waiters
        else
          try dla.await ()
          with exn ->
            release_all t.head_waiters;
            raise exn);
      push t new_node old_tail)
  else (
    set_capacity new_node (capacity - 1);
    set_counter new_node (get_counter old_tail + 1);
    if not (compare_and_set_next old_tail Null new_node) then
      push t new_node (get_next old_tail)
    else (
      if not (Atomic.compare_and_set t.tail old_tail new_node) then
        fix_tail t.tail new_node;
      release_all t.tail_waiters))

let push t value =
  let new_node = Node { _next = Null; value; capacity = 0; counter = 0 } in
  push t new_node (Atomic.get t.tail)
  [@@inline]

(* *)

let rec try_push t new_node old_tail =
  let capacity = get_capacity old_tail in
  if capacity = 0 then
    let old_head = Atomic.get t.head in
    let length = get_counter old_tail - get_counter old_head in
    let capacity = t.capacity - length in
    0 < capacity
    &&
    (set_capacity old_tail capacity;
     try_push t new_node old_tail)
  else (
    set_capacity new_node (capacity - 1);
    set_counter new_node (get_counter old_tail + 1);
    if not (compare_and_set_next old_tail Null new_node) then
      try_push t new_node (get_next old_tail)
    else (
      if not (Atomic.compare_and_set t.tail old_tail new_node) then
        fix_tail t.tail new_node;
      release_all t.tail_waiters;
      true))

let try_push t value =
  let new_node = Node { _next = Null; value; capacity = 0; counter = 0 } in
  try_push t new_node (Atomic.get t.tail)
  [@@inline]
