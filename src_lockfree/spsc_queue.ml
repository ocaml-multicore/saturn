(*
 * Copyright (c) 2022, Bartosz Modelski
 * Copyright (c) 2024, Vesa Karvonen
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Single producer single consumer queue
 *
 * The algorithms here are inspired by:

 * https://dl.acm.org/doi/pdf/10.1145/3437801.3441583
 *)

module Atomic = Transparent_atomic

type 'a t = {
  array : 'a Array.t;
  tail : int Atomic.t;
  tail_cache : int ref;
  head : int Atomic.t;
  head_cache : int ref;
}

exception Full

let create ~size_exponent =
  let size = Int.shift_left 1 size_exponent in
  {
    array = Array.make size (Obj.magic ());
    tail = Atomic.make 0 |> Multicore_magic.copy_as_padded;
    tail_cache = ref 0 |> Multicore_magic.copy_as_padded;
    head = Atomic.make 0 |> Multicore_magic.copy_as_padded;
    head_cache = ref 0 |> Multicore_magic.copy_as_padded;
  }
  |> Multicore_magic.copy_as_padded

let push t element =
  let size = Array.length t.array in
  let tail = Atomic.fenceless_get t.tail in
  let head_cache = !(t.head_cache) in
  if
    head_cache == tail - size
    &&
    let head = Atomic.get t.head in
    t.head_cache := head;
    head == head_cache
  then raise Full
  else begin
    Array.unsafe_set t.array (tail land (size - 1)) element;
    Atomic.incr t.tail
  end

let try_push t element =
  let size = Array.length t.array in
  let tail = Atomic.fenceless_get t.tail in
  let head_cache = !(t.head_cache) in
  if
    head_cache == tail - size
    &&
    let head = Atomic.get t.head in
    t.head_cache := head;
    head == head_cache
  then false
  else begin
    Array.unsafe_set t.array (tail land (size - 1)) element;
    Atomic.incr t.tail;
    true
  end

exception Empty

let pop t =
  let head = Atomic.fenceless_get t.head in
  let tail_cache = !(t.tail_cache) in
  if
    head == tail_cache
    &&
    let tail = Atomic.get t.tail in
    t.tail_cache := tail;
    tail_cache == tail
  then raise Empty
  else
    let index = head land (Array.length t.array - 1) in
    let v = Array.unsafe_get t.array index in
    (* allow gc to collect it *)
    Array.unsafe_set t.array index (Obj.magic ());
    Atomic.incr t.head;
    v

let pop_opt t =
  let head = Atomic.fenceless_get t.head in
  let tail_cache = !(t.tail_cache) in
  if
    head == tail_cache
    &&
    let tail = Atomic.get t.tail in
    t.tail_cache := tail;
    tail_cache == tail
  then None
  else
    let index = head land (Array.length t.array - 1) in
    let v = Array.unsafe_get t.array index in
    (* allow gc to collect it *)
    Array.unsafe_set t.array index (Obj.magic ());
    Atomic.incr t.head;
    Some v

let peek_opt t =
  let head = Atomic.fenceless_get t.head in
  let tail_cache = !(t.tail_cache) in
  if
    head == tail_cache
    &&
    let tail = Atomic.get t.tail in
    t.tail_cache := tail;
    tail_cache == tail
  then None
  else Some (Array.unsafe_get t.array (head land (Array.length t.array - 1)))

let peek t =
  let head = Atomic.fenceless_get t.head in
  let tail_cache = !(t.tail_cache) in
  if
    head == tail_cache
    &&
    let tail = Atomic.get t.tail in
    t.tail_cache := tail;
    tail_cache == tail
  then raise Empty
  else Array.unsafe_get t.array (head land (Array.length t.array - 1))

let size t =
  let tail = Atomic.get t.tail in
  let head = Atomic.fenceless_get t.head in
  tail - head
