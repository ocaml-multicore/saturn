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
  array : 'a Option.t Array.t;
  tail : int Atomic.t;
  tail_cache : int ref;
  head : int Atomic.t;
  head_cache : int ref;
  mask : int;
}

exception Full

let create ~size_exponent =
  let size = Int.shift_left 1 size_exponent in
  {
    array = Array.make size None;
    tail = Atomic.make 0 |> Multicore_magic.copy_as_padded;
    tail_cache = ref 0 |> Multicore_magic.copy_as_padded;
    head = Atomic.make 0 |> Multicore_magic.copy_as_padded;
    head_cache = ref 0 |> Multicore_magic.copy_as_padded;
    mask = size - 1;
  }
  |> Multicore_magic.copy_as_padded

let push t element =
  let size = t.mask + 1 in
  let tail_val = Atomic.fenceless_get t.tail in
  if
    !(t.head_cache) + size == tail_val
    && begin
         t.head_cache := Atomic.get t.head;
         !(t.head_cache) + size == tail_val
       end
  then raise Full
  else begin
    Array.set t.array (tail_val land t.mask) (Some element);
    Atomic.incr t.tail
  end

let try_push t element =
  let size = t.mask + 1 in
  let tail_val = Atomic.fenceless_get t.tail in
  if
    !(t.head_cache) + size == tail_val
    && begin
         t.head_cache := Atomic.get t.head;
         !(t.head_cache) + size == tail_val
       end
  then false
  else begin
    Array.set t.array (tail_val land t.mask) (Some element);
    Atomic.incr t.tail;
    true
  end

exception Empty

let pop t =
  let head_val = Atomic.fenceless_get t.head in
  if
    head_val == !(t.tail_cache)
    && begin
         t.tail_cache := Atomic.get t.tail;
         head_val == !(t.tail_cache)
       end
  then raise Empty
  else
    let index = head_val land t.mask in
    let v = Array.get t.array index in
    (* allow gc to collect it *)
    Array.set t.array index None;
    Atomic.incr t.head;
    match v with None -> assert false | Some v -> v

let pop_opt t =
  let head_val = Atomic.fenceless_get t.head in
  if
    head_val == !(t.tail_cache)
    && begin
         t.tail_cache := Atomic.get t.tail;
         head_val == !(t.tail_cache)
       end
  then None
  else
    let index = head_val land t.mask in
    let v = Array.get t.array index in
    (* allow gc to collect it *)
    Array.set t.array index None;
    Atomic.incr t.head;
    assert (Option.is_some v);
    v

let peek_opt t =
  let head_val = Atomic.fenceless_get t.head in
  if
    head_val == !(t.tail_cache)
    && begin
         t.tail_cache := Atomic.get t.tail;
         head_val == !(t.tail_cache)
       end
  then None
  else
    let v = Array.get t.array (head_val land t.mask) in
    assert (Option.is_some v);
    v

let peek t =
  let head_val = Atomic.fenceless_get t.head in
  if
    head_val == !(t.tail_cache)
    && begin
         t.tail_cache := Atomic.get t.tail;
         head_val == !(t.tail_cache)
       end
  then raise Empty
  else
    let v = Array.get t.array (head_val land t.mask) in
    match v with None -> assert false | Some v -> v

let size t =
  let tail = Atomic.get t.tail in
  let head = Atomic.fenceless_get t.head in
  tail - head
