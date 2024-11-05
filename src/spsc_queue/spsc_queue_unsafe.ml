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

module Atomic = Multicore_magic.Transparent_atomic

type not_float = [ `Not_float of not_float ]

type 'a t = {
  array : not_float Array.t;
  tail : int Atomic.t;
  tail_cache : int ref;
  head : int Atomic.t;
  head_cache : int ref;
}

exception Full

let create ~size_exponent =
  if size_exponent < 0 || Sys.int_size - 2 < size_exponent then
    invalid_arg "size_exponent out of range";
  let size = 1 lsl size_exponent in
  let array = Array.make size (Obj.magic ()) in
  let tail = Atomic.make_contended 0 in
  let tail_cache = ref 0 |> Multicore_magic.copy_as_padded in
  let head = Atomic.make_contended 0 in
  let head_cache = ref 0 |> Multicore_magic.copy_as_padded in
  { array; tail; tail_cache; head; head_cache }
  |> Multicore_magic.copy_as_padded

type _ mono = Unit : unit mono | Bool : bool mono

(* NOTE: Uses of [@inline never] prevent Flambda from noticing that we might be
   storing float values into a non-float array. *)

let[@inline never] push_as (type r) t element (mono : r mono) : r =
  let size = Array.length t.array in
  let tail = Atomic.fenceless_get t.tail in
  let head_cache = !(t.head_cache) in
  if
    head_cache == tail - size
    &&
    let head = Atomic.get t.head in
    t.head_cache := head;
    head == head_cache
  then match mono with Unit -> raise_notrace Full | Bool -> false
  else begin
    Array.unsafe_set t.array (tail land (size - 1)) (Obj.magic element);
    Atomic.incr t.tail;
    match mono with Unit -> () | Bool -> true
  end

let push_exn t element = push_as t element Unit
let try_push t element = push_as t element Bool

exception Empty

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly
type op = Peek | Pop

let[@inline never] pop_or_peek_as (type a r) t op (poly : (a, r) poly) : r =
  let head = Atomic.fenceless_get t.head in
  let tail_cache = !(t.tail_cache) in
  if
    head == tail_cache
    &&
    let tail = Atomic.get t.tail in
    t.tail_cache := tail;
    tail_cache == tail
  then match poly with Value -> raise_notrace Empty | Option -> None
  else
    let index = head land (Array.length t.array - 1) in
    let v = Array.unsafe_get t.array index |> Obj.magic in
    begin
      match op with
      | Pop ->
          Array.unsafe_set t.array index (Obj.magic ());
          Atomic.incr t.head
      | Peek -> ()
    end;
    match poly with Value -> v | Option -> Some v

let pop_exn t = pop_or_peek_as t Pop Value
let pop_opt t = pop_or_peek_as t Pop Option
let peek_exn t = pop_or_peek_as t Peek Value
let peek_opt t = pop_or_peek_as t Peek Option

let size t =
  let tail = Atomic.get t.tail in
  let head = Atomic.fenceless_get t.head in
  tail - head
