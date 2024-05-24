(*
 * Copyright (c) 2022, Bartosz Modelski
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

module Padded_int_ref = struct
  type t = int array

  let[@inline] make s i : t = Array.make s i
  let[@inline] get (t : t) = Array.unsafe_get t 0
  let[@inline] set (t : t) v = Array.unsafe_set t 0 v
end

type 'a t = {
  array : 'a Option.t Array.t;
  tail : int Atomic.t;
  tail_cache : Padded_int_ref.t;
  head : int Atomic.t;
  head_cache : Padded_int_ref.t;
}

exception Full

let create ~size_exponent =
  if size_exponent < 0 || Sys.int_size - 2 < size_exponent then
    invalid_arg "size_exponent out of range";
  let size = 1 lsl size_exponent in
  let array = Array.make size None in
  let tail = Atomic.make_contended 0 in
  let s = Obj.size (Obj.repr tail) in
  let tail_cache = Padded_int_ref.make s 0 in
  let head = Atomic.make_contended 0 in
  let head_cache = Padded_int_ref.make s 0 in
  { array; tail; tail_cache; head; head_cache }

type _ mono = Unit : unit mono | Bool : bool mono

let push_as (type r) t element (mono : r mono) : r =
  let size = Array.length t.array in
  let tail = Atomic.get t.tail in
  let head_cache = Padded_int_ref.get t.head_cache in
  if
    head_cache == tail - size
    &&
    let head = Atomic.get t.head in
    Padded_int_ref.set t.head_cache head;
    head == head_cache
  then match mono with Unit -> raise_notrace Full | Bool -> false
  else begin
    Array.unsafe_set t.array (tail land (size - 1)) (Some element);
    Atomic.incr t.tail;
    match mono with Unit -> () | Bool -> true
  end

let push_exn t element = push_as t element Unit
let try_push t element = push_as t element Bool

exception Empty

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly
type op = Peek | Pop

let pop_or_peek_as (type a r) (t : a t) op (poly : (a, r) poly) : r =
  let head = Atomic.get t.head in
  let tail_cache = Padded_int_ref.get t.tail_cache in
  if
    head == tail_cache
    &&
    let tail = Atomic.get t.tail in
    Padded_int_ref.set t.tail_cache tail;
    tail_cache == tail
  then match poly with Value -> raise_notrace Empty | Option -> None
  else
    let index = head land (Array.length t.array - 1) in
    let v = Array.unsafe_get t.array index in
    begin
      match op with
      | Pop ->
          Array.unsafe_set t.array index None;
          Atomic.incr t.head
      | Peek -> ()
    end;
    match poly with Value -> Option.get v | Option -> v

let pop_exn t = pop_or_peek_as t Pop Value
let pop_opt t = pop_or_peek_as t Pop Option
let peek_exn t = pop_or_peek_as t Peek Value
let peek_opt t = pop_or_peek_as t Peek Option

let size t =
  let tail = Atomic.get t.tail in
  let head = Atomic.get t.head in
  tail - head
