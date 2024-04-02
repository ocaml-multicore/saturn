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

type 'a t = {
  array : 'a Option.t Array.t;
  tail : int Atomic.t;
  head : int Atomic.t;
  mask : int;
}

exception Full

let create ~size_exponent =
  let size = Int.shift_left 1 size_exponent in
  {
    head = Atomic.make 0 |> Multicore_magic.copy_as_padded;
    tail = Atomic.make 0 |> Multicore_magic.copy_as_padded;
    mask = size - 1;
    array = Array.init size (fun _ -> None);
  }
  |> Multicore_magic.copy_as_padded

let push { array; head; tail; mask; _ } element =
  let size = mask + 1 in
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  if head_val + size == tail_val then raise Full
  else (
    Array.set array (tail_val land mask) (Some element);
    Atomic.set tail (tail_val + 1))

let try_push { array; head; tail; mask; _ } element =
  let size = mask + 1 in
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  if head_val + size == tail_val then false
  else (
    Array.set array (tail_val land mask) (Some element);
    Atomic.set tail (tail_val + 1);
    true)

exception Empty

let pop { array; head; tail; mask; _ } =
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  if head_val == tail_val then raise Empty
  else
    let index = head_val land mask in
    let v = Array.get array index in
    (* allow gc to collect it *)
    Array.set array index None;
    Atomic.set head (head_val + 1);
    match v with None -> assert false | Some v -> v

let pop_opt { array; head; tail; mask; _ } =
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  if head_val == tail_val then None
  else
    let index = head_val land mask in
    let v = Array.get array index in
    (* allow gc to collect it *)
    Array.set array index None;
    Atomic.set head (head_val + 1);
    assert (Option.is_some v);
    v

let peek_opt { array; head; tail; mask; _ } =
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  if head_val == tail_val then None
  else
    let v = Array.get array @@ (head_val land mask) in
    assert (Option.is_some v);
    v

let peek { array; head; tail; mask; _ } =
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  if head_val == tail_val then raise Empty
  else
    let v = Array.get array @@ (head_val land mask) in
    match v with None -> assert false | Some v -> v

let size { head; tail; _ } = Atomic.get tail - Atomic.get head
