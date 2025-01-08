(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2021, Tom Kelly <ctk21@cl.cam.ac.uk>
 * Copyright (c) 2024, Vesa Karvonen <vesa.a.j.k@gmail.com>
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

(* Work Stealing Queue
 *
 * See:
 *   Dynamic circular work-stealing deque
 *   https://dl.acm.org/doi/10.1145/1073970.1073974
 *  &
 *   Correct and efficient work-stealing for weak memory models
 *   https://dl.acm.org/doi/abs/10.1145/2442516.2442524
 *)

module Atomic = Multicore_magic.Transparent_atomic

(** This must be a power of two. *)
let min_capacity = 16

type 'a t = {
  top : int Atomic.t;
  bottom : int Atomic.t;
  top_cache : int ref;
  mutable tab : 'a ref array;
}

let create () =
  let top = Atomic.make_contended 0 in
  let tab = Array.make min_capacity (Obj.magic ()) in
  let bottom = Atomic.make_contended 0 in
  let top_cache = ref 0 |> Multicore_magic.copy_as_padded in
  { top; bottom; top_cache; tab } |> Multicore_magic.copy_as_padded

let next_pow2 n =
  let rec loop acc = if acc >= n then acc else loop (acc lsl 1) in
  loop 1

let of_list l =
  let len = List.length l in
  let capacity = max min_capacity (next_pow2 len) in
  let top = Atomic.make_contended 0 in
  let tab = Array.make capacity (Obj.magic ()) in
  List.iteri (fun i x -> Array.unsafe_set tab i (ref x)) l;
  let bottom = Atomic.make_contended len in
  let top_cache = ref 0 |> Multicore_magic.copy_as_padded in
  { top; bottom; top_cache; tab } |> Multicore_magic.copy_as_padded

(* *)

let realloc a t b sz new_sz =
  let new_a = Array.make new_sz (Obj.magic ()) in
  ArrayExtra.blit_circularly a
    (t land (sz - 1))
    new_a
    (t land (new_sz - 1))
    (b - t);
  new_a

let push q v =
  let v = ref v in
  (* Read of [bottom] by the owner simply does not require a fence as the
     [bottom] is only mutated by the owner. *)
  let b = Atomic.fenceless_get q.bottom in
  let t_cache = !(q.top_cache) in
  let a = q.tab in
  let size = b - t_cache in
  let capacity = Array.length a in
  if
    size < capacity
    ||
    let t = Atomic.get q.top in
    q.top_cache := t;
    t != t_cache
  then begin
    Array.unsafe_set a (b land (capacity - 1)) v;
    Atomic.incr q.bottom
  end
  else
    let a = realloc a t_cache b capacity (capacity lsl 1) in
    Array.unsafe_set a (b land (Array.length a - 1)) v;
    q.tab <- a;
    Atomic.incr q.bottom

(* *)

type ('a, _) poly =
  | Option : ('a, 'a option) poly
  | Value : ('a, 'a) poly
  | Unit : ('a, unit) poly

exception Empty

let pop_as : type a r. a t -> (a, r) poly -> r =
 fun q poly ->
  let b = Atomic.fetch_and_add q.bottom (-1) - 1 in
  (* Read of [top] at this point requires no fence as we simply need to ensure
     that the read happens after updating [bottom]. *)
  let t = Atomic.fenceless_get q.top in
  let size = b - t in
  if 0 < size then begin
    let a = q.tab in
    let capacity = Array.length a in
    let out = Array.unsafe_get a (b land (capacity - 1)) in
    let res = !out in
    out := Obj.magic ();
    if size + size + size <= capacity - min_capacity then
      q.tab <- realloc a t b capacity (capacity lsr 1);
    match poly with Option -> Some res | Value -> res | Unit -> ()
  end
  else if b = t then begin
    (* Whether or not the [compare_and_set] below succeeds, [top_cache] can be
       updated, because in either case [top] has been incremented. *)
    q.top_cache := t + 1;
    let got = Atomic.compare_and_set q.top t (t + 1) in
    (* This write of [bottom] requires no fence.  The deque is empty and
       remains so until the next [push]. *)
    Atomic.fenceless_set q.bottom (b + 1);
    if got then begin
      let a = q.tab in
      let out = Array.unsafe_get a (b land (Array.length a - 1)) in
      let res = !out in
      out := Obj.magic ();
      match poly with Option -> Some res | Value -> res | Unit -> ()
    end
    else match poly with Option -> None | Value | Unit -> raise Empty
  end
  else begin
    (* This write of [bottom] requires no fence.  The deque is empty and
       remains so until the next [push]. *)
    Atomic.fenceless_set q.bottom (b + 1);
    match poly with Option -> None | Value | Unit -> raise Empty
  end

let pop_exn q = pop_as q Value
let pop_opt q = pop_as q Option
let drop_exn q = pop_as q Unit

(* *)

let rec steal_as : type a r. a t -> Backoff.t -> (a, r) poly -> r =
 fun q backoff poly ->
  (* Read of [top] does not require a fence at this point, but the read of
     [top] must happen before the read of [bottom].  The write of [top] later
     has no effect in case we happened to read an old value of [top]. *)
  let t = Atomic.fenceless_get q.top in
  let b = Atomic.get q.bottom in
  let size = b - t in
  if 0 < size then
    let a = q.tab in
    let out = Array.unsafe_get a (t land (Array.length a - 1)) in
    if Atomic.compare_and_set q.top t (t + 1) then begin
      let res = !out in
      out := Obj.magic ();
      match poly with Option -> Some res | Value -> res | Unit -> ()
    end
    else steal_as q (Backoff.once backoff) poly
  else match poly with Option -> None | Value | Unit -> raise Empty

let steal_exn q = steal_as q Backoff.default Value
let steal_opt q = steal_as q Backoff.default Option
let steal_drop_exn q = steal_as q Backoff.default Unit
