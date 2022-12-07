(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2021, Tom Kelly <ctk21@cl.cam.ac.uk>
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

open Virtual_atomic

module type S = sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val steal : 'a t -> 'a
end

module CArray = struct
  type 'a t = 'a array

  let rec log2 n = if n <= 1 then 0 else 1 + log2 (n asr 1)

  let create sz v =
    (* [sz] must be a power of two. *)
    assert (0 < sz && sz = Int.shift_left 1 (log2 sz));
    assert (Int.logand sz (sz - 1) == 0);
    Array.make sz v

  let size t = Array.length t [@@inline]
  let mask t = size t - 1 [@@inline]

  let index i t =
    (* Because [size t] is a power of two, [i mod (size t)] is the same as
       [i land (size t - 1)], that is, [i land (mask t)]. *)
    Int.logand i (mask t)
    [@@inline]

  let get t i = Array.unsafe_get t (index i t) [@@inline]
  let put t i v = Array.unsafe_set t (index i t) v [@@inline]

  let transfer src dst top num =
    ArrayExtra.blit_circularly (* source array and index: *)
      src
      (index top src) (* target array and index: *)
      dst
      (index top dst) (* number of elements: *)
      num
    [@@inline]

  let grow t top bottom =
    let sz = size t in
    assert (bottom - top = sz);
    let dst = create (2 * sz) (Obj.magic ()) in
    transfer t dst top sz;
    dst

  let shrink t top bottom =
    let sz = size t in
    assert (bottom - top <= sz / 2);
    let dst = create (sz / 2) (Obj.magic ()) in
    transfer t dst top (bottom - top);
    dst
end

module M : S = struct
  let min_size = 32
  let shrink_const = 3

  type 'a t = {
    top : int Atomic.t;
    bottom : int Atomic.t;
    tab : 'a ref CArray.t Atomic.t;
    mutable next_shrink : int;
  }

  let create () =
    {
      top = Atomic.make 1;
      bottom = Atomic.make 1;
      tab = Atomic.make (CArray.create min_size (Obj.magic ()));
      next_shrink = 0;
    }

  let set_next_shrink q =
    let sz = CArray.size (Atomic.get q.tab) in
    if sz <= min_size then q.next_shrink <- 0
    else q.next_shrink <- sz / shrink_const

  let grow q t b =
    Atomic.set q.tab (CArray.grow (Atomic.get q.tab) t b);
    set_next_shrink q

  let size q =
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    b - t

  let push q v =
    let v' = ref v in
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    let a = Atomic.get q.tab in
    let size = b - t in
    let a =
      if size = CArray.size a then (
        grow q t b;
        Atomic.get q.tab)
      else a
    in
    CArray.put a b v';
    Atomic.set q.bottom (b + 1)

  let release ptr =
    let res = !ptr in
    (* we know this ptr will never be dereferenced, but want to
       break the reference to ensure that the contents of the
       deque array get garbage collected *)
    ptr := Obj.magic ();
    res
    [@@inline]

  let pop q =
    if size q = 0 then raise Exit
    else
      let b = Atomic.get q.bottom - 1 in
      Atomic.set q.bottom b;
      let t = Atomic.get q.top in
      let a = Atomic.get q.tab in
      let size = b - t in
      if size < 0 then (
        (* empty queue *)
        Atomic.set q.bottom (b + 1);
        raise Exit)
      else
        let out = CArray.get a b in
        if b = t then
          (* single last element *)
          if Atomic.compare_and_set q.top t (t + 1) then (
            Atomic.set q.bottom (b + 1);
            release out)
          else (
            Atomic.set q.bottom (b + 1);
            raise Exit)
        else (
          (* non-empty queue *)
          if q.next_shrink > size then (
            Atomic.set q.tab (CArray.shrink a t b);
            set_next_shrink q);
          release out)

  let rec steal q =
    let t = Atomic.get q.top in
    let b = Atomic.get q.bottom in
    let size = b - t in
    if size <= 0 then raise Exit
    else
      let a = Atomic.get q.tab in
      let out = CArray.get a t in
      if Atomic.compare_and_set q.top t (t + 1) then release out
      else (
        Domain.cpu_relax ();
        steal q)
end
