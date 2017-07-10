(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
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

(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

(* Work Stealing Queue *)

module Cas = Kcas.W1;;

module type S = sig
  type 'a t;;
  val create : unit -> 'a t;;
  val is_empty : 'a t -> bool;;
  val size : 'a t -> int;;
  val push : 'a t -> 'a -> unit;;
  val pop : 'a t -> 'a option;;
  val steal : 'a t -> 'a option;;
end;;

module CArray = struct
  type 'a t = 'a Cas.ref array;;

  let create s v = Array.init s (fun i -> Cas.ref v);;

  let size t = Array.length t;;

  let get t i = Cas.get t.(i mod (size t));;

  let put t i v = Cas.set t.(i mod (size t)) v;;

  let grow t top bottom =
    let s = size t in
    let ns = 2 * s in
    let v = get t 0 in
    let out = create ns v in
    for i = top to bottom do
      out.(i mod (size out)) <- t.(i mod (size t))
    done;
    out
  ;;

  let shrink t top bottom =
    let s = size t in
    let ns = s / 2 in
    let v = get t 0 in
    let out = create ns v in
    for i = top to bottom do
      out.(i mod (size out)) <- t.(i mod (size t))
    done;
    out
  ;;
end;;


module M : S = struct
  let min_size = 16;;
  let shrink_const = 3;;

  type 'a t = {
    top : int Cas.ref;
    bottom : int Cas.ref;
    tab : 'a CArray.t Cas.ref
  };;

  let create () = {
    top = Cas.ref 0;
    bottom = Cas.ref 0;
    tab = Cas.ref (CArray.create min_size (Obj.magic ()))
  };;

  let is_empty q =
    let b = Cas.get q.bottom in
    let t = Cas.get q.top in
    b - t <= 0
  ;;

  let size q =
    let b = Cas.get q.bottom in
    let t = Cas.get q.top in
    b - t
  ;;

  let push q v =
    let b = Cas.get q.bottom in
    let t = Cas.get q.top in
    let a = Cas.get q.tab in
    let size = b - t in
    if size >= CArray.size a - 1 then
      Cas.set q.tab (CArray.grow a t b);
    CArray.put a b v;
    Cas.set q.bottom (b+1)
  ;;

  let pop q =
    let b = Cas.get q.bottom - 1 in
    Cas.set q.bottom b;
    let t = Cas.get q.top in
    let a = Cas.get q.tab in
    let size = b - t in
    if size < 0 then begin
      Cas.set q.bottom t;
      None
    end else
      let out = Some(CArray.get a b) in
      if size > 0 then begin
        if CArray.size a / shrink_const > size then
          Cas.set q.tab (CArray.shrink a t b);
        out
      end else begin
        Cas.set q.bottom (t+1);
        if Cas.cas q.top t (t+1) then
          out
        else
          None
      end
  ;;

  let rec steal q =
    let wait = Kcas.Backoff.create () in
    let rec loop () =
      let t = Cas.get q.top in
      let b = Cas.get q.bottom in
      let a = Cas.get q.tab in
      let size = b - t in
      if size <= 0 then
        None
      else
        let out = Some(CArray.get a t) in
        if Cas.cas q.top t (t+1) then
          out
        else begin
          Kcas.Backoff.once wait;
          loop ()
        end
    in loop ()
  ;;
end;;
