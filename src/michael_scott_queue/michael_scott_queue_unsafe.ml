(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
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

module Node = Michael_scott_queue_unsafe_node
module Atomic = Node.Atomic

type 'a t = {
  head : ('a, [ `Next ]) Node.t Atomic.t;
  tail : ('a, [ `Next ]) Node.t Atomic.t;
}

let create () =
  let node = Node.make (Obj.magic ()) in
  let head = Atomic.make node |> Multicore_magic.copy_as_padded in
  let tail = Atomic.make node |> Multicore_magic.copy_as_padded in
  { head; tail } |> Multicore_magic.copy_as_padded

let is_empty t = Atomic.get (Node.as_atomic (Atomic.get t.head)) == Nil

let of_list list =
  match list with
  | [] -> create ()
  | _ ->
      let tail, head = Node.node_of_list list in
      let head = Node.Next { next = head; value = Obj.magic () } in
      let head = Atomic.make head |> Multicore_magic.copy_as_padded in
      let tail = Atomic.make tail |> Multicore_magic.copy_as_padded in
      { head; tail } |> Multicore_magic.copy_as_padded

(* *)

exception Empty

type ('a, _) poly =
  | Option : ('a, 'a option) poly
  | Value : ('a, 'a) poly
  | Unit : ('a, unit) poly

let rec pop_as : type a r.
    (a, [ `Next ]) Node.t Atomic.t -> Backoff.t -> (a, r) poly -> r =
 fun head backoff poly ->
  let old_head = Atomic.get head in
  match Atomic.get (Node.as_atomic old_head) with
  | Nil -> begin
      match poly with Value | Unit -> raise Empty | Option -> None
    end
  | Next r as new_head ->
      if Atomic.compare_and_set head old_head new_head then begin
        let value = r.value in
        r.value <- Obj.magic ();
        match poly with Value -> value | Option -> Some value | Unit -> ()
      end
      else
        let backoff = Backoff.once backoff in
        pop_as head backoff poly

let pop_opt t = pop_as t.head Backoff.default Option
let pop_exn t = pop_as t.head Backoff.default Value
let drop_exn t = pop_as t.head Backoff.default Unit

(* *)

let rec peek_as : type a r. (a, [ `Next ]) Node.t Atomic.t -> (a, r) poly -> r =
 fun head poly ->
  let old_head = Atomic.get head in
  match Atomic.get (Node.as_atomic old_head) with
  | Nil -> begin
      match poly with Value | Unit -> raise Empty | Option -> None
    end
  | Next r ->
      let value = r.value in
      if Atomic.get head == old_head then
        match poly with Value -> value | Option -> Some value | Unit -> ()
      else peek_as head poly

let peek_opt t = peek_as t.head Option
let peek_exn t = peek_as t.head Value

(* *)

let rec fix_tail tail new_tail backoff =
  let old_tail = Atomic.get tail in
  if
    Atomic.get (Node.as_atomic new_tail) == Nil
    && not (Atomic.compare_and_set tail old_tail new_tail)
  then fix_tail tail new_tail (Backoff.once backoff)

let rec push tail link (Next _ as new_node : (_, [ `Next ]) Node.t) backoff =
  match Atomic.get link with
  | Node.Nil ->
      if Atomic.compare_and_set link Node.Nil new_node then begin
        fix_tail tail new_node Backoff.default
      end
      else
        let backoff = Backoff.once backoff in
        push tail link new_node backoff
  | Next _ as next -> push tail (Node.as_atomic next) new_node backoff

let push { tail; _ } value =
  let (Next _ as new_node : (_, [ `Next ]) Node.t) = Node.make value in
  let old_tail = Atomic.get tail in
  let link = Node.as_atomic old_tail in
  if Atomic.compare_and_set link Nil new_node then
    Atomic.compare_and_set tail old_tail new_node |> ignore
  else
    let backoff = Backoff.once Backoff.default in
    push tail link new_node backoff
