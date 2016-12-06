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

(* Michael-Scott queue *)

(* TODO KC: Replace with concurrent lock free bag --
 * http://dl.acm.org/citation.cfm?id=1989550 *)


module type S = sig
  type 'a t
  val create      : unit -> 'a t
  val is_empty    : 'a t -> bool
  val push        : 'a t -> 'a -> unit
  val pop         : 'a t -> 'a option
  val clean_until : 'a t -> ('a -> bool) -> unit

  type 'a cursor
  val snapshot  : 'a t -> 'a cursor
  val next      : 'a cursor -> ('a * 'a cursor) option
end

module M : S = struct

  type 'a node =
    | Nil
    | Next of 'a * 'a node Kcas.ref

  type 'a t =
    { head : 'a node Kcas.ref ;
      tail : 'a node Kcas.ref }

  let create () =
    let head = (Next (Obj.magic (), Kcas.ref Nil)) in
    { head = Kcas.ref head ; tail = Kcas.ref head }

  let is_empty q =
    match Kcas.get q.head with
    | Nil -> failwith "MSQueue.is_empty: impossible"
    | Next (_,x) ->
        ( match Kcas.get x with
          | Nil -> true
          | _ -> false )

  let pop q =
    let b = Kcas.Backoff.create () in
    let rec loop () =
      let s = Kcas.get q.head in
      let nhead = match s with
        | Nil -> failwith "MSQueue.pop: impossible"
        | Next (_, x) -> Kcas.get x
      in match nhead with
       | Nil -> None
       | Next (v, _) when Kcas.cas q.head s nhead -> Some v
       | _ -> ( Kcas.Backoff.once b ; loop () )
    in loop ()

  let push q v =
    let rec find_tail_and_enq curr_end node =
      if Kcas.cas curr_end Nil node then ()
      else match Kcas.get curr_end with
           | Nil -> find_tail_and_enq curr_end node
           | Next (_, n) -> find_tail_and_enq n node
    in
    let newnode = Next (v, Kcas.ref Nil) in
    let tail = Kcas.get q.tail in
    match tail with
    | Nil         -> failwith "HW_MSQueue.push: impossible"
    | Next (_, n) -> begin
        find_tail_and_enq n newnode;
        ignore (Kcas.cas q.tail tail newnode)
    end

  let rec clean_until q f =
    let b = Kcas.Backoff.create () in
    let rec loop () =
      let s = Kcas.get q.head in
      let nhead = match s with
        | Nil -> failwith "MSQueue.pop: impossible"
        | Next (_, x) -> Kcas.get x
      in match nhead with
       | Nil -> ()
       | Next (v, _) ->
           if not (f v) then
              if Kcas.cas q.head s nhead
              then (Kcas.Backoff.reset b; loop ())
              else (Kcas.Backoff.once b; loop ())
           else ()
    in loop ()

  type 'a cursor = 'a node

  let snapshot q =
    match Kcas.get q.head with
    | Nil -> failwith "MSQueue.snapshot: impossible"
    | Next (_, n) -> Kcas.get n

  let next c =
    match c with
    | Nil -> None
    | Next (a, n) -> Some (a, Kcas.get n)

end
