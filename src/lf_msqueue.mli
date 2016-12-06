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

module type S = sig
  type 'a t
  (** The type of lock-free queue. *)

  val create      : unit -> 'a t
  (** Create a new queue, which is initially empty. *)

  val is_empty    : 'a t -> bool
  (** [is_empty q] returns empty if [q] is empty. *)

  val push        : 'a t -> 'a -> unit
  (** [push q v] pushes [v] to the back of the queue. *)

  val pop         : 'a t -> 'a option
  (** [pop q] pops an element [e] from the front of the queue and returns 
      [Some v] if the queue is non-empty. Otherwise, returns [None]. *)

  val clean_until : 'a t -> ('a -> bool) -> unit
  (** [clean_until q f] drops the prefix of the queue until the element [e],
      where [f e] is [true]. If no such element exists, then the queue is
      emptied. *)

  type 'a cursor
  (** The type of cursor. *)

  val snapshot  : 'a t -> 'a cursor
  (** Obtain a snapshot of the queue. This is a constant time operation. *)

  val next      : 'a cursor -> ('a * 'a cursor) option
  (** [next c] returns [Some (e, c')] where [e] is the head of the queue and
      [c'] is the tail, if the queue is non-empty. Otherwise, returns [None]. *)
end

module M : S
