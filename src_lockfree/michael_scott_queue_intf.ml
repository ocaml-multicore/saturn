module type MS_QUEUE = sig
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

  (**
    Michael-Scott classic multi-producer multi-consumer queue.

   All functions are lockfree. It is the recommended starting point
   when needing FIFO structure. It is inspired by {{:
   https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf}
   Simple, Fast, and Practical Non-Blocking and Blocking Concurrent
   Queue Algorithms}.
*)

  type 'a t
  (** The type of lock-free queue. *)

  val create : unit -> 'a t
  (** [create ()] returns a new queue, initially empty. *)

  val is_empty : 'a t -> bool
  (** [is_empty q] returns empty if [q] is empty. *)

  val push_exn : 'a t -> 'a -> unit
  (** [push q v] adds the element [v] at the end of the queue [q]. *)

  exception Empty
  (** Raised when {!pop} or {!peek} is applied to an empty queue. *)

  val pop_exn : 'a t -> 'a
  (** [pop q] removes and returns the first element in queue [q].

    @raise Empty if [q] is empty. *)

  val pop_opt : 'a t -> 'a option
  (** [pop_opt q] removes and returns the first element in queue [q], or
    returns [None] if the queue is empty. *)

  val peek_exn : 'a t -> 'a
  (** [peek q] returns the first element in queue [q].

    @raise Empty if [q] is empty. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt q] returns the first element in queue [q], or
    returns [None] if the queue is empty. *)
end
