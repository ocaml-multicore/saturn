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

module type MS_QUEUE = sig
  (**
    Michael-Scott classic multi-producer multi-consumer queue.

   All functions are lockfree. It is the recommended starting point
   when needing FIFO structure. It is inspired by {{:
   https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf}
   Simple, Fast, and Practical Non-Blocking and Blocking Concurrent
   Queue Algorithms}.

   If you need a [length] function, you can use the bounded queue
   {!Saturn.Bounded_queue} instead with maximun capacity (default value).
   However, this adds a general overhead to the operation.
*)

  (** {1 API} *)

  type 'a t
  (** Represents a lock-free queue holding elements of type ['a]. *)

  val create : unit -> 'a t
  (** [create ()] returns a new queue, initially empty. *)

  val of_list : 'a list -> 'a t
  (** [of_list list] creates a new queue from a list.
          
    🐌 This is a linear-time operation.
    
    {[
      # open Saturn.Queue
      # let t : int t = of_list [1;2;3;4]
      val t : int t = <abstr>
      # pop_opt t
      - : int option = Some 1
      # pop_opt t 
      - : int option = Some 2
    ]}
    *)

  val is_empty : 'a t -> bool
  (** [is_empty q] returns [true] if [q] is empty and [false] otherwise. *)

  (** {2 Consumer functions} *)

  exception Empty
  (** Raised when {!pop_exn}, {!peek_exn}, or {!drop_exn} is applied to an empty
   queue. *)

  val peek_exn : 'a t -> 'a
  (** [peek_exn queue] returns the first element of the [queue] without removing it.
       
     @raises Empty if the [queue] is empty. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt queue] returns [Some] of the first element of the [queue] without
       removing it, or [None] if the [queue] is empty. *)

  val pop_exn : 'a t -> 'a
  (** [pop_exn queue] removes and returns the first element of the [queue].
   
    @raises Empty if the [queue] is empty. *)

  val pop_opt : 'a t -> 'a option
  (** [pop_opt q] removes and returns the first element in queue [q], or
    returns [None] if the queue is empty. *)

  val drop_exn : 'a t -> unit
  (** [drop_exn queue] removes the top element of the [queue]. 
      
    @raises Empty if the [queue] is empty. *)

  (** {2 Producer functions} *)

  val push : 'a t -> 'a -> unit
  (** [push q v] adds the element [v] at the end of the queue [q]. *)

  (** {1 Examples}  *)

  (** {2 Sequential example}
  
   An example top-level session:
    {[
      # open Saturn.Queue
      # let t : int t = of_list [1;2;3]
      val t : int t = <abstr>
      # push t 42
      - : unit = ()
      # pop_exn t
      - : int = 1
      # peek_opt t
      - : int option = Some 2
      # drop_exn t
      - : unit = ()
      # pop_opt t
      - : int option = Some 3
      # pop_opt t
      - : int option = Some 42
      # pop_exn t
      Exception: Saturn__Michael_scott_queue.Empty.]}
  *)

  (** {2 Parallel example} 
  Note: The barrier is used in this example solely to make the results more
   interesting by increasing the likelihood of parallelism. Spawning a domain is 
   a costly operation, especially compared to the relatively small amount of work
   being performed here. In practice, using a barrier in this manner is unnecessary.


    {@ocaml non-deterministic=command[
      # open Saturn.Queue
      # let t : string t = create ()
      val t : string t = <abstr>
      # Random.self_init ()
      - : unit = ()
      # let barrier = Atomic.make 2
      val barrier : int Atomic.t = <abstr>

      # let work id =
          Atomic.decr barrier;
          while Atomic.get barrier <> 0 do
            Domain.cpu_relax ()
          done;
          for _ = 1 to 4 do
            Domain.cpu_relax ();
            if Random.bool () then push t id
            else
             match pop_opt t with
              | None -> Format.printf "Domain %s sees an empty queue.\n%!" id
              | Some v -> Format.printf "Domain %s pops values pushed by %s.\n%!" id v
          done
      val work : string -> unit = <fun>

      # let domainA = Domain.spawn (fun () -> work "A")
      val domainA : unit Domain.t = <abstr>
      # let domainB = Domain.spawn (fun () -> work "B")
      Domain B pops values pushed by B.
      Domain A pops values pushed by A.
      Domain B pops values pushed by A.
      Domain B pops values pushed by A.
      val domainB : unit Domain.t = <abstr>

      # Domain.join domainA; Domain.join domainB
      - : unit = ()
    ]}
  *)
end
