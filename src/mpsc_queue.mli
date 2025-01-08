(** Lock-free multi-producer, single-consumer, domain-safe queue without support
    for cancellation.

    This data structure is well-suited for use as a scheduler's run queue.

    {b Warning}: This queue does not include safety mechanisms to prevent
    misuse. If consumer-only functions are called concurrently by multiple
    domains, the queue may enter an unexpected state, due to data races and a
    lack of linearizability. *)

(** {1 API} *)

type 'a t
(** Represents a single-consumer queue of items of type ['a]. *)

exception Closed

val create : unit -> 'a t
(** [create ()] returns a new empty single-consumer queue. *)

val of_list : 'a list -> 'a t
(** [of_list l] creates a new single-consumer queue from list [l].

    🐌 This is a linear-time operation.

    {[
      # open Saturn.Single_consumer_queue
      # let t : int t = of_list [1; 2; 3]
      val t : int t = <abstr>
      # pop_opt t
      - : int option = Some 1
      # peek_opt t
      - : int option = Some 2
      # pop_opt t
      - : int option = Some 2
      # pop_opt t
      - : int option = Some 3
    ]} *)

(** {2 Producer-only functions} *)

val push : 'a t -> 'a -> unit
(** [push q v] adds the element [v] at the end of the queue [q]. This can be
    used safely by multiple producer domains, in parallel with the other
    operations.

    @raise Closed if [q] is closed. *)

val push_all : 'a t -> 'a list -> unit
(** [push_all q vs] adds all the elements [vs] at the end of the queue [q]. This
    can be used safely by multiple producer domains, in parallel with the other
    operations.

    @raise Closed if [q] is closed.

    🐌 This is a linear-time operation on the size of [vs].

    {[
      # open Saturn.Single_consumer_queue
      # let t : int t = create ()
      val t : int t = <abstr>
      # push_all t [1; 2; 3]
      - : unit = ()
      # pop_opt t
      - : int option = Some 1
      # peek_opt t
      - : int option = Some 2
      # pop_opt t
      - : int option = Some 2
      # pop_opt t
      - : int option = Some 3
      # pop_exn t
      Exception: Saturn__Mpsc_queue.Empty.
    ]} *)

(** {2 Consumer-only functions} *)

exception Empty
(** Raised when {!pop_exn} or {!peek_exn} is applied to an empty queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] is [true] if calling [pop_exn] would return [None]. This can
    only be used by the consumer.

    @raise Closed if [q] is closed and empty. *)

val close : 'a t -> unit
(** [close q] marks [q] as closed, preventing any further items from being
    pushed by the producers (i.e. with {!push}). This can only be used by the
    consumer.

    @raise Closed if [q] has already been closed. *)

val pop_exn : 'a t -> 'a
(** [pop_exn q] removes and returns the first element in queue [q]. This can
    only be used by the consumer.

    @raise Empty if [q] is empty.

    @raise Closed if [q] is closed and empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt q] removes and returns the first element in queue [q] or returns
    [None] if the queue is empty. This can only be used by the consumer.

    @raise Closed if [q] is closed and empty. *)

val drop_exn : 'a t -> unit
(** [drop_exn q] removes the first element in queue [q]. This can only be used
    by the consumer.

    @raise Empty if [q] is empty.

    @raise Closed if [q] is closed and empty. *)

val peek_exn : 'a t -> 'a
(** [peek_exn q] returns the first element in queue [q]. This can only be used
    by the consumer

    @raise Empty if [q] is empty.

    @raise Closed if [q] is closed and empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt q] returns the first element in queue [q] or returns [None] if the
    queue is empty. This can only be used by the consumer.

    @raise Closed if [q] is closed and empty. *)

val push_head : 'a t -> 'a -> unit
(** [push_head q v] adds the element [v] at the head of the queue [q]. This can
    only be used by the consumer (if run in parallel with {!pop_exn}, the item
    might be skipped).

    @raise Closed if [q] is closed and empty. *)

(** {1 Examples} *)

(** {2 Sequential example}
    An example top-level session:
    {[
      # open Saturn.Single_consumer_queue
      # let t : int t = create ()
      val t : int t = <abstr>
      # push t 1
      - : unit = ()
      # push t 42
      - : unit = ()
      # pop_opt t
      - : int option = Some 1
      # peek_opt t
      - : int option = Some 42
      # drop_exn t
      - : unit = ()
      # pop_exn t
      Exception: Saturn__Mpsc_queue.Empty.
    ]} *)

(** {2 Multicore example}
    {b Note}: The barrier is used in this example solely to make the results
    more interesting by increasing the likelihood of parallelism. Spawning a
    domain is a costly operation, especially compared to the relatively small
    amount of work being performed here. In practice, using a barrier in this
    manner is unnecessary.

    {@ocaml non-deterministic=command[
      # open Saturn.Single_consumer_queue
      # let t : (string * int) t = create ()
      val t : (string * int) t = <abstr>
      # let barrier = Atomic.make 3
      val barrier : int Atomic.t = <abstr>
      # let n = 10
      val n : int = 10

      # let work_consumer () =
          Atomic.decr barrier;
          while Atomic.get barrier <> 0 do Domain.cpu_relax () done;
          for i = 1 to n do
            begin
            match pop_opt t with
            | None -> Printf.printf "Empty.\n%!"
            | Some (s, n) ->
                Printf.printf "Consumed ressource #%d from %s.\n%!" n s
            end;
            Domain.cpu_relax ()
          done;
      val work_consumer : unit -> unit = <fun>

      # let work_producer id () =
          Atomic.decr barrier;
          while Atomic.get barrier <> 0 do Domain.cpu_relax () done;
          List.init n Fun.id
          |>  List.iter (fun i -> push t (id , i);
                                  Domain.cpu_relax ())
      val work_producer : string -> unit -> unit = <fun>

      # let consumer = Domain.spawn work_consumer
      val consumer : unit Domain.t = <abstr>
      # let producerA = Domain.spawn (work_producer "A")
      val producerA : unit Domain.t = <abstr>
      # let producerB = Domain.spawn (work_producer "B")
      Empty.
      Consumed ressource #0 from A.
      Consumed ressource #0 from B.
      Consumed ressource #1 from B.
      Consumed ressource #2 from B.
      Consumed ressource #3 from B.
      Consumed ressource #4 from B.
      Consumed ressource #5 from B.
      Consumed ressource #6 from B.
      Consumed ressource #7 from B.
      val producerB : unit Domain.t = <abstr>

      # Domain.join consumer
      - : unit = ()
      # Domain.join producerA
      - : unit = ()
      # Domain.join producerB
      - : unit = ()
    ]} *)
