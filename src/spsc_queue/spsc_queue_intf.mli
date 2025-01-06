module type SPSC_queue = sig
  (** {1 API} *)

  type 'a t
  (** Represents a single-producer single-consumer non-resizable queue that
      works in FIFO order. *)

  val create : size_exponent:int -> 'a t
  (** [create ~size_exponent] creates a new single-producer single-consumer
      queue with a maximum size of [2^size_exponent] and initially empty.

      🐌 This is a linear-time operation in [2^size_exponent]. *)

  val of_list_exn : size_exponent:int -> 'a list -> 'a t
  (** [of_list_exn ~size_exponent list] creates a new queue from a list.

      @raise Full if the length of [list] is greater than [2^size_exponent].

      🐌 This is a linear-time operation.

      {[
        # open Saturn.Single_prod_single_cons_queue
        # let t : int t = of_list_exn ~size_exponent:6 [1;2;3;4]
        val t : int t = <abstr>
        # pop_opt t
        - : int option = Some 1
        # pop_opt t
        - : int option = Some 2
      ]} *)

  val length : 'a t -> int
  (** [length] returns the length of the queue. This method linearizes only when
      called from either the consumer or producer domain. Otherwise, it is safe
      to call but provides only an *indication* of the size of the structure. *)

  (** {2 Producer functions} *)

  exception Full
  (** Raised when {!push_exn} is applied to a full queue. *)

  val push_exn : 'a t -> 'a -> unit
  (** [push queue elt] adds the element [elt] at the end of the [queue]. This
      method can be used by at most one domain at a time.

      @raise Full if the [queue] is full. *)

  val try_push : 'a t -> 'a -> bool
  (** [try_push queue elt] tries to add the element [elt] at the end of the
      [queue]. If the queue [q] is full, [false] is returned. This method can be
      used by at most one domain at a time. *)

  (** {2 Consumer functions} *)

  exception Empty
  (** Raised when {!pop_exn}, {!peek_exn}, or {!drop_exn} is applied to an empty
      queue. *)

  val pop_exn : 'a t -> 'a
  (** [pop_exn queue] removes and returns the first element in [queue]. This
      method can be used by at most one domain at a time.

      @raise Empty if the [queue] is empty. *)

  val pop_opt : 'a t -> 'a option
  (** [pop_opt queue] removes and returns [Some] of the first element of the
      [queue], or [None] if the queue is empty. This method can be used by at
      most one domain at a time. *)

  val peek_exn : 'a t -> 'a
  (** [peek_exn queue] returns the first element in [queue] without removing it.
      This method can be used by at most one domain at a time.

      @raise Empty if the [queue] is empty. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt queue] returns [Some] of the first element in [queue], or [None]
      if the queue is empty. This method can be used by at most one domain at a
      time. *)

  val drop_exn : 'a t -> unit
  (** [drop_exn queue] removes the top element of the [queue].

      @raise Empty if the [queue] is empty. *)

  (** {1 Examples} *)

  (** {2 Sequential example} *)

  (** {[
        # open Saturn.Single_prod_single_cons_queue
        # let t : int t = create ~size_exponent:2
        val t : int t = <abstr>
        # push_exn t 1
        - : unit = ()
        # push_exn t 2
        - : unit = ()
        # try_push t 3
        - : bool = true
        # try_push t 4
        - : bool = true
        # try_push t 5
        - : bool = false

        # pop_opt t
        - : int option = Some 1
        # peek_opt t
        - : int option = Some 2
        # drop_exn t
        - : unit = ()
        # pop_exn t
        - : int = 3
        # pop_opt t
        - : int option = Some 4
        # pop_exn t
        Exception: Saturn__Spsc_queue.Empty.
      ]} *)

  (** {2 Parallel example}
      Note: The barrier is used in this example solely to make the results more
      interesting by increasing the likelihood of parallelism. Spawning a domain
      is a costly operation, especially compared to the relatively small amount
      of work being performed here. In practice, using a barrier in this manner
      is unnecessary.

      {@ocaml non-deterministic=command[
        # open Saturn.Single_prod_single_cons_queue
        # let t : int t = create ~size_exponent:5
        val t : int t = <abstr>

        # let nwork = 5
        val nwork : int = 5
        # let barrier = Atomic.make 2

        val barrier : int Atomic.t = <abstr>
        # let consumer_work () =
            (* Atomic.decr barrier;
            while Atomic.get barrier <> 0 do Domain.cpu_relax () done; *)
            let rec loop n =
              if n < 1 then ()
              else
                (Domain.cpu_relax ();
                match pop_opt t with
                | Some p -> Format.printf "Popped %d\n%!" p; loop (n-1)
                | None ->  loop n)
            in
            loop nwork
        val consumer_work : unit -> unit = <fun>

        # let producer_work () =
            (* Atomic.decr barrier;
            while Atomic.get barrier <> 0 do Domain.cpu_relax () done; *)
            for i = 1 to nwork do
              Domain.cpu_relax ();
              try_push t i |> ignore;
              Format.printf "Pushed %d\n%!" i
            done
        val producer_work : unit -> unit = <fun>

        # let consumer = Domain.spawn consumer_work
        val consumer : unit Domain.t = <abstr>
        # let producer = Domain.spawn producer_work
        Pushed 1
        Popped 1
        Pushed 2
        Popped 2
        Pushed 3
        Popped 3
        Pushed 4
        Popped 4
        Popped 5
        Pushed 5
        val producer : unit Domain.t = <abstr>
        # Domain.join consumer
        - : unit = ()
        # Domain.join producer
        - : unit = ()
      ]} *)
end
