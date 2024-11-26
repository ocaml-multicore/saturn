(** Lock-free single-producer, multi-consumer dynamic-size double-ended queue (deque).

      The main strength of a deque in a typical work-stealing setup with a 
      per-core structure, is efficient work distribution. The owner uses [push] 
      and [pop] methods to operate at one end of the deque, while other (free) 
      cores can efficiently steal work from the other side.

      This approach is great for throughput. Stealers and the owner working on
      different sides, reduce contention in work distribution. Further, the 
      local LIFO order, running related tasks one after another, improves locality.

      On the other hand, the local LIFO order does not offer any fairness 
      guarantees. Thus, it is not the best choice when tail latency matters.
*)

(** {1 API} *)

type 'a t
(** Type of work-stealing queue *)

val create : unit -> 'a t
(** [create ()] returns a new empty work-stealing queue. *)

val of_list : 'a list -> 'a t
(** [of_list list] creates a new work-stealing queue from [list].

  🐌 This is a linear-time operation.

 {[
        # open Saturn.Work_stealing_deque
        # let t : int t = of_list [1;2;3;4]
        val t : int t = <abstr>
        # pop_opt t
        - : int option = Some 4
        # pop_opt t 
        - : int option = Some 3
 ]}
*)

exception Empty

(** {2 Queue owner functions} *)

val push : 'a t -> 'a -> unit
(** [push queue element] adds [element] at the end of the [queue].
      It should only be invoked by the domain that owns the [queue]. *)

val pop_exn : 'a t -> 'a
(** [pop_exn queue] removes and returns the last element of the [queue]. It 
      should only be invoked by the domain that owns the [queue].

       @raises Empty if the [queue] is empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt queue] removes and returns [Some] of the last element of the 
      [queue], or returns [None] if the [queue] is empty.  *)

val drop_exn : 'a t -> unit
(** [drop_exn queue] removes the last element of the [queue]. 
            
      @raises Empty if the [queue] is empty. *)

(** {2 Stealer functions} *)

val steal_exn : 'a t -> 'a
(** [steal_exn queue] removes and returns the first element of the [queue]. 
      It should only be invoked by a domain that doesn't own the [queue].

        @raises Empty if the [queue] is empty. *)

val steal_opt : 'a t -> 'a option
(** [steal_opt queue] removes and returns [Some] of the first element of the 
      [queue], or returns [None] if the [queue] is empty. It should only be
      invoked by a domain that doesn't own the [queue]. *)

val steal_drop_exn : 'a t -> unit
(** [steal_drop_exn queue] removes the first element of the [queue]. 
            
      @raises Empty if the [queue] is empty. *)

(** {1 Examples} *)

(** {2 Sequential example} 
      An example top-level session:
{[
      # open Saturn.Work_stealing_deque
      # let t : int t = of_list [1;2;3;4;5;6]
      val t : int t = <abstr>
      # pop_opt t
      - : int option = Some 6
      # steal_opt t
      - : int option = Some 1
      # drop_exn t
      - : unit = ()
      # pop_opt t
      - : int option = Some 4
      # steal_drop_exn t
      - : unit = ()
      # steal_exn t
      - : int = 3
      # steal_exn t
      Exception: Saturn__Ws_deque.Empty.
]}
*)

(** {2 Multicore example} 
  Note: The barrier is used in this example solely to make the results more
  interesting by increasing the likelihood of parallelism. Spawning a domain is 
  a costly operation, especially compared to the relatively small amount of work
  being performed here. In practice, using a barrier in this manner is unnecessary.


{@ocaml non-deterministic=command[
      # open Saturn.Work_stealing_deque
      # let t : int t = create ()
      val t : int t = <abstr>
      # let barrier = Atomic.make 3
      val barrier : int Atomic.t = <abstr>

      # let owner () = 
            Atomic.decr barrier;
            while Atomic.get barrier <> 0 do Domain.cpu_relax () done;
            for i = 1 to 10 do push t i; Domain.cpu_relax () done
      val owner : unit -> unit = <fun>

      # let stealer id () =
            Atomic.decr barrier;
            while Atomic.get barrier <> 0 do Domain.cpu_relax () done;

            for _ = 1 to 5 do
                  match steal_opt t with
                  | None -> ()
                  | Some v -> Format.printf "Stealer %s stole %d@." id v
            done
      val stealer : string -> unit -> unit = <fun>

      # let stealerA = Domain.spawn (stealer "A")
      val stealerA : unit Domain.t = <abstr>
      # let stealerB = Domain.spawn (stealer "B")
      val stealerB : unit Domain.t = <abstr>
      # owner ()
      Stealer A stole 1
      Stealer B stole 2
      Stealer A stole 3
      Stealer B stole 4
      Stealer A stole 5
      Stealer A stole 7
      Stealer B stole 6
      Stealer A stole 8
      Stealer B stole 9
      Stealer B stole 10
      - : unit = ()
      # Domain.join stealerA; Domain.join stealerB
      - : unit = ()
]}
*)
