(** Helper library that ensures all workers have started before any
  starts making progress on the benchmark. *)

type t
(** An orchestrator is similar to a counter that ensures each domain
    has started and complete each round simultanously. All domains
    wait for the other before beginning the next round. *)

val init : total_domains:int -> rounds:int -> t
(** [init ~total_domains:nd ~rounds:nr] create an orchestrator that
    will run [nr] rounds for a test that uses exactly [nd] worker
    domains *)

val worker : t -> (unit -> unit) -> unit
(** [worker t f] builds the function to pass to [Domain.spawn] while
    using the orchestrator [t]. Doing [Domain.spawn (fun () -> worker
    t f)] is similar to [Domain.spawn f] except that the orchestrator
    is used to synchronize all domains progress.
 *)

val run : ?drop_first:bool -> t -> float List.t
(** [run t] is launching the benchmark by enabling domains to progress. Benchmarks code should have the following structure :

{[
   (* Initialize the orchestrator, with [nd] the number of domains we want. *)
   let orchestrator = init ~total_domain:nd ~round:100 in
   (* Spawn domains with [worker] *)
   let domains =
         List.init nd (fun _ ->
               Domain.spawn (fun () ->
                      worker orchestrator (fun () -> some_function ()))) in
   (* Run the benchmarks by freeing domains round by round. *)
   let times = run orchestrator in
   ...
]}

*)
