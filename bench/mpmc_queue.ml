module type QUEUE = sig
  type 'a t

  val make : unit -> int t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val name : string
end

module Bench (Q : QUEUE) = struct
  let num_of_elements = ref 2_100_000
  let num_of_pushers = ref 4
  let num_of_takers = ref 4
  let num_of_iterations = ref 20

  let taker queue num_of_elements () =
    let i = ref 0 in
    while !i < num_of_elements do
      if Option.is_some (Q.pop queue) then i := !i + 1
    done

  let pusher queue num_of_elements () =
    for i = 0 to num_of_elements - 1 do
      Q.push queue i
    done

  let create_output ~time_median ~throughput_median ~throughput_stddev =
    let time =
      ({
         name = "time";
         value = `Numeric time_median;
         units = "s";
         description = "median time";
       }
        : Benchmark_result.Metric.t)
    in
    let throughput =
      ({
         name = "throughput";
         value = `Numeric throughput_median;
         units = "item/s";
         description = "median throughput";
       }
        : Benchmark_result.Metric.t)
    in
    let throughput_stddev =
      ({
         name = "throughput-stddev";
         value = `Numeric throughput_stddev;
         units = "item/s";
         description = "stddev throughput";
       }
        : Benchmark_result.Metric.t)
    in
    let metrics = [ time; throughput; throughput_stddev ] in
    let name =
      Printf.sprintf "%s-pushers:%d,takers:%d" Q.name !num_of_pushers
        !num_of_takers
    in
    ({ name; metrics } : Benchmark_result.t)

  let run_bench () =
    let queue = Q.make () in
    let orchestrator =
      Orchestrator.init
        ~total_domains:(!num_of_takers + !num_of_pushers)
        ~rounds:!num_of_iterations
    in
    (* define function to start domains *)
    let start_n_domains n f =
      assert (!num_of_elements mod n == 0);
      let items_per_pusher = !num_of_elements / n in
      List.init n (fun _ ->
          Domain.spawn (fun () ->
              Orchestrator.worker orchestrator (f queue items_per_pusher)))
    in
    (* start domains *)
    let domains =
      let takers = start_n_domains !num_of_takers taker in
      let pushers = start_n_domains !num_of_pushers pusher in
      Sys.opaque_identity (pushers @ takers)
    in
    (* run test *)
    let times = Orchestrator.run orchestrator in
    List.iter Domain.join domains;
    let time_median = Stats.median times in
    let throughputs =
      List.map (fun time -> Int.to_float !num_of_elements /. time) times
    in
    let throughput_median = Stats.median throughputs in
    let throughput_stddev = Stats.stddev throughputs in
    create_output ~time_median ~throughput_median ~throughput_stddev

  let benchmark ?takers ?pushers ?iterations ?elements () =
    num_of_takers := Option.value takers ~default:!num_of_takers;
    num_of_pushers := Option.value pushers ~default:!num_of_pushers;
    num_of_iterations := Option.value iterations ~default:!num_of_iterations;
    num_of_elements := Option.value elements ~default:!num_of_elements;
    run_bench ()

  let bench : (unit -> _) list =
    [
      benchmark ~takers:1 ~pushers:1;
      benchmark ~takers:4 ~pushers:4;
      benchmark ~takers:1 ~pushers:7;
      benchmark ~takers:7 ~pushers:1;
    ]
end

module Michael_scott_queue = Bench (struct
  let name = "michael-scott-queue"

  include Lockfree.Michael_scott_queue

  let make () = create ()
end)

module Relaxed = Bench (struct
  let name = "mpmc-relaxed-fad"

  module Q = Lockfree.Mpmc_relaxed_queue
  include Q.Not_lockfree

  type 'a t = 'a Q.t

  let make () = Q.create ~size_exponent:10 ()
  let rec push t x = if not (Q.Not_lockfree.push t x) then push t x
end)

module Relaxed_cas = Bench (struct
  let name = "mpmc-relaxed-cas"

  module Q = Lockfree.Mpmc_relaxed_queue
  include Q.Not_lockfree.CAS_interface

  type 'a t = 'a Q.t

  let make () = Q.create ~size_exponent:10 ()

  let rec push t x =
    if not (Q.Not_lockfree.CAS_interface.push t x) then push t x
end)

module Unbounded = Bench (struct
  let name = "mpmc-unbounded"

  include Lockfree.Mpmc_queue

  let make () = make ~dummy:(-1) ()
end)

let bench =
  Michael_scott_queue.bench @ Relaxed.bench @ Relaxed_cas.bench
  @ Unbounded.bench

let benchmark ~takers ~pushers ~impl ~iterations ~elements () =
  let impl =
    match impl with
    | `CAS -> Relaxed_cas.benchmark
    | `FAD -> Relaxed.benchmark
    | `Unbounded -> Unbounded.benchmark
  in
  impl ~takers ~pushers ~iterations ~elements ()
