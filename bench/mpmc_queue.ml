open Lockfree.Mpmc_relaxed_queue

let num_of_elements = ref 500_000
let num_of_pushers = ref 4
let num_of_takers = ref 4
let num_of_iterations = ref 10
let use_cas_intf = ref false
let pop = ref Not_lockfree.pop
let push = ref Not_lockfree.push

let taker queue num_of_elements () =
  let i = ref 0 in
  while !i < num_of_elements do
    if Option.is_some (!pop queue) then i := !i + 1
  done

let pusher queue num_of_elements () =
  let i = ref 0 in
  while !i < num_of_elements do
    if !push queue !i then i := !i + 1
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
    Printf.sprintf "mpmc-queue-pushers:%d,takers:%d,use-cas:%b" !num_of_pushers
      !num_of_takers !use_cas_intf
  in
  ({ name; metrics } : Benchmark_result.t)

let run_bench () =
  if !use_cas_intf then (
    push := Lockfree.Mpmc_relaxed_queue.Not_lockfree.CAS_interface.push;
    pop := Lockfree.Mpmc_relaxed_queue.Not_lockfree.CAS_interface.pop);
  let queue = create ~size_exponent:10 () in
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

let bench ?takers ?pushers ?use_cas ?iterations ?elements () =
  num_of_takers := Option.value takers ~default:!num_of_takers;
  num_of_pushers := Option.value pushers ~default:!num_of_pushers;
  use_cas_intf := Option.value use_cas ~default:!use_cas_intf;
  num_of_iterations := Option.value iterations ~default:!num_of_iterations;
  num_of_elements := Option.value elements ~default:!num_of_elements;
  run_bench ()
