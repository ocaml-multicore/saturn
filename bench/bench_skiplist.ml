open Saturn

let workload num_elems num_threads add remove =
  let sl = Skiplist.create ~compare:Int.compare () in
  let elems = Array.init num_elems (fun _ -> Random.int 10000) in
  let push () =
    Domain.spawn (fun () ->
        let start_time = Unix.gettimeofday () in
        for i = 0 to (num_elems - 1) / num_threads do
          Domain.cpu_relax ();
          let prob = Random.float 1.0 in
          if prob < add then Skiplist.try_add sl (Random.int 10000) () |> ignore
          else if prob >= add && prob < add +. remove then
            Skiplist.try_remove sl (Random.int 10000) |> ignore
          else Skiplist.mem sl elems.(i) |> ignore
        done;
        start_time)
  in
  let threads = List.init num_threads (fun _ -> push ()) in
  let start_time_threads =
    List.map (fun domain -> Domain.join domain) threads
  in
  let end_time = Unix.gettimeofday () in
  let time_diff = end_time -. List.nth start_time_threads 0 in
  time_diff

(* A write heavy workload with threads with 50% adds and 50% removes. *)
let write_heavy_workload num_elems num_threads =
  workload num_elems num_threads 0.5 0.5

(* A regular workload with 90% reads, 9% adds and 1% removes. *)
let read_heavy_workload num_elems num_threads =
  workload num_elems num_threads 0.09 0.01

let moderate_heavy_workload num_elems num_threads =
  workload num_elems num_threads 0.2 0.1

let balanced_heavy_workload num_elems num_threads =
  workload num_elems num_threads 0.3 0.2

let bench ~workload_type ~num_elems ~num_threads () =
  let workload =
    if workload_type = "read_heavy" then read_heavy_workload
    else if workload_type = "moderate_heavy" then moderate_heavy_workload
    else if workload_type = "balanced_heavy" then balanced_heavy_workload
    else write_heavy_workload
  in
  let results = ref [] in
  for i = 1 to 10 do
    let time = workload num_elems num_threads in
    if i > 1 then results := time :: !results
  done;
  let results = List.sort Float.compare !results in
  let median_time = List.nth results 4 in
  let median_throughput = Float.of_int num_elems /. median_time in
  Benchmark_result.create_generic ~median_time ~median_throughput
    ("atomic_skiplist_" ^ workload_type)
