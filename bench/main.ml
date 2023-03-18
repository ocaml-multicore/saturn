let backoff_benchmarks =
  let open Backoff in
  [
    bench_basic ~with_backoff:true;
    bench_basic ~with_backoff:false;
    bench_artificial ~with_backoff:true;
    bench_artificial ~with_backoff:false;
  ]

let benchmark_list =
  Bench_spsc_queue.bench @ Mpmc_queue.bench @ backoff_benchmarks

let () =
  List.iter
    (fun f ->
      (* todo: should assert no stranded domains between tests. *)
      let r = f () in
      let r = Benchmark_result.to_json r in
      let output =
        Printf.sprintf {| {"name": "lockfree", "results": [%s]}|} r
        (* Cannot use Yojson rewriters as of today none works on OCaml 5.1.0.
           This at least verifies that the manually crafted JSON is well-formed.

           If the type grow, we could switch to running ppx manually on 5.0.0 and
           pasting in its output. *)
        |> Yojson.Basic.prettify
      in
      Printf.printf "%s\n%!" output)
    benchmark_list
