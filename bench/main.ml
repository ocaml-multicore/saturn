let benchmark_list = 
  [ Bench_spsc_queue.bench;
    Mpmc_queue.bench ~takers:4 ~pushers:4;
    Mpmc_queue.bench ~takers:1 ~pushers:8; 
    Mpmc_queue.bench ~takers:8 ~pushers:1;
    Mpmc_queue.bench ~use_cas:true ~takers:4 ~pushers:4;
    Mpmc_queue.bench ~use_cas:true ~takers:1 ~pushers:8;
    Mpmc_queue.bench ~use_cas:true ~takers:8 ~pushers:1;
]

let () =
  let results =
    (* todo: should assert no stranded domains between tests. *)
    List.map (fun f -> f ()) benchmark_list
    |> List.map Benchmark_result.to_json
    |> String.concat ", "
  in
  let output = 
    Printf.sprintf 
    {| {"name": "lockfree", "results": [%s]}|} 
    results
    (* Cannot use Yojson rewriters as of today none works on OCaml 5.1.0. 
       This at least verifies that the manually crafted JSON is well-formed. 
       
       If the type grow, we could switch to running ppx manually on 5.0.0 and 
       pasting in its output. *)
    |> Yojson.Basic.prettify
  in
  Printf.printf "%s" output