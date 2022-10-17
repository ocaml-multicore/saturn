let benchmark_list = [ Bench_spsc_queue.bench ]

let () =
  let results =
    List.map (fun f -> f ()) benchmark_list
    |> List.map Benchmark_result.to_json
    |> String.concat ", "
  in
  let output = 
    Printf.sprintf 
    {| {"name": "lockfree", "results": %s}|} 
    results
    (* Cannot use Yojson rewriters as of today none works on OCaml 5.1.0. 
       This at least verifies that the manually crafted JSON is well-formed. *)
    |> Yojson.Basic.prettify
  in
  Printf.printf "%s" output