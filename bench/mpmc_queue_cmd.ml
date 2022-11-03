let elements = ref 100_000 
let pushers = ref 4 
let takers = ref 4 
let iterations = ref 10
let use_cas = ref false 

let speclist =
  [ ("-items", Arg.Set_int elements, "number of items to insert and remove");
    ("-pushers", Arg.Set_int pushers, "number of domains pushing items");
    ("-takers", Arg.Set_int takers, "number of domains taking times");
    ("-iterations", Arg.Set_int iterations, "run the benchmark this many times");
    ("-use-cas", Arg.Set use_cas, "use CAS instead of FAD")
  ]

let _f () =
  Arg.parse speclist
    (fun _ -> ())
    "mpmc_queue.exe [-items INT] [-pushers INT] [-takers INT] [-iterations INT] [-use-cas]";
  let result = Mpmc_queue.bench ~takers:!takers ~pushers:!pushers ~use_cas:!use_cas ~iterations:!iterations ~elements:!elements () in 
  Benchmark_result.to_json result 
  |> Yojson.Basic.prettify 
  |> print_string;;
