let elements = ref 100_000
let pushers = ref 4
let takers = ref 4
let iterations = ref 10
let impl = ref `FAD

let use_impl = function
  | "MS" -> impl := `MS
  | "CAS" -> impl := `CAS
  | "FAD" -> impl := `FAD
  | "UNBOUNDED" -> impl := `Unbounded
  | "WS" -> impl := `WS
  | str ->
      Printf.ksprintf failwith "-impl expected MS|CAS|FAD|UNBOUNDED|WS, got %S"
        str

let speclist =
  [
    ("-items", Arg.Set_int elements, "number of items to insert and remove");
    ("-pushers", Arg.Set_int pushers, "number of domains pushing items");
    ("-takers", Arg.Set_int takers, "number of domains taking times");
    ("-iterations", Arg.Set_int iterations, "run the benchmark this many times");
    ( "-impl",
      Arg.String use_impl,
      "queue implementation to use: MS or CAS or FAD or UNBOUNDED or WS" );
  ]

let () =
  Arg.parse speclist
    (fun _ -> ())
    "mpmc_queue.exe [-items INT] [-pushers INT] [-takers INT] [-iterations \
     INT] [-impl CAS|FAD|UNBOUNDED]";
  let result =
    Mpmc_queue.benchmark ~takers:!takers ~pushers:!pushers ~impl:!impl
      ~iterations:!iterations ~elements:!elements ()
  in
  Benchmark_result.to_json result |> Yojson.Basic.prettify |> print_endline
