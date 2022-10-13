open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let benchmark_list = [ Bench_spsc_queue.bench ]

type t = { name : string; results : Benchmark_result.t list }
[@@deriving yojson]

let () =
  let results =
    List.map (fun f -> f ()) benchmark_list
  in
  Yojson.Safe.pretty_print Format.std_formatter
    (yojson_of_t ({ name = "lockfree"; results } : t))
