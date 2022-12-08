module Metric = struct
  type t = {
    name : string;
    value : [ `Text of string | `Numeric of float ];
    units : string;
    description : string;
  }

  let to_json { name; value; units; description } =
    let value_str =
      match value with
      | `Text text -> Printf.sprintf {|"%s"|} text
      | `Numeric number -> Printf.sprintf {|%f|} number
    in
    Printf.sprintf
      {| {"name":"%s", "value":%s, "units":"%s", "description":"%s"} |} name
      value_str units description
end

type t = { name : string; metrics : Metric.t list }

let to_json { name; metrics } =
  let metrics = List.map Metric.to_json metrics |> String.concat ", " in
  Printf.sprintf {| {"name": "%s", "metrics": [%s]} |} name metrics

let create_generic ?median_time ?median_throughput name =
  let time =
    Option.map
      (fun median_time : Metric.t ->
        {
          name = "time";
          value = `Numeric median_time;
          units = "s";
          description = "median time result";
        })
      median_time
  in
  let throughput =
    Option.map
      (fun median_throughput : Metric.t ->
        {
          name = "throughput";
          value = `Numeric median_throughput;
          units = "item/s";
          description = "median throughput result";
        })
      median_throughput
  in
  let metrics = [ time; throughput ] |> List.filter_map (fun v -> v) in
  if metrics = [] then
    failwith "Benchmark_result.create: require at least one metric";
  ({ name; metrics } : t)
