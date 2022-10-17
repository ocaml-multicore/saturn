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
    Printf.sprintf {| {"name":"%s", "value":%s, "units":"%s", "description":"%s"} |}
      name value_str units description
end

type t = { name : string; metrics : Metric.t list }

let to_json { name; metrics } =
  let metrics = List.map Metric.to_json metrics |> String.concat ", " in
  Printf.sprintf {| {"name": "%s", "metrics": [%s]} |} name metrics
