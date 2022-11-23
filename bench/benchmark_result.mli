module Metric : sig
  type t = {
    name : String.t;
    value : [ `Text of string | `Numeric of float ];
    units : String.t;
    description : String.t;
  }
end

type t = { name : String.t; metrics : Metric.t list }

val to_json : t -> string

val create_generic : ?median_time : float -> ?median_throughput : float -> string -> t
