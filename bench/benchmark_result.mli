module Metric : sig
  type t = {
    name : String.t; 
    value : String.t;
    units : String.t;
    description : String.t;
  } 
end

type t = {
  name : String.t;
  metrics : Metric.t list 
} [@@deriving yojson]