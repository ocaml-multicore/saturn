module Metric = struct 
  type t = {
    name : string; 
    value : string;
    units : string;
    description : string;
  } [@@deriving yojson]
end 


type t = {
  name : string;
  metrics : Metric.t list 
} [@@deriving yojson]

  

