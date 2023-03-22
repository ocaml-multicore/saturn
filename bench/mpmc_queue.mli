val bench : (unit -> Benchmark_result.t) list

val benchmark :
  takers:int ->
  pushers:int ->
  impl:[ `MS | `CAS | `FAD | `Unbounded | `WS ] ->
  iterations:int ->
  elements:int ->
  unit ->
  Benchmark_result.t
