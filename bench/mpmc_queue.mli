val bench : (unit -> Benchmark_result.t) list

val benchmark :
  takers:int ->
  pushers:int ->
  impl:[ `CAS | `FAD | `Unbounded ] ->
  iterations:int ->
  elements:int ->
  unit ->
  Benchmark_result.t
