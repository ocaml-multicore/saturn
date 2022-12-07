val bench :
  ?takers:int ->
  ?pushers:int ->
  ?use_cas:bool ->
  ?iterations:int ->
  ?elements:int ->
  unit ->
  Benchmark_result.t
