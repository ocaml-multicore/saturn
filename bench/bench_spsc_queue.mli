module Spsc_queue : sig
  val bench : unit -> Benchmark_result.t
end

module Mpmc_queue : sig
  val bench : unit -> Benchmark_result.t
end
