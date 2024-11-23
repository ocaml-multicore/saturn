module type Bounded_queue_tests = sig
  include Bounded_queue_intf.BOUNDED_QUEUE

  val name : string
end

module Bounded_queue : Bounded_queue_tests = struct
  include Saturn.Bounded_queue

  let name = "Bounded_queue_safe"
end

module Bounded_queue_unsafe : Bounded_queue_tests = struct
  include Saturn.Bounded_queue_unsafe

  let name = "Bounded_queue_unsafe"
end
