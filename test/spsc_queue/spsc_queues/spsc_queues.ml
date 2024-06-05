module type SPSC_tests = sig
  include Spsc_queue_intf.SPSC_queue

  val name : string
end

module Spsc_queue : SPSC_tests = struct
  include Saturn_lockfree.Single_prod_single_cons_queue

  let name = "Spsc_queue"
end

module Spsc_queue_unsafe : SPSC_tests = struct
  include Saturn_lockfree.Single_prod_single_cons_queue_unsafe

  let name = "Spsc_queue_unsafe"
end
