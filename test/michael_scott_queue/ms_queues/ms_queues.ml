module type MS_queue_tests = sig
  include Michael_scott_queue_intf.MS_QUEUE

  val name : string
end

module Michael_scott_queue : MS_queue_tests = struct
  include Saturn.Queue

  let name = "michael_scott_queue_safe"
end

module Michael_scott_queue_unsafe : MS_queue_tests = struct
  include Saturn.Queue_unsafe

  let name = "michael_scott_queue_unsafe"
end
