(** Optimized lock-free single-producer, single-consumer queue. 
  
   {b Warning}: This queue does not include safety mechanisms to prevent 
    misuse. If consumer-only functions are called concurrently by multiple
    domains, the queue may enter an unexpected state, due to data races 
    and a lack of linearizability. The same goes for producer-only functions. 
  *)

include Spsc_queue_intf.SPSC_queue
