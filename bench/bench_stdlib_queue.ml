open Multicore_bench
module Queue = Stdlib.Queue

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Queue.create () in

  let op push = if push then Queue.push 101 t else Queue.take_opt t |> ignore in

  let init _ =
    assert (Queue.is_empty t);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_suite ~budgetf = run_one_domain ~budgetf ()
