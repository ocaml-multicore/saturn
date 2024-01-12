open Multicore_bench
module Stack = Stdlib.Stack

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Stack.create () in

  let op push = if push then Stack.push 101 t else Stack.pop_opt t |> ignore in

  let init _ =
    assert (Stack.is_empty t);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_suite ~budgetf = run_one_domain ~budgetf ()
