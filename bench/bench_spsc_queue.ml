open Multicore_bench
module Queue = Saturn.Single_prod_single_cons_queue

let run_one ~budgetf ?(size_exponent = 3) ?(n_msgs = 80 * Util.iter_factor) () =
  let t = Queue.create ~size_exponent in

  let before () =
    while Queue.size t <> 0 do
      Queue.pop t |> ignore
    done;
    let n = Random.int ((1 lsl size_exponent) + 1) in
    for i = 1 to n do
      Queue.push t i
    done
  in
  let init _ = () in
  let work i () =
    if i = 0 then
      let rec loop n =
        if 0 < n then loop (n - Bool.to_int (Queue.try_push t n))
      in
      loop n_msgs
    else
      let rec loop n =
        if 0 < n then
          match Queue.pop_opt t with Some _ -> loop (n - 1) | None -> loop n
      in
      loop n_msgs
  in

  let config = Printf.sprintf "2 workers, capacity %d" (1 lsl size_exponent) in
  Times.record ~budgetf ~n_domains:2 ~before ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  [ 0; 3; 6; 9; 12; 15 ]
  |> List.concat_map @@ fun size_exponent -> run_one ~budgetf ~size_exponent ()
