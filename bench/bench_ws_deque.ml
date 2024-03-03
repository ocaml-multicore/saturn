open Multicore_bench
module Ws_deque = Saturn_lockfree.Work_stealing_deque.M

let run_as_scheduler ~budgetf ?(n_domains = 1) () =
  let spawns =
    Array.init n_domains @@ fun _ -> ref 0 |> Multicore_magic.copy_as_padded
  in
  let deques = Array.init n_domains @@ fun _ -> Ws_deque.create () in
  let exit = ref false |> Multicore_magic.copy_as_padded in

  let next i =
    let i = i + 1 in
    if i = n_domains then 0 else i
  in

  let rec try_own own =
    match Ws_deque.pop (Array.unsafe_get deques own) with
    | work -> work
    | exception Exit -> try_steal own (next own)
  and try_steal own other =
    if other = own then raise_notrace Exit
    else
      match Ws_deque.steal (Array.unsafe_get deques other) with
      | work -> work
      | exception Exit -> try_steal own (next other)
  in
  let rec run own =
    match try_own own with
    | work ->
        work own;
        run own
    | exception Exit ->
        if not !exit then begin
          Domain.cpu_relax ();
          run own
        end
  in

  let spawn own work =
    incr (Array.unsafe_get spawns own);
    let promise = ref (Obj.magic exit) in
    Ws_deque.push (Array.unsafe_get deques own) (fun own -> promise := work own);
    promise
  in
  let rec await own promise =
    let x = !promise in
    if x == Obj.magic exit then begin
      begin
        match try_own own with
        | exception Exit -> Domain.cpu_relax ()
        | work -> work own
      end;
      await own promise
    end
    else x
  in

  let rec fib n worker =
    if n < 2 then n
    else
      let n2 = spawn worker (fib (n - 2)) in
      let n1 = fib (n - 1) worker in
      await worker n2 + n1
  in

  let rec bits n = if n <= 1 then 0 else 1 + bits (n lsr 1) in

  let init own =
    Array.unsafe_get spawns own := 0;
    if own = 0 then begin
      exit := false;
      let n = 27 + bits n_domains in
      spawn own (fun own ->
          fib n own |> ignore;
          exit := true)
      |> ignore
    end
  in
  let work own () = run own in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in
  let times = Times.record ~budgetf ~n_domains ~init ~work () in
  let n = Array.fold_left (fun n c -> n + !c) 0 spawns in
  Times.to_thruput_metrics ~n ~singular:"spawn" ~config times

let run_as_one_domain ~budgetf ?(n_msgs = 150 * Util.iter_factor) order =
  let t = Ws_deque.create () in

  let op_lifo push =
    if push then Ws_deque.push t 101
    else match Ws_deque.pop t with _ -> () | exception Exit -> ()
  and op_fifo push =
    if push then Ws_deque.push t 101
    else match Ws_deque.steal t with _ -> () | exception Exit -> ()
  in

  let init _ =
    assert (match Ws_deque.steal t with _ -> false | exception Exit -> true);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits =
    Util.Bits.iter (match order with `FIFO -> op_fifo | `LIFO -> op_lifo) bits
  in

  let config =
    let label = match order with `FIFO -> "FIFO" | `LIFO -> "LIFO" in
    Printf.sprintf "one domain (%s)" label
  in
  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_as_spmc ~budgetf ~n_thiefs () =
  let n_domains = n_thiefs + 1 in

  let n_msgs = 70 * Util.iter_factor in

  let t = Ws_deque.create () in

  let n_msgs_to_steal = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    assert (match Ws_deque.steal t with _ -> false | exception Exit -> true);
    Atomic.set n_msgs_to_steal n_msgs
  in
  let work i () =
    if i < n_thiefs then
      let rec work () =
        let n = Util.alloc n_msgs_to_steal in
        if 0 < n then
          let rec loop n =
            if 0 < n then
              match Ws_deque.steal t with
              | exception Exit ->
                  Domain.cpu_relax ();
                  loop n
              | _ -> loop (n - 1)
            else work ()
          in
          loop n
      in
      work ()
    else
      for i = 1 to n_msgs do
        Ws_deque.push t i
      done
  in

  let config =
    Printf.sprintf "1 adder, %d taker%s" n_thiefs
      (if n_thiefs = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  List.concat
    [
      [ 1; 2; 4; 8 ]
      |> List.concat_map (fun n_domains ->
             run_as_scheduler ~budgetf ~n_domains ());
      [ 1; 2; 4 ]
      |> List.concat_map (fun n_thiefs -> run_as_spmc ~budgetf ~n_thiefs ());
      run_as_one_domain ~budgetf `FIFO;
      run_as_one_domain ~budgetf `LIFO;
    ]
