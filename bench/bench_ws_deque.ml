open Multicore_bench
module Ws_deque = Saturn_lockfree.Work_stealing_deque.M

let run_one ~budgetf ?(n_domains = 1) () =
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
    | exception Exit -> if not !exit then run own
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
        match try_own own with exception Exit -> () | work -> work own
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

let run_suite ~budgetf =
  [ 1; 2; 4; 8 ]
  |> List.concat_map @@ fun n_domains -> run_one ~budgetf ~n_domains ()
