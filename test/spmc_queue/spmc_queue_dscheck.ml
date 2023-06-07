let drain queue =
  let remaining = ref 0 in
  while Option.is_some (Spmc_queue.local_pop queue) do
    remaining := !remaining + 1
  done;
  !remaining

let prepare_queue () =
  let queue = Spmc_queue.create ~size_exponent:4 () in
  for _ = 1 to 3 do
    assert (Spmc_queue.local_push queue 0);
    assert (Option.is_some (Spmc_queue.local_pop queue))
  done;
  queue

let push_pop_steal () =
  Atomic.trace (fun () ->
      let queue = prepare_queue () in

      let popped = ref 0 in
      let stolen = ref 0 in
      let items_total = 3 in

      (* owner *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            assert (Spmc_queue.local_push queue i)
          done;

          for _ = 1 to items_total do
            match Spmc_queue.local_pop queue with
            | None -> ()
            | Some _ -> stolen := !stolen + 1
          done);

      (* stealer *)
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            match Spmc_queue.steal_one queue with
            | None -> ()
            | Some _ -> stolen := !stolen + 1
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () -> drain queue == 0);
          Atomic.check (fun () -> !popped + !stolen = items_total)))

let push_pop_double_steal () =
  Atomic.trace (fun () ->
      let queue = prepare_queue () in
      let items_total = 2 in

      let popped = ref 0 in

      (* owner *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            assert (Spmc_queue.local_push queue i)
          done;

          match Spmc_queue.local_pop queue with
          | None -> ()
          | Some _ -> popped := !popped + 1);

      (* stealers *)
      for _ = 1 to 2 do
        Atomic.spawn (fun () ->
            match Spmc_queue.steal_one queue with
            | None -> ()
            | Some _ -> popped := !popped + 1)
      done;

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              remaining + !popped = items_total)))

let () =
  let open Alcotest in
  run "spmc_queue_dscheck"
    [
      ( "basic",
        [
          test_case "push-pop-1-stealer" `Slow push_pop_steal;
          test_case "push-pop-2-stealers" `Slow push_pop_double_steal;
        ] );
    ]
