let drain_remaining queue =
  let remaining = ref [] in
  (try
     while true do
       remaining := Ws_deque.pop_exn queue :: !remaining
     done
   with _ -> ());
  !remaining

let owner_stealer () =
  Atomic.trace (fun () ->
      let queue = Ws_deque.create () in
      let total_items = 3 in

      let popped = ref 0 in

      (* owner thr *)
      Atomic.spawn (fun () ->
          for _ = 1 to total_items do
            Ws_deque.push queue 0
          done;
          for _ = 1 to total_items / 2 do
            match Ws_deque.pop_exn queue with
            | exception _ -> ()
            | _ -> popped := !popped + 1
          done);

      (* stealer *)
      Atomic.spawn (fun () ->
          for _ = 1 to total_items / 2 do
            match Ws_deque.steal_exn queue with
            | exception _ -> ()
            | _ -> popped := !popped + 1
          done);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain_remaining queue |> List.length in
              remaining + !popped == total_items)))

let of_list_stealers () =
  Atomic.trace (fun () ->
      let total_items = 3 in
      let queue = Ws_deque.of_list (List.init total_items (fun x -> x + 1)) in

      (* stealers *)
      let popped = ref 0 in
      Atomic.spawn (fun () ->
          Ws_deque.push queue (total_items + 1);
          popped := Ws_deque.pop_exn queue);

      let stolen = ref [] in
      let stealer () =
        match Ws_deque.steal_exn queue with
        | exception _ -> ()
        | v -> stolen := v :: !stolen
      in
      for _ = 1 to total_items - 1 do
        Atomic.spawn stealer
      done;

      Atomic.final (fun () ->
          Atomic.check (fun () -> !popped = total_items + 1);
          Atomic.check (fun () ->
              List.sort Int.compare !stolen
              = List.init (total_items - 1) (fun x -> x + 1));
          Atomic.check (fun () -> drain_remaining queue = [ total_items ])))

let popper_stealer () =
  Atomic.trace (fun () ->
      let total_items = 5 in
      let queue = Ws_deque.of_list (List.init total_items (fun _ -> 0)) in

      (* stealers *)
      let popped = ref 0 in
      Atomic.spawn (fun () ->
          match Ws_deque.pop_opt queue with
          | None -> ()
          | _ -> popped := !popped + 1);

      let stealer () =
        match Ws_deque.steal_exn queue with
        | exception _ -> ()
        | _ -> popped := !popped + 1
      in
      for _ = 1 to total_items - 2 do
        Atomic.spawn stealer
      done;

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain_remaining queue |> List.length in
              remaining = 1 && !popped = total_items - 1)))

let popper_stealer_drop () =
  Atomic.trace (fun () ->
      let total_items = 5 in
      let queue = Ws_deque.of_list (List.init total_items (fun _ -> 0)) in

      (* stealers *)
      let popped = ref 0 in
      Atomic.spawn (fun () ->
          match Ws_deque.drop_exn queue with
          | exception Ws_deque.Empty -> ()
          | _ -> popped := !popped + 1);

      let stealer () =
        match Ws_deque.steal_drop_exn queue with
        | exception Ws_deque.Empty -> ()
        | _ -> popped := !popped + 1
      in
      for _ = 1 to total_items - 2 do
        Atomic.spawn stealer
      done;

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain_remaining queue |> List.length in
              remaining = 1 && !popped = total_items - 1)))

let owner_2_stealers () =
  Atomic.trace (fun () ->
      let queue = Ws_deque.create () in
      let total_items = 6 in

      let popped = ref [] in
      (* owner thr *)
      Atomic.spawn (fun () ->
          for i = 1 to total_items do
            Ws_deque.push queue i
          done;
          for _ = 1 to total_items / 3 do
            match Ws_deque.pop_exn queue with
            | exception _ -> ()
            | v -> popped := v :: !popped
          done);

      let stolen1 = ref [] in
      (* stealer *)
      Atomic.spawn (fun () ->
          for _ = 1 to total_items / 3 do
            match Ws_deque.steal_exn queue with
            | exception _ -> ()
            | v -> stolen1 := v :: !stolen1
          done);

      let stolen2 = ref [] in
      (* stealer *)
      Atomic.spawn (fun () ->
          for _ = 1 to total_items / 3 do
            match Ws_deque.steal_exn queue with
            | exception _ -> ()
            | v -> stolen2 := v :: !stolen2
          done);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain_remaining queue in
              let total = !popped @ !stolen1 @ !stolen2 @ remaining in
              List.sort Int.compare total
              = List.init total_items (fun x -> x + 1))))

let () =
  let open Alcotest in
  run "ws_deque_dscheck"
    [
      ( "basic",
        [
          test_case "1-owner-1-stealer" `Slow owner_stealer;
          test_case "1-popped-n-stealers" `Slow popper_stealer;
          test_case "1-popped-n-stealers-drop" `Slow popper_stealer_drop;
          test_case "1-owner-2-stealers" `Slow owner_2_stealers;
          test_case "of_list-n-stealers" `Slow of_list_stealers;
        ] );
    ]
