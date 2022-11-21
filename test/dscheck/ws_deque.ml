module Atomic = Dscheck.TracedAtomic
open Lockfree

let drain_remaining queue =
  let remaining = ref 0 in
  (try
     while true do
       Ws_deque.M.pop queue |> ignore;
       remaining := !remaining + 1
     done
   with _ -> ());
  !remaining

let owner_stealer () =
  Atomic.trace (fun () ->
      let queue = Ws_deque.M.create () in
      let total_items = 3 in

      let popped = ref 0 in

      (* owner thr *)
      Atomic.spawn (fun () ->
          for _ = 1 to total_items do
            Ws_deque.M.push queue 0
          done;
          for _ = 1 to total_items / 2 do
            match Ws_deque.M.pop queue with
            | exception _ -> ()
            | _ -> popped := !popped + 1
          done);

      (* stealer *)
      Atomic.spawn (fun () ->
          for _ = 1 to total_items / 2 do
            match Ws_deque.M.steal queue with
            | exception _ -> ()
            | _ -> popped := !popped + 1
          done);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain_remaining queue in
              remaining + !popped == total_items)))

let popper_stealer () =
  Atomic.trace (fun () ->
      let queue = Ws_deque.M.create () in
      let total_items = 3 in
      for _ = 1 to total_items do
        Ws_deque.M.push queue 0
      done;

      (* stealers *)
      let popped = ref 0 in
      let stealer () =
        match Ws_deque.M.steal queue with
        | exception _ -> ()
        | _ -> popped := !popped + 1
      in
      Atomic.spawn stealer |> ignore;
      Atomic.spawn stealer |> ignore;

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain_remaining queue in
              remaining = 1 && !popped = 2)))

let () =
  let open Alcotest in
  run "ws_deque_dscheck"
    [
      ( "basic",
        [
          test_case "1-owner-1-stealer" `Slow owner_stealer;
          test_case "1-pusher-2-stealers" `Slow popper_stealer;
          (* we'd really want to test cases with more threads here,
             but dscheck is not optimized enough for that yet *)
        ] );
    ]
