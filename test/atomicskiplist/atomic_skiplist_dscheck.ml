(* This dscheck testcase is not terminating. *)
let two_producers () =
  Atomic.trace (fun () ->
      let sl = Atomicskiplist.create 10 in

      Atomic.spawn (fun () -> Atomicskiplist.mem sl 2 |> ignore);
      Atomic.spawn (fun () -> Atomicskiplist.mem sl 3 |> ignore);

      Atomic.final (fun () ->
          (* Atomic.check (fun () -> !present) *)
          Atomic.check (fun () -> true)))

let () =
  let open Alcotest in
  run "atomic_skiplist_dscheck"
    [ ("basic", [ test_case "2-producers" `Slow two_producers ]) ]
