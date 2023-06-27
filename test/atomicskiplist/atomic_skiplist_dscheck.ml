open Atomicskiplist

let _two_mem () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 () in
      let added1 = ref false in
      let found1 = ref false in
      let found2 = ref false in

      Atomic.spawn (fun () ->
          added1 := add sl 1;
          found1 := mem sl 1);

      Atomic.spawn (fun () -> found2 := mem sl 2);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !added1 && !found1 && not !found2)))

let _two_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 () in
      let added1 = ref false in
      let added2 = ref false in

      Atomic.spawn (fun () -> added1 := add sl 1);
      Atomic.spawn (fun () -> added2 := add sl 2);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !added1 && !added2 && mem sl 1 && mem sl 2)))

let _two_add_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 () in
      let added1 = ref false in
      let added2 = ref false in

      Atomic.spawn (fun () -> added1 := add sl 1);
      Atomic.spawn (fun () -> added2 := add sl 1);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              (!added1 && not !added2)
              || (((not !added1) && !added2) && mem sl 1))))

let _two_remove_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:1 () in
      let added1 = ref false in
      let removed1 = ref false in
      let removed2 = ref false in

      Atomic.spawn (fun () ->
          added1 := add sl 1;
          removed1 := remove sl 1);
      Atomic.spawn (fun () -> removed2 := remove sl 1);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              !added1
              && ((!removed1 && not !removed2) || ((not !removed1) && !removed2))
              && not (mem sl 1))))

let _two_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:1 () in
      let added1 = ref false in
      let removed1 = ref false in
      let removed2 = ref false in

      Atomic.spawn (fun () ->
          added1 := add sl 1;
          removed1 := remove sl 1);
      Atomic.spawn (fun () -> removed2 := remove sl 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let found1 = mem sl 1 in
              !added1 && !removed1 && not !removed2 && not found1)))

let () =
  let open Alcotest in
  run "skiplist_dscheck"
    [
      ( "basic",
        [
          test_case "2-mem" `Slow _two_mem;
          test_case "2-add-same" `Slow _two_add_same;
          test_case "2-add" `Slow _two_add;
          test_case "2-remove-same" `Slow _two_remove_same;
          test_case "2-remove" `Slow _two_remove;
        ] );
    ]
