open Priority_queue

let _two_mem () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 () in
      let found1 = ref false in
      let found2 = ref false in

      Atomic.spawn (fun () ->
          push sl 1;
          found1 := contains sl 1);

      Atomic.spawn (fun () -> found2 := contains sl 2);

      Atomic.final (fun () -> Atomic.check (fun () -> !found1 && not !found2)))

let _two_mem_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 () in
      let found1 = ref false in
      let found2 = ref false in

      Atomic.spawn (fun () ->
          push sl 1;
          found1 := contains sl 1);

      Atomic.spawn (fun () ->
          push sl 1;
          found2 := contains sl 1);

      Atomic.final (fun () -> Atomic.check (fun () -> !found1 && !found2)))

let _extra_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 () in
      let removed1 = ref false in
      let removed2 = ref false in

      Atomic.spawn (fun () ->
          push sl 1;
          removed1 := pop sl <> Int.max_int);
      Atomic.spawn (fun () -> removed2 := pop sl <> Int.max_int);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              ((!removed1 && not !removed2) || ((not !removed1) && !removed2))
              && not (contains sl 1))))

let _two_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:1 () in
      let removed1 = ref false in
      let removed2 = ref false in

      Atomic.spawn (fun () ->
          push sl 1;
          removed1 := pop sl <> Int.max_int);
      Atomic.spawn (fun () ->
          push sl 2;
          removed2 := pop sl <> Int.max_int);

      Atomic.final (fun () -> Atomic.check (fun () -> !removed1 && !removed2)))

let _remove_mem () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:1 () in
      let removed1 = ref false in

      Atomic.spawn (fun () ->
          push sl 1;
          removed1 := pop sl <> Int.max_int);
      Atomic.spawn (fun () -> push sl 1);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !removed1 && contains sl 1)))

let _two_remove_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:1 () in
      let removed1 = ref false in
      let removed2 = ref false in

      Atomic.spawn (fun () ->
          push sl 1;
          removed1 := pop sl = 1);
      Atomic.spawn (fun () ->
          push sl 1;
          removed2 := pop sl = 1);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              (!removed1 && !removed2) && not (contains sl 1))))

let () =
  let open Alcotest in
  run "lockfree_pq_dscheck"
    [
      ( "basic",
        [
          test_case "2-mem" `Slow _two_mem;
          test_case "2-mem-same" `Slow _two_mem_same;
          test_case "extra-remove" `Slow _extra_remove;
          test_case "2-remove" `Slow _two_remove;
          test_case "remove-mem" `Slow _remove_mem;
          test_case "2-remove-same" `Slow _two_remove_same;
        ] );
    ]
