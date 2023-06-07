let create_test ~shift_by () =
  let queue = Spsc_queue.create ~size_exponent:2 in
  let items_count = 3 in

  (* shift the queue, that helps testing overlap handling *)
  for _ = 1 to shift_by do
    assert (Spsc_queue.try_push queue (-1));
    assert (Option.is_some (Spsc_queue.try_pop queue))
  done;

  (* enqueuer *)
  Atomic.spawn (fun () ->
      for i = 1 to items_count do
        assert (Spsc_queue.try_push queue i)
      done);

  (* dequeuer *)
  let dequeued = ref 0 in
  Atomic.spawn (fun () ->
      for _ = 1 to items_count + 1 do
        match Spsc_queue.try_pop queue with
        | None -> ()
        | Some v ->
            assert (v = !dequeued + 1);
            dequeued := v
      done);

  (* ending assertions *)
  Atomic.final (fun () ->
      Atomic.check (fun () -> Spsc_queue.size queue == items_count - !dequeued))

let with_trace ?(shift_by = 0) f () = Atomic.trace (fun () -> f ~shift_by ())

let size_linearizes_with_1_thr () =
  Atomic.trace (fun () ->
      let queue = Spsc_queue.create ~size_exponent:4 in
      assert (Spsc_queue.try_push queue (-1));
      assert (Spsc_queue.try_push queue (-1));

      Atomic.spawn (fun () ->
          for _ = 1 to 4 do
            assert (Spsc_queue.try_push queue (-1))
          done);

      let size = ref 0 in
      Atomic.spawn (fun () ->
          assert (Option.is_some (Spsc_queue.try_pop queue));
          size := Spsc_queue.size queue);

      Atomic.final (fun () -> Atomic.check (fun () -> 1 <= !size && !size <= 5)))

let () =
  let open Alcotest in
  run "Spsc_queue_dscheck"
    [
      ("basic", [ test_case "simple-test" `Slow (with_trace create_test) ]);
      ( "wrap-arounds",
        let with_shift s =
          test_case
            (Printf.sprintf "shift-by-%d" s)
            `Slow
            (with_trace ~shift_by:s create_test)
        in
        [ with_shift 1; with_shift 6; with_shift 11 ] );
      ( "size",
        [ test_case "linearizes-with-1-thr" `Slow size_linearizes_with_1_thr ]
      );
    ]
