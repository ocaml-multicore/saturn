module Dscheck_spsc (Spsc_queue : Spsc_queue_intf.SPSC_queue) = struct
  let create_test ~shift_by () =
    let queue = Spsc_queue.create ~size_exponent:2 in
    let items_count = 3 in

    (* shift the queue, that helps testing overlap handling *)
    for _ = 1 to shift_by do
      Spsc_queue.push_exn queue (-1);
      assert (Option.is_some (Spsc_queue.pop_opt queue))
    done;

    (* enqueuer *)
    Atomic.spawn (fun () ->
        for i = 1 to items_count do
          Spsc_queue.push_exn queue i
        done);

    (* dequeuer *)
    let dequeued = ref 0 in
    Atomic.spawn (fun () ->
        for _ = 1 to items_count + 1 do
          let peeked = Spsc_queue.peek_opt queue in
          match Spsc_queue.pop_opt queue with
          | None -> assert (peeked = None)
          | Some v as popped ->
              assert (v = !dequeued + 1);
              assert (popped = peeked || peeked = None);
              dequeued := v
        done);

    (* ending assertions *)
    Atomic.final (fun () ->
        Atomic.check (fun () ->
            Spsc_queue.size queue == items_count - !dequeued))

  let with_trace ?(shift_by = 0) f () = Atomic.trace (fun () -> f ~shift_by ())

  let size_linearizes_with_1_thr () =
    Atomic.trace (fun () ->
        let queue = Spsc_queue.create ~size_exponent:4 in
        Spsc_queue.push_exn queue (-1);
        Spsc_queue.push_exn queue (-1);

        Atomic.spawn (fun () ->
            for _ = 1 to 4 do
              Spsc_queue.push_exn queue (-1)
            done);

        let size = ref 0 in
        Atomic.spawn (fun () ->
            assert (Option.is_some (Spsc_queue.pop_opt queue));
            size := Spsc_queue.size queue);

        Atomic.final (fun () ->
            Atomic.check (fun () -> 1 <= !size && !size <= 5)))

  let tests name =
    let open Alcotest in
    [
      ( "basic_" ^ name,
        [ test_case "simple-test" `Slow (with_trace create_test) ] );
      ( "wrap-arounds_" ^ name,
        let with_shift s =
          test_case
            (Printf.sprintf "shift-by-%d" s)
            `Slow
            (with_trace ~shift_by:s create_test)
        in
        [ with_shift 1; with_shift 6; with_shift 11 ] );
      ( "size_" ^ name,
        [ test_case "linearizes-with-1-thr" `Slow size_linearizes_with_1_thr ]
      );
    ]
end

let () =
  let module Safe = Dscheck_spsc (Spsc_queue) in
  let safe_test = Safe.tests "safe" in
  let module Unsafe = Dscheck_spsc (Spsc_queue_unsafe) in
  let unsafe_test = Unsafe.tests "unsafe" in

  let open Alcotest in
  run "spsc_queue_dscheck" (safe_test @ unsafe_test)
