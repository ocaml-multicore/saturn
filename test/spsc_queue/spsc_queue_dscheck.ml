module Atomic = Dscheck.TracedAtomic

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
            Spsc_queue.length queue == items_count - !dequeued))

  let with_trace ?(shift_by = 0) f () = Atomic.trace (fun () -> f ~shift_by ())

  let length_linearizes_with_1_thr () =
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
            size := Spsc_queue.length queue);

        Atomic.final (fun () ->
            Atomic.check (fun () -> 1 <= !size && !size <= 5)))

  let of_list_exn () =
    Atomic.trace (fun () ->
        let queue = Spsc_queue.of_list_exn ~size_exponent:4 [ 1; 2; 3 ] in

        Atomic.spawn (fun () ->
            for i = 4 to 6 do
              Spsc_queue.try_push queue i |> ignore
            done);

        let popped1 = ref [] in
        let popped2 = ref [] in
        Atomic.spawn (fun () ->
            for _ = 1 to 3 do
              popped1 := Spsc_queue.pop_opt queue :: !popped1
            done;
            for _ = 4 to 6 do
              popped2 := Spsc_queue.pop_opt queue :: !popped2
            done);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                List.map Option.get !popped1 = List.rev [ 1; 2; 3 ]);
            Atomic.check (fun () ->
                let popped2 =
                  List.filter Option.is_some !popped2 |> List.map Option.get
                in
                match popped2 with
                | [] | [ 4 ] | [ 5; 4 ] | [ 6; 5; 4 ] -> true
                | _ -> false)))

  let drop_exn () =
    Atomic.trace (fun () ->
        let queue = Spsc_queue.of_list_exn ~size_exponent:4 [ 1; 2; 3 ] in

        Atomic.spawn (fun () ->
            for i = 4 to 6 do
              Spsc_queue.try_push queue i |> ignore
            done);

        let popped1 = ref [] in
        let popped2 = ref [] in
        Atomic.spawn (fun () ->
            Spsc_queue.drop_exn queue;
            for _ = 2 to 3 do
              popped1 := Spsc_queue.pop_opt queue :: !popped1
            done;
            for _ = 4 to 6 do
              popped2 := Spsc_queue.pop_opt queue :: !popped2
            done);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                List.map Option.get !popped1 = List.rev [ 2; 3 ]);
            Atomic.check (fun () ->
                let popped2 =
                  List.filter Option.is_some !popped2 |> List.map Option.get
                in
                match popped2 with
                | [] | [ 4 ] | [ 5; 4 ] | [ 6; 5; 4 ] -> true
                | _ -> false)))

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
      ( "length_" ^ name,
        [ test_case "linearizes-with-1-thr" `Slow length_linearizes_with_1_thr ]
      );
      ("of_list_exn_" ^ name, [ test_case "of_list" `Slow of_list_exn ]);
      ("drop_exn " ^ name, [ test_case "drop_exn" `Slow drop_exn ]);
    ]
end

let () =
  let module Safe = Dscheck_spsc (Spsc_queue) in
  let safe_test = Safe.tests "safe" in
  let module Unsafe = Dscheck_spsc (Spsc_queue_unsafe) in
  let unsafe_test = Unsafe.tests "unsafe" in

  let open Alcotest in
  run "spsc_queue_dscheck" (safe_test @ unsafe_test)
