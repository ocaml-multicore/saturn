module Atomic = Dscheck.TracedAtomic

module Dscheck_htbl (Htbl : Htbl_intf.HTBL) = struct
  open Htbl

  let try_add htbl k = try_add htbl k k

  module Int = struct
    type t = int

    let equal = Int.equal
    let hash = Hashtbl.hash
  end

  let create ?min_buckets () = create ?min_buckets ~hashed_type:(module Int) ()

  let _two_mem () =
    Atomic.trace ~record_traces:true (fun () ->
        Random.init 0;

        let htbl = create () in
        let added = List.init 10 (fun i -> try_add htbl i) in
        let found1, found2, found3, found4 =
          (ref false, ref false, ref false, ref false)
        in

        Atomic.spawn (fun () ->
            found1 := mem htbl 1;
            found2 := mem htbl 20);
        Atomic.spawn (fun () ->
            found3 := mem htbl 1;
            found4 := mem htbl 2);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                List.for_all (fun x -> x) added
                && !found1 && (not !found2) && !found3 && !found4)));

    Dscheck.Trace_tracker.print_traces stdout

  let _two_add () =
    Atomic.trace (fun () ->
        Random.init 0;
        let htbl = create ~min_buckets:8 () in
        try_add htbl 1 |> ignore;
        let added1, added2, added3, added4 =
          (ref false, ref false, ref false, ref false)
        in

        Atomic.spawn (fun () ->
            added1 := try_add htbl 1;
            added2 := try_add htbl 21);
        Atomic.spawn (fun () ->
            added4 := try_add htbl 22;
            added3 := try_add htbl 1);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                (not !added1) && !added2 && (not !added3) && !added4
                && mem htbl 1 && mem htbl 21 && mem htbl 22)))

  let _two_add_resize () =
    (* Should trigger a resize *)
    Atomic.trace ~interleavings:stdout ~record_traces:true (fun () ->
        Random.init 0;
        let htbl = create ~min_buckets:2 () in
        try_add htbl 1 |> ignore;
        try_add htbl 2 |> ignore;

        let added1, added2 = (ref false, ref false) in

        Atomic.spawn (fun () -> added1 := try_add htbl 21);
        Atomic.spawn (fun () -> added2 := try_add htbl 22);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                !added1 && !added2 && mem htbl 21 && mem htbl 22)));
    Dscheck.Trace_tracker.print_traces stdout

  let _two_add_resize2 () =
    Atomic.trace ~interleavings:stdout ~record_traces:true (fun () ->
        Random.init 6;
        let htbl = create ~min_buckets:2 () in
        try_add htbl 1 |> ignore;
        try_add htbl 2 |> ignore;

        let added1, added2 = (ref false, ref false) in

        Atomic.spawn (fun () -> added1 := try_add htbl 1);
        Atomic.spawn (fun () -> added2 := try_add htbl 22);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                (not !added1) && !added2 && mem htbl 1 && mem htbl 22)));
    Dscheck.Trace_tracker.print_traces stdout

  let _two_remove () =
    let random_offset = Random.int 1000 in
    Atomic.trace (fun () ->
        Random.init (random_offset + 0);
        let htbl = create ~min_buckets:8 () in
        for i = 0 to 19 do
          try_add htbl i |> ignore
        done;
        let removed = List.init 10 (fun i -> try_remove htbl i) in
        let removed1, removed2, removed3, removed4 =
          (ref false, ref false, ref false, ref false)
        in

        Atomic.spawn (fun () ->
            removed1 := try_remove htbl 11;
            removed2 := try_remove htbl 9);
        Atomic.spawn (fun () ->
            removed3 := try_remove htbl 11;
            removed4 := try_remove htbl 10);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                List.for_all (fun x -> x) removed
                && (not !removed2)
                && ((!removed1 && not !removed3)
                   || ((not !removed1) && !removed3))
                && !removed4
                && List.init 12 (fun i -> not (mem htbl i))
                   |> List.for_all (fun x -> x))))

  let _two_add_remove_same () =
    let seed = Random.int 1000 in

    Atomic.trace (fun () ->
        Random.init seed;
        let htbl = create ~min_buckets:32 () in
        let added1, added2, removed1, removed2 =
          (ref false, ref false, ref false, ref false)
        in

        Atomic.spawn (fun () ->
            added1 := try_add htbl 1;
            removed1 := try_remove htbl 1);
        Atomic.spawn (fun () ->
            added2 := try_add htbl 1;
            removed2 := try_remove htbl 1);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                (!added1 || !added2) && (!removed1 || !removed2)
                && not (mem htbl 1))))

  let _two_add_remove_alt () =
    let seed = Random.int 1000 in

    Atomic.trace (fun () ->
        Random.init seed;
        let htbl = create ~min_buckets:32 () in
        let added1, added2, removed1, removed2 =
          (ref false, ref false, ref false, ref false)
        in

        Atomic.spawn (fun () ->
            added1 := try_add htbl 1;
            removed1 := try_remove htbl 1);
        Atomic.spawn (fun () ->
            added2 := try_add htbl 2;
            removed2 := try_remove htbl 2);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                !added1 && !added2 && !removed1 && !removed2
                && (not (mem htbl 1))
                && not (mem htbl 2))))

  let _two_add_remove_crossed () =
    let seed = Random.int 1000 in

    Atomic.trace (fun () ->
        Random.init seed;
        let htbl = create ~min_buckets:32 () in
        let added1, added2, removed1, removed2 =
          (ref false, ref false, ref false, ref false)
        in

        Atomic.spawn (fun () ->
            added1 := try_add htbl 1;
            removed2 := try_remove htbl 2);
        Atomic.spawn (fun () ->
            added2 := try_add htbl 2;
            removed1 := try_remove htbl 1);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                let mem1 = mem htbl 1 in
                let mem2 = mem htbl 2 in
                !added1 && !added2 && !removed1 <> mem1 && !removed2 <> mem2)))

  let _two_add_remove_all () =
    let seed = Random.int 1000 in

    Atomic.trace (fun () ->
        Random.init seed;
        let htbl : (int, int) Htbl.t = create ~min_buckets:32 () in
        for i = 0 to 9 do
          try_add htbl i |> ignore
        done;
        let added, removed = (ref false, ref []) in

        Atomic.spawn (fun () -> removed := remove_all htbl |> List.of_seq);
        Atomic.spawn (fun () -> added := try_add htbl 10);

        Atomic.final (fun () ->
            Atomic.check (fun () ->
                List.sort compare !removed = List.init 10 (fun i -> (i, i))
                && not (mem htbl 10)
                || List.sort compare !removed = List.init 9 (fun i -> (i, i))
                   && mem htbl 10)))

  let tests name =
    let open Alcotest in
    [
      ( "basic_" ^ name,
        [
          test_case "2-mem" `Slow _two_mem;
          test_case "2-add" `Slow _two_add;
          test_case "2-add-resize" `Slow _two_add_resize;
          test_case "2-add-resize2" `Slow _two_add_resize2;
          test_case "2-remove" `Slow _two_remove;
          test_case "2-add-remove-same" `Slow _two_add_remove_same;
          test_case "2-add-remove-alt" `Slow _two_add_remove_alt;
          test_case "2-add-remove-crossed" `Slow _two_add_remove_crossed;
          test_case "2-add-remove_all" `Slow _two_add_remove_crossed;
        ] );
    ]
end

let () =
  (* Both safe and unsafe version have the same body code. We randomly pick one for testing. *)
  Random.self_init ();
  let safe = Random.bool () in
  if safe then
    let module Safe = Dscheck_htbl (Htbl) in
    let open Alcotest in
    run "dscheck_htbl" (Safe.tests "safe")
  else
    let module Unsafe = Dscheck_htbl (Htbl_unsafe) in
    let open Alcotest in
    run "dscheck_htbl_unsafe" (Unsafe.tests "unsafe")
