open Skiplist

module Atomic = Dscheck.TracedAtomic
(** This is needed in this order as the skiplist.ml file contains
    {[
      module Atomic = Multicore_magic.Transparent_atomic
    ]}
    which is in multicore-magic-dscheck library only a subset of
    [Dscheck.TracedAtomic] function. *)

let test_max_height_of () =
  let s = create ~max_height:1 ~compare () in
  assert (max_height_of s = 1);
  let s = create ~max_height:10 ~compare () in
  assert (max_height_of s = 10);
  let s = create ~max_height:30 ~compare () in
  assert (max_height_of s = 30)

let try_add s k = try_add s k ()

let _two_mem () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 ~compare:Int.compare () in
      let added1 = ref false in
      let found1 = ref false in
      let found2 = ref false in

      Atomic.spawn (fun () ->
          added1 := try_add sl 1;
          found1 := mem sl 1);

      Atomic.spawn (fun () -> found2 := mem sl 2);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !added1 && !found1 && not !found2)))

let _two_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:3 ~compare:Int.compare () in
      let added1 = ref false in
      let added2 = ref false in

      Atomic.spawn (fun () -> added1 := try_add sl 1);
      Atomic.spawn (fun () -> added2 := try_add sl 2);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !added1 && !added2 && mem sl 1 && mem sl 2)))

let _two_add_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:3 ~compare:Int.compare () in
      let added1 = ref false in
      let added2 = ref false in

      Atomic.spawn (fun () -> added1 := try_add sl 1);
      Atomic.spawn (fun () -> added2 := try_add sl 1);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              (!added1 && not !added2)
              || (((not !added1) && !added2) && mem sl 1))))

let _two_remove_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 ~compare:Int.compare () in
      let added1 = ref false in
      let removed1 = ref false in
      let removed2 = ref false in

      Atomic.spawn (fun () ->
          added1 := try_add sl 1;
          removed1 := try_remove sl 1);
      Atomic.spawn (fun () -> removed2 := try_remove sl 1);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              !added1
              && ((!removed1 && not !removed2) || ((not !removed1) && !removed2))
              && not (mem sl 1))))

let _two_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let sl = create ~max_height:2 ~compare:Int.compare () in
      let added1 = ref false in
      let removed1 = ref false in
      let removed2 = ref false in

      Atomic.spawn (fun () ->
          added1 := try_add sl 1;
          removed1 := try_remove sl 1);
      Atomic.spawn (fun () -> removed2 := try_remove sl 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let found1 = mem sl 1 in
              !added1 && !removed1 && (not !removed2) && not found1)))

let () =
  let open Alcotest in
  run "DSCheck_Skiplist"
    [
      ( "basic",
        [
          test_case "max_height_of" `Quick test_max_height_of;
          test_case "2-mem" `Slow _two_mem;
          test_case "2-add-same" `Slow _two_add_same;
          test_case "2-add" `Slow _two_add;
          test_case "2-remove-same" `Slow _two_remove_same;
          test_case "2-remove" `Slow _two_remove;
        ] );
    ]
