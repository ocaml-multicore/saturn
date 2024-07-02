module Atomic = Dscheck.TracedAtomic
open Linked_set.Make (Atomic) (Size)

let test_underflow_and_overflow () =
  let s = Size.create () in
  assert (Size.get s = 0);
  Size.update_once s (Size.new_once s Size.decr);
  assert (Size.get s = Size.max_value);
  Size.update_once s (Size.new_once s Size.incr);
  assert (Size.get s = 0)

let two_mem () =
  Atomic.trace @@ fun () ->
  let sl = create () in
  let added1 = ref false in
  let found1 = ref false in
  let found2 = ref false in

  Atomic.spawn (fun () ->
      added1 := try_add sl 1;
      found1 := mem sl 1);

  Atomic.spawn (fun () -> found2 := mem sl 2);

  Atomic.final (fun () ->
      Atomic.check (fun () -> !added1 && !found1 && not !found2))

let two_add () =
  Atomic.trace @@ fun () ->
  let sl = create () in
  let added1 = ref false in
  let added2 = ref false in

  Atomic.spawn (fun () -> added1 := try_add sl 1);
  Atomic.spawn (fun () -> added2 := try_add sl 2);

  Atomic.final (fun () ->
      Atomic.check (fun () -> !added1 && !added2 && mem sl 1 && mem sl 2))

let two_add_same () =
  Atomic.trace @@ fun () ->
  let sl = create () in
  let added1 = ref false in
  let added2 = ref false in

  Atomic.spawn (fun () -> added1 := try_add sl 1);
  Atomic.spawn (fun () -> added2 := try_add sl 1);

  Atomic.final (fun () ->
      Atomic.check (fun () ->
          (!added1 && not !added2) || (((not !added1) && !added2) && mem sl 1)))

let two_remove_same () =
  Atomic.trace @@ fun () ->
  let sl = create () in
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
          && not (mem sl 1)))

let two_remove () =
  Atomic.trace @@ fun () ->
  let sl = create () in
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
          !added1 && !removed1 && (not !removed2) && not found1))

let () =
  Alcotest.run "dscheck_size"
    [
      ( "basic",
        [
          Alcotest.test_case "underflow and overflow" `Quick
            test_underflow_and_overflow;
          Alcotest.test_case "2-mem" `Slow two_mem;
          Alcotest.test_case "2-add-same" `Slow two_add_same;
          Alcotest.test_case "2-add" `Slow two_add;
          Alcotest.test_case "2-remove-same" `Slow two_remove_same;
          Alcotest.test_case "2-remove" `Slow two_remove;
        ] );
    ]
