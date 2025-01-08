open Priority_queue

module Atomic = Dscheck.TracedAtomic
(** This is needed in this order as the priority_queue.ml file contains
    {[
      module Atomic = Multicore_magic.Transparent_atomic
    ]}
    which is in multicore-magic-dscheck library only a subset of
    [Dscheck.TracedAtomic] function. *)

let _test_max_height_of () =
  let s = create ~max_height:1 ~compare () in
  assert (max_height_of s = 1);
  let s = create ~max_height:10 ~compare () in
  assert (max_height_of s = 10);
  let s = create ~max_height:30 ~compare () in
  assert (max_height_of s = 30)

let _two_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:3 ~compare:Int.compare () in

      Atomic.spawn (fun () -> add pq 1 1);
      Atomic.spawn (fun () -> add pq 2 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let r1 = remove_min_opt pq in
              let r2 = remove_min_opt pq in
              r1 = Some (1, 1) && r2 = Some (2, 2))))

let _two_add_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:3 ~compare:Int.compare () in

      Atomic.spawn (fun () -> add pq 1 1);
      Atomic.spawn (fun () -> add pq 1 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let r1 = remove_min_opt pq in
              let r2 = remove_min_opt pq in
              (r1 = Some (1, 1) && r2 = Some (1, 2))
              || (r2 = Some (1, 1) && r1 = Some (1, 2)))))

let _two_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in

      Atomic.spawn (fun () ->
          add pq 1 1;
          removed1 := remove_min_opt pq);

      Atomic.spawn (fun () -> removed2 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              match (!removed1, !removed2) with
              | None, Some (1, 1) | Some (1, 1), None -> true
              | _ -> false);
          Atomic.check (fun () -> remove_min_opt pq = None)))

let _two_remove2 () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in
      let removed3 = ref None in
      add pq 1 1;

      Atomic.spawn (fun () -> removed1 := remove_min_opt pq);

      Atomic.spawn (fun () ->
          removed2 := remove_min_opt pq;
          removed3 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !removed3 = None);
          Atomic.check (fun () ->
              match (!removed1, !removed2) with
              | None, Some (1, 1) | Some (1, 1), None -> true
              | _ -> false);
          Atomic.check (fun () -> remove_min_opt pq = None)))

let _two_remove_fifo () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in

      Atomic.spawn (fun () ->
          removed1 := remove_min_opt pq;
          removed2 := remove_min_opt pq);
      Atomic.spawn (fun () ->
          add pq 1 1;
          add pq 2 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let r1 = remove_min_opt pq in
              let r2 = remove_min_opt pq in
              match (!removed1, !removed2) with
              | None, None -> r1 = Some (1, 1) && r2 = Some (2, 2)
              | Some (1, 1), None | None, Some (1, 1) ->
                  r1 = Some (2, 2) && r2 = None
              | Some (1, 1), Some (2, 2) -> r1 = None && r2 = None
              | _ -> false)))

let _two_remove_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let removed1 = ref None in

      Atomic.spawn (fun () ->
          add pq 1 1;
          removed1 := remove_min_opt pq);
      Atomic.spawn (fun () -> add pq 1 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              (!removed1 = Some (1, 1) && remove_min_opt pq = Some (1, 2))
              || (!removed1 = Some (1, 2) && remove_min_opt pq = Some (1, 1)))))

let _two_remove_add2 () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let removed1 = ref None in

      Atomic.spawn (fun () ->
          add pq 2 2;
          removed1 := remove_min_opt pq);

      Atomic.spawn (fun () -> add pq 1 1);

      Atomic.final (fun () ->
          Atomic.check (fun () -> length pq = 1);
          Atomic.check (fun () ->
              (!removed1 = Some (1, 1) && remove_min_opt pq = Some (2, 2))
              || (!removed1 = Some (2, 2) && remove_min_opt pq = Some (1, 1)))))

let _two_remove_add3 () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:1 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in
      let removed3 = ref None in

      add pq 2 2;
      add pq 2 10;

      Atomic.spawn (fun () -> removed1 := remove_min_opt pq);
      Atomic.spawn (fun () ->
          add pq 4 10;
          removed2 := remove_min_opt pq;
          removed3 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !removed2 != None && !removed3 != None);
          Atomic.check (fun () ->
              (!removed1 != None && length pq = 0)
              || !removed1 = None
                 && length pq = 1
                 && remove_min_opt pq = Some (4, 10))))

let _two_remove_add4 () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:1 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in
      add pq 2 2;

      Atomic.spawn (fun () ->
          add pq 1 1;
          removed1 := remove_min_opt pq);

      Atomic.spawn (fun () ->
          add pq 0 0;
          removed2 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              length pq = 1
              &&
              match (!removed1, !removed2) with
              | Some (1, 1), Some (0, 0) -> remove_min_opt pq = Some (2, 2)
              | Some (0, 0), Some (1, 1) -> remove_min_opt pq = Some (2, 2)
              | _ -> false)))

(* length is not linearisable, this test is not working. *)
(* let _two_add_remove_length () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:1 ~compare:Int.compare () in
      let removed1 = ref None in
      let len = ref 0 in
      add pq 2 2;

      Atomic.spawn (fun () ->
          add pq 1 1;
          len := length pq);

      Atomic.spawn (fun () -> removed1 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              match (!removed1, remove_min_opt pq) with
              | Some (2, 2), Some (1, 1) -> !len = 1
              | Some (1, 1), Some (2, 2) -> !len = 2 || !len = 1
              | _ -> false))) *)

let () =
  let open Alcotest in
  run "DSCheck_Skiplist"
    [
      ( "basic",
        [
          test_case "max_height_of" `Quick _test_max_height_of;
          test_case "2-add-same" `Slow _two_add_same;
          test_case "2-add" `Slow _two_add;
          test_case "2-remove" `Slow _two_remove;
          test_case "2-remove2" `Slow _two_remove2;
          test_case "2-remove-fifo" `Slow _two_remove_fifo;
          test_case "2-remove-add" `Slow _two_remove_add;
          test_case "2-remove-add2" `Slow _two_remove_add2;
          test_case "2-remove-add3" `Slow _two_remove_add3;
          test_case "2-remove-add4" `Slow _two_remove_add4;
          (* test_case "2-length" `Slow _two_add_remove_length; *)
        ] );
    ]
