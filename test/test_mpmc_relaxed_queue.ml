open Lockfree

let smoke_test (push, pop) () =
  let queue = Mpmc_relaxed_queue.create ~size_exponent:2 () in
  (* enqueue 4 *)
  for i = 1 to 4 do
    Alcotest.(check bool)
      "there should be space in the queue" (push queue i) true
  done;
  assert (not (push queue 0));
  let ({ tail; head; _ } : 'a Mpmc_relaxed_queue.t) = queue in
  assert (Atomic.get tail = 4);
  assert (Atomic.get head = 0);
  (* dequeue 4 *)
  for i = 1 to 4 do
    Alcotest.(check (option int))
      "items should come out in FIFO order" (Some i) (pop queue)
  done;
  Alcotest.(check (option int)) "queue should be empty" None (pop queue)

let two_threads_test (push, pop) () =
  let queue = Mpmc_relaxed_queue.create ~size_exponent:2 () in
  let num_of_elements = 1_000_000 in
  (* start dequeuer *)
  let dequeuer =
    Domain.spawn (fun () ->
        let i = ref 0 in
        while !i < num_of_elements do
          match pop queue with
          | Some item ->
              Alcotest.(check int)
                "popped items should follow FIFO order" item !i;
              i := !i + 1
          | None -> ()
        done)
  in
  (* enqueue *)
  let i = ref 0 in
  while !i < num_of_elements do
    if push queue !i then i := !i + 1
  done;
  Domain.join dequeuer |> ignore;
  ()

module Wait_for_others = struct
  type t = { currently : int Atomic.t; total_expected : int }

  let init ~total_expected = { currently = Atomic.make 0; total_expected }

  let wait { currently; total_expected } =
    Atomic.incr currently;
    while Atomic.get currently < total_expected do
      ()
    done
end

let taker wfo queue num_of_elements () =
  Wait_for_others.wait wfo;
  let i = ref 0 in
  while !i < num_of_elements do
    if Option.is_some (Mpmc_relaxed_queue.Not_lockfree.pop queue) then
      i := !i + 1
  done

let pusher wfo queue num_of_elements () =
  Wait_for_others.wait wfo;
  let i = ref 0 in
  while !i < num_of_elements do
    if Mpmc_relaxed_queue.Not_lockfree.push queue !i then i := !i + 1
  done

let run_test num_takers num_pushers () =
  let queue = Mpmc_relaxed_queue.create ~size_exponent:3 () in
  let num_of_elements = 4_000_000 in
  let wfo = Wait_for_others.init ~total_expected:(num_takers + num_pushers) in
  let _ =
    let takers =
      assert (num_of_elements mod num_takers == 0);
      let items_per_taker = num_of_elements / num_takers in
      List.init num_takers (fun _ ->
          Domain.spawn (taker wfo queue items_per_taker))
    in
    let pushers =
      assert (num_of_elements mod num_pushers == 0);
      let items_per_pusher = num_of_elements / num_pushers in
      List.init num_pushers (fun _ ->
          Domain.spawn (pusher wfo queue items_per_pusher))
    in
    Sys.opaque_identity (List.map Domain.join (pushers @ takers))
  in
  let ({ array; head; tail; _ } : 'a Mpmc_relaxed_queue.t) = queue in
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  Alcotest.(check int) "hd an tl match" head_val tail_val;
  Array.iter
    (fun item ->
      Alcotest.(check (option int))
        "ghost item in the queue!" None (Atomic.get item))
    array

let smoke_test_spinning () =
  let queue = Mpmc_relaxed_queue.create ~size_exponent:2 () in
  (* enqueue 4 *)
  for i = 1 to 4 do
    Mpmc_relaxed_queue.Spin.push queue i
  done;
  assert (not (Mpmc_relaxed_queue.Not_lockfree.push queue 0));
  let ({ tail; head; _ } : 'a Mpmc_relaxed_queue.t) = queue in
  assert (Atomic.get tail = 4);
  assert (Atomic.get head = 0);
  (* dequeue 4 *)
  for i = 1 to 4 do
    Alcotest.(check (option int))
      "items should come out in FIFO order" (Some i)
      (Mpmc_relaxed_queue.Not_lockfree.pop queue)
  done;
  Alcotest.(check (option int))
    "queue should be empty" None
    (Mpmc_relaxed_queue.Not_lockfree.pop queue)

let two_threads_spin_test () =
  let queue = Mpmc_relaxed_queue.create ~size_exponent:2 () in
  let num_of_elements = 1_000_000 in
  (* start dequeuer *)
  let dequeuer =
    Domain.spawn (fun () ->
        for i = 1 to num_of_elements do
          assert (Mpmc_relaxed_queue.Spin.pop queue == i)
        done)
  in
  (* enqueue *)
  for i = 1 to num_of_elements do
    Mpmc_relaxed_queue.Spin.push queue i
  done;
  Domain.join dequeuer |> ignore;
  ()

let () =
  let open Alcotest in
  run "Mpmc_queue"
    (let open Mpmc_relaxed_queue.Not_lockfree in
    [
      ( "single-thread",
        [ test_case "is it a queue" `Quick (smoke_test (push, pop)) ] );
      ( "validate items",
        [ test_case "1 prod. 1 cons." `Quick (two_threads_test (push, pop)) ] );
      ( "validate indices under load",
        [
          test_case " 4 prod. 4 cons." `Slow (run_test 4 4);
          test_case " 8 prod. 1 cons." `Slow (run_test 8 1);
          test_case " 1 prod. 8 cons." `Slow (run_test 1 8);
        ] );
    ]
    @
    let open Mpmc_relaxed_queue.Not_lockfree.CAS_interface in
    [
      ( "single-thread-CAS-intf",
        [ test_case "is it a queue" `Quick (smoke_test (push, pop)) ] );
      ( "validate items-CAS-intf",
        [ test_case "1 prod. 1 cons." `Quick (two_threads_test (push, pop)) ] );
    ]
    @ [
        ( "single-thread-spinning",
          [ test_case "is it a queue" `Quick smoke_test_spinning ] );
        ( "validate-items-spinning",
          [ test_case "1 prod. 1 cons" `Quick two_threads_spin_test ] );
      ])
