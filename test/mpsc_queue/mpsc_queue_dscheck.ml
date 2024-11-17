module Atomic = Dscheck.TracedAtomic
module Queue = Mpsc_queue

let drain queue =
  let rec pop_until_empty acc =
    match Queue.pop_opt queue with
    | None -> acc |> List.rev
    | Some v -> pop_until_empty (v :: acc)
  in
  pop_until_empty []

let push_pop () =
  Atomic.trace (fun () ->
      let queue = Queue.create () in
      let items_total = 4 in

      (* producer *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            Queue.push queue i
          done);

      (* consumer *)
      let popped = ref [] in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            begin
              match Queue.pop_opt queue with
              | None -> ()
              | Some v -> popped := v :: !popped
            end;
            (* Ensure is_empty does not interfere with other functions *)
            Queue.is_empty queue |> ignore
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              let pushed = List.init items_total (fun x -> x + 1) in
              List.sort Int.compare (!popped @ remaining) = pushed)))

let is_empty () =
  Atomic.trace (fun () ->
      let queue = Queue.create () in

      (* producer *)
      Atomic.spawn (fun () -> Queue.push queue 1);

      (* consumer *)
      let res = ref false in
      Atomic.spawn (fun () ->
          match Queue.pop_opt queue with
          | None -> res := true
          | Some _ -> res := Queue.is_empty queue);

      (* checks*)
      Atomic.final (fun () -> Atomic.check (fun () -> !res)))

let push_drop () =
  Atomic.trace (fun () ->
      let queue = Queue.create () in
      let items_total = 4 in

      (* producer *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            Queue.push queue i
          done);

      (* consumer *)
      let dropped = ref 0 in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            match Queue.drop_exn queue with
            | () -> dropped := !dropped + 1
            | exception Queue.Empty -> ()
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              remaining
              = List.init (items_total - !dropped) (fun x -> x + !dropped + 1))))

let push_push () =
  Atomic.trace (fun () ->
      let queue = Queue.create () in
      let items_total = 6 in

      (* two producers *)
      for i = 0 to 1 do
        Atomic.spawn (fun () ->
            for j = 1 to items_total / 2 do
              (* even nums belong to thr 1, odd nums to thr 2 *)
              Queue.push queue (i + (j * 2))
            done)
      done;

      (* checks*)
      Atomic.final (fun () ->
          let items = drain queue in

          (* got the same number of items out as in *)
          Atomic.check (fun () -> items_total = List.length items);

          (* they are in fifo order *)
          let odd, even = List.partition (fun v -> v mod 2 == 0) items in

          Atomic.check (fun () -> List.sort Int.compare odd = odd);
          Atomic.check (fun () -> List.sort Int.compare even = even)))

let two_producers_one_consumer () =
  Atomic.trace (fun () ->
      let ninit_push = 3 in
      let queue = Queue.of_list (List.init ninit_push (fun i -> i + 1)) in
      let nproducers = 3 in
      let ntotal = (2 * nproducers) + ninit_push in

      (* producer 1 *)
      Atomic.spawn (fun () ->
          for i = 1 to nproducers do
            Queue.push queue (i + ninit_push)
          done);

      (* producer 2 *)
      Atomic.spawn (fun () ->
          for i = 1 to nproducers do
            Queue.push queue (i + ninit_push + nproducers)
          done);

      (* consumer *)
      let popped = ref [] in
      Atomic.spawn (fun () ->
          for _ = 1 to 5 do
            match Queue.pop_opt queue with
            | None -> ()
            | Some v -> popped := v :: !popped
          done);

      (* checks *)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              let pushed = List.init ntotal (fun i -> i + 1) in
              List.sort Int.compare (!popped @ remaining) = pushed);

          Atomic.check (fun () ->
              let pushed_1 =
                List.filter (fun x -> x <= ninit_push + nproducers) !popped
              in
              let pushed_2 =
                List.filter (fun x -> x > ninit_push + nproducers) !popped
              in

              List.sort Int.compare pushed_1 = List.rev pushed_1
              && List.sort Int.compare pushed_2 = List.rev pushed_2)))

let tests =
  let open Alcotest in
  [
    ( "basic",
      [
        test_case "1-producer-1-consumer" `Slow push_pop;
        test_case "2-domains-is_empty" `Slow is_empty;
        test_case "1-push-1-drop" `Slow push_drop;
        test_case "2-producers" `Slow push_push;
        test_case "2-producers-1-consumer" `Slow two_producers_one_consumer;
      ] );
  ]

let () =
  let open Alcotest in
  run "dscheck_bounded_queue" tests
