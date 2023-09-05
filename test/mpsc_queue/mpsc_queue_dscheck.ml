let drain queue =
  let remaining = ref 0 in
  while not (Mpsc_queue.is_empty queue) do
    remaining := !remaining + 1;
    assert (Option.is_some (Mpsc_queue.pop_opt queue))
  done;
  !remaining

let producer_consumer () =
  Atomic.trace (fun () ->
      let queue = Mpsc_queue.create () in
      let items_total = 4 in

      (* producer *)
      Atomic.spawn (fun () ->
          for _ = 1 to items_total - 1 do
            Mpsc_queue.push queue 0
          done);

      (* consumer *)
      let popped = ref 0 in
      Atomic.spawn (fun () ->
          Mpsc_queue.push_head queue 1;
          for _ = 1 to items_total do
            match Mpsc_queue.pop_opt queue with
            | None -> ()
            | Some _ -> popped := !popped + 1
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              !popped + remaining = items_total)))

let producer_consumer_peek () =
  Atomic.trace (fun () ->
      let queue = Mpsc_queue.create () in
      let items_total = 1 in
      let pushed = List.init items_total (fun i -> i) in

      (* producer *)
      Atomic.spawn (fun () ->
          List.iter (fun elt -> Mpsc_queue.push queue elt) pushed);

      (* consumer *)
      let popped = ref [] in
      let peeked = ref [] in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            peeked := Mpsc_queue.peek_opt queue :: !peeked;
            popped := Mpsc_queue.pop_opt queue :: !popped
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let rec check pushed peeked popped =
                match (pushed, peeked, popped) with
                | _, [], [] -> true
                | _, None :: peeked, None :: popped ->
                    check pushed peeked popped
                | push :: pushed, None :: peeked, Some pop :: popped
                  when push = pop ->
                    check pushed peeked popped
                | push :: pushed, Some peek :: peeked, Some pop :: popped
                  when push = peek && push = pop ->
                    check pushed peeked popped
                | _, _, _ -> false
              in
              check pushed (List.rev !peeked) (List.rev !popped));
          Atomic.check (fun () ->
              let remaining = drain queue in
              let popped = List.filter Option.is_some !popped in
              List.length popped + remaining = items_total)))

let two_producers () =
  Atomic.trace (fun () ->
      let queue = Mpsc_queue.create () in
      let items_total = 4 in

      (* producers *)
      for _ = 1 to 2 do
        Atomic.spawn (fun () ->
            for _ = 1 to items_total / 2 do
              Mpsc_queue.push queue 0
            done)
      done;

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              remaining = items_total)))

let () =
  let open Alcotest in
  run "mpsc_queue_dscheck"
    [
      ( "basic",
        [
          test_case "1-producer-1-consumer" `Slow producer_consumer;
          test_case "1-producer-1-consumer-peek" `Slow producer_consumer_peek;
          test_case "2-producers" `Slow two_producers;
        ] );
    ]
