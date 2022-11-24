open Lockfree

let tests_sequential =
  QCheck.
    [
      (* TEST 1: push *)
      Test.make ~name:"push" (list int) (fun lpush ->
          assume (lpush <> []);
          (* Building a random queue *)
          let queue = Michael_scott_queue.create () in
          List.iter (Michael_scott_queue.push queue) lpush;

          (* Testing property *)
          not (Michael_scott_queue.is_empty queue));
      (* TEST 2 - push, pop until empty *)
      Test.make ~name:"push_pop_until_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Michael_scott_queue.create () in
          List.iter (Michael_scott_queue.push queue) lpush;

          (* Popping until [is_empty q] is true *)
          let count = ref 0 in
          while not (Michael_scott_queue.is_empty queue) do
            incr count;
            ignore (Michael_scott_queue.pop queue)
          done;

          (* Testing property *)
          Michael_scott_queue.pop queue = None && !count = List.length lpush);
      (* TEST 3 - push, pop, check FIFO  *)
      Test.make ~name:"fifo" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Michael_scott_queue.create () in
          List.iter (Michael_scott_queue.push queue) lpush;

          let out = ref [] in
          let insert v = out := v :: !out in

          for _ = 1 to List.length lpush do
            match Michael_scott_queue.pop queue with
            | None -> assert false
            | Some v -> insert v
          done;

          (* Testing property *)
          lpush = List.rev !out);
    ]

let tests_one_consumer_one_producer =
  QCheck.
    [
      (* TEST 1 - one consumer one producer:
         Parallel [push] and [pop]. *)
      Test.make ~count:10_000 ~name:"parallel_fifo" (list int) (fun lpush ->
          (* Initialization *)
          let queue = Michael_scott_queue.create () in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () ->
                List.iter (Michael_scott_queue.push queue) lpush)
          in

          let fifo =
            List.fold_left
              (fun acc item ->
                let popped = ref None in
                while Option.is_none !popped do
                  popped := Michael_scott_queue.pop queue
                done;
                acc && item = Option.get !popped)
              true lpush
          in
          let empty = Michael_scott_queue.is_empty queue in

          (* Ensure nothing is left behind. *)
          Domain.join producer;
          fifo && empty);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Michael_scott_queue"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("one_cons_one_prod", to_alcotest tests_one_consumer_one_producer);
    ]
;;

main ()
