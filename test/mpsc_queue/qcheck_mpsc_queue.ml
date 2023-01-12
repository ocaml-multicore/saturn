module Mpsc_queue = Lockfree.Mpsc_queue

(* Mpsc_queue is a multiple producers, single consumer queue. *)
(* Producers can use the functions
   - [push],
   - [is_empty],
   - [close] *)
(* Consumer can use the functions
   - [pop],
   - [push_head],
   - [is_empty],
   - [close] *)

let extract_n q n close =
  let rec loop acc = function
    | 0 -> acc
    | m ->
        if m = n - close then Mpsc_queue.close q;
        let res =
          match Mpsc_queue.pop q with
          | Some elt -> `Some elt
          | None -> `None
          | exception Mpsc_queue.Closed -> `Closed
        in
        Domain.cpu_relax ();
        loop (res :: acc) (m - 1)
  in
  if n < 0 then failwith "Number of pop should be positive.";
  loop [] n |> List.rev

let popped_until_empty_and_closed q =
  let rec loop acc =
    try
      let popped = Mpsc_queue.pop q in
      Domain.cpu_relax ();
      loop (popped :: acc)
    with Mpsc_queue.Closed -> acc
  in
  loop [] |> List.rev

let keep_n_first n = List.filteri (fun i _ -> i < n)
let keep_n_last n l = List.filteri (fun i _ -> i >= List.length l - n) l
let list_some = List.map (fun elt -> `Some elt)

(* With just one consumer, the [Mpsc_queue] is basically a LIFO. *)
let tests_one_consumer =
  QCheck.
    [
      (* TEST 1 - single consumer no producer:
         forall q and n, pop (push queue i; queue) = Some i*)
      Test.make ~name:"push_head_pop"
        (pair (list int) int)
        (fun (lpush, i) ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Testing property *)
          Mpsc_queue.push_head queue i;
          Mpsc_queue.pop queue = Some i);
      (* TEST 2 - single consumer no producer:
         forall q, if is_empty q then pop queue = None *)
      Test.make ~name:"pop_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);
          (* Popping until [is_empty q] is true*)
          let count = ref 0 in
          while not (Mpsc_queue.is_empty queue) do
            incr count;
            ignore (Mpsc_queue.pop queue)
          done;

          (* Testing property *)
          Mpsc_queue.pop queue = None && !count = List.length lpush);
      (* TEST 3 - single consumer no producer:
         forall q and i,  push_head q i; is_empty q = false*)
      Test.make ~name:"push_head_not_empty" (list int) (fun lpush ->
          assume (lpush <> []);
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Testing property *)
          not (Mpsc_queue.is_empty queue));
      (* TEST 4 - single consumer no producer:
         forall q and i,  [close q; push_head q i] raises Closed <=> q is empty. *)
      Test.make ~name:"close_push_head"
        (pair (list int) int)
        (fun (lpush, i) ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* is_empty raises Close if the queue is closed and empty,
             so we need to register its value before closing. Next
             test checks [is_empty] behaviour on a closed queue. *)
          let is_empty = Mpsc_queue.is_empty queue in
          Mpsc_queue.close queue;

          (* Testing property *)
          if is_empty then
            try
              Mpsc_queue.push_head queue i;
              false
            with Mpsc_queue.Closed -> true
          else
            try
              Mpsc_queue.push_head queue i;
              true
            with Mpsc_queue.Closed -> false);
      (* TEST 5 - single consumer no producer:
         This test works also for one producer no consumer.
         forall q and i, [close q; is_empty q] raises Closed <=> q is empty *)
      Test.make ~name:"close_is_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          let is_empty = Mpsc_queue.is_empty queue in
          Mpsc_queue.close queue;

          (* Testing property *)
          if is_empty then
            try
              ignore (Mpsc_queue.is_empty queue);
              false
            with Mpsc_queue.Closed -> true
          else
            try Mpsc_queue.is_empty queue = false
            with Mpsc_queue.Closed -> false);
      (* TEST 6 - single consumer no producer:
         forall q and i, [close q; pop q] raises Closed <=> q is empty *)
      Test.make ~name:"close_pop" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          let is_empty = Mpsc_queue.is_empty queue in
          Mpsc_queue.close queue;

          (* Testing property *)
          if is_empty then
            try
              ignore (Mpsc_queue.pop queue);
              false
            with Mpsc_queue.Closed -> true
          else
            try Mpsc_queue.pop queue = Some (List.hd lpush)
            with Mpsc_queue.Closed -> false);
      (* TEST 7 - single consumer no producer:
         More complex test. Maybe redondant with tests 1 to 6.
         Sequentially does n [push_head] then m [pops], [close] and may call [pop] again.
         Checks :
         - that closing the queue does not prevent [pop]
         - [pop] order (it's a LIFO)
         - [pop] on a [close]d and empty queue raises [Closed]
      *)
      Test.make ~name:"pop_order"
        (pair (list int) (pair small_nat small_nat))
        (fun (lpush, (npop, when_close)) ->
          (* Initialisation*)
          let npush = List.length lpush in
          let queue = Mpsc_queue.create () in

          (* Sequential [push_head] *)
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Call [pop] [npop] times and [close] after [when_close] pops. *)
          let popped = extract_n queue npop when_close in

          let expected =
            List.init npop (fun i ->
                if npop <= npush then
                  (* Closing the queue does not prevent popping *)
                  `Some (List.nth lpush i)
                else if npush <= npop && npop <= when_close then
                  if i < npush then `Some (List.nth lpush i) else `None
                else if npush <= when_close && when_close <= npop then
                  if i < npush then `Some (List.nth lpush i)
                  else if i < when_close then `None
                  else `Closed
                else if when_close <= npush && npush <= npop then
                  if i < npush then `Some (List.nth lpush i) else `Closed
                else failwith "should not happen")
          in
          expected = popped);
      (* TEST 8 - single consumer no producer:
         More complex test. Maybe redondant with tests 1 to 6.
         Sequentially does n [push_head], followed by m [pop] and n' more [push_head].
         Checks :
         - order of [pop] and [push_head] -> LIFO
      *)
      Test.make ~name:"seq_push_pop"
        (pair small_nat (pair (list int) (list int)))
        (fun (npop, (lpush1, lpush2)) ->
          (* Initialisation*)
          let queue = Mpsc_queue.create () in

          (* Sequential [push_head] *)
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) lpush1;
          (* Call [pop] [npop] times without closing. *)
          let popped = extract_n queue npop (npop + 1) in
          (* Sequential [push_head] *)
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) lpush2;
          (* Dequeue and closing *)
          let size_queue =
            List.length lpush2 + Int.max 0 (List.length lpush1 - npop)
          in
          let final = extract_n queue size_queue 0 in

          if npop <= List.length lpush1 then
            let expected_final =
              keep_n_first (List.length lpush1 - npop) lpush1 @ lpush2
              |> list_some
            in
            let expected_popped =
              keep_n_first npop (List.rev lpush1) |> list_some
            in
            List.rev final = expected_final && popped = expected_popped
          else
            let expected_popped =
              (List.rev lpush1 |> list_some)
              @ List.init (npop - List.length lpush1) (fun _ -> `None)
            in

            List.rev final = list_some lpush2 && popped = expected_popped);
    ]

(* With just one producer, only the [push], [empty] and [close] functions can be used. *)
let tests_one_producer =
  QCheck.
    [
      (* TEST 1 - single producer no consumer:
         forall l and q built by pushing each element of l,
           is_empty q = true <=>  l = [] *)
      Test.make ~name:"push_not_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push queue elt) lpush;

          (* Testing property *)
          match lpush with
          | [] -> Mpsc_queue.is_empty queue
          | _ -> not (Mpsc_queue.is_empty queue));
      (* TEST 2 - single producer no consumer:
         forall q and i,  [close q; push q i] raises Closed. *)
      Test.make ~name:"closing_prevents_pushing"
        (pair (list int) int)
        (fun (lpush, i) ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push queue elt) lpush;

          Mpsc_queue.close queue;
          (* Testing property *)
          try
            Mpsc_queue.push queue i;
            false
          with Mpsc_queue.Closed -> true);
    ]

let tests_one_consumer_one_producer =
  QCheck.
    [
      (* TEST 1 - one consumer one producer:
         Sequential [push] then several [pop].
         Checks [pop] order. *)
      Test.make ~name:"seq_push_pop"
        (pair (list int) small_nat)
        (fun (lpush, npop) ->
          (* Initialization *)
          let queue = Mpsc_queue.create () in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () ->
                List.iter (fun elt -> Mpsc_queue.push queue elt) lpush)
          in

          (* Sequential test: we wait for the producer to be finished *)
          let () = Domain.join producer in
          let popped = extract_n queue npop (npop + 1) in

          (* Testing property *)
          let expected =
            (keep_n_first npop lpush |> list_some)
            @ List.init (Int.max 0 (npop - List.length lpush)) (fun _ -> `None)
          in
          popped = expected);
      (* TEST 2 - one consumer one producer:
         Parallel [push] and [pop]. *)
      Test.make ~name:"par_push_pop"
        (pair (list int) small_nat)
        (fun (lpush, npop) ->
          (* Initialization *)
          let queue = Mpsc_queue.create () in
          let sema = Semaphore.Binary.make false in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                try
                  List.iter
                    (fun elt ->
                      Mpsc_queue.push queue elt;
                      Domain.cpu_relax ())
                    lpush;
                  false
                with Mpsc_queue.Closed -> true)
          in

          (* Waiting to make sure the producer can start *)
          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;

          (* Consumer pops. *)
          let popped = extract_n queue npop (npop + 1) in

          let closed = Domain.join producer in

          let popped_value =
            List.filter (function `Some _ -> true | _ -> false) popped
          in

          (* Testing property *)
          (not closed)
          && List.length popped = npop
          && popped_value
             = (keep_n_first (List.length popped_value) lpush |> list_some)
          && (List.for_all (function
               | `Some _ | `None -> true
               | `Closed -> false))
               popped);
      (* TEST 3 - one consumer one producer:
         Parallel [push] and [push_head]. *)
      Test.make ~name:"par_push_push_head"
        (pair (list int) (list int))
        (fun (lpush, lpush_head) ->
          (* Initialization *)
          let queue = Mpsc_queue.create () in
          let sema = Semaphore.Binary.make false in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                try
                  List.iter
                    (fun elt ->
                      Mpsc_queue.push queue elt;
                      Domain.cpu_relax ())
                    lpush;
                  false
                with Mpsc_queue.Closed -> true)
          in

          (* Waiting to make sure the producer can start *)
          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;

          List.iter (fun elt -> Mpsc_queue.push_head queue elt) lpush_head;

          let closed = Domain.join producer in

          (* We pop everything to check order.*)
          let total_push = List.length lpush + List.length lpush_head in
          let all_pushed = extract_n queue total_push (total_push + 1) in

          (* Testing property *)
          (not closed) && Mpsc_queue.is_empty queue
          && keep_n_first (List.length lpush_head) all_pushed
             = list_some (lpush_head |> List.rev)
          && keep_n_last (List.length lpush) all_pushed = list_some lpush);
      (* TEST 4 - one consumer one producer

         Consumer push then close while consumer pop until the queue
         is empty and closed. *)
      Test.make ~name:"par_pop_push2" (list int) (fun lpush ->
          (* Initialisation*)
          let queue = Mpsc_queue.create () in
          let sema = Semaphore.Binary.make false in

          (* Sequential [push_head] *)
          let producer =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                let res =
                  try
                    List.iter
                      (fun elt ->
                        Mpsc_queue.push queue elt;
                        Domain.cpu_relax ())
                      lpush;
                    false
                  with Mpsc_queue.Closed -> true
                in
                Mpsc_queue.close queue;
                res)
          in

          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;

          let popped = popped_until_empty_and_closed queue in

          let unexpected_closed = Domain.join producer in

          let popped_value =
            List.filter (function Some _ -> true | _ -> false) popped
          in

          (not unexpected_closed)
          && lpush |> List.map (fun elt -> Some elt) = popped_value);
    ]

let tests_one_consumer_two_producers =
  QCheck.
    [
      (* TEST 1 - one consumer two producers:
         Two producers push at the same time.
         Checks that producers do not erase each other [pushes]. *)
      Test.make ~name:"par_push"
        (pair (list int) (list int))
        (fun (lpush1, lpush2) ->
          (* Initialization *)
          let npush1, npush2 = (List.length lpush1, List.length lpush2) in
          let queue = Mpsc_queue.create () in
          let sema = Semaphore.Counting.make 2 in

          let multi_push lpush =
            Semaphore.Counting.acquire sema;
            while Semaphore.Counting.try_acquire sema do
              Semaphore.Counting.release sema;
              Domain.cpu_relax ()
            done;
            try
              List.iter
                (fun elt ->
                  Mpsc_queue.push queue elt;
                  Domain.cpu_relax ())
                lpush;
              false
            with Mpsc_queue.Closed -> true
          in

          (* Producers pushes. *)
          let producer1 = Domain.spawn (fun () -> multi_push lpush1) in
          let producer2 = Domain.spawn (fun () -> multi_push lpush2) in

          let closed1 = Domain.join producer1 in
          let closed2 = Domain.join producer2 in

          Mpsc_queue.close queue;

          (* Retrieve pushed values. *)
          let popped = popped_until_empty_and_closed queue in

          let popped_value =
            List.fold_left
              (fun acc elt ->
                match elt with Some elt -> elt :: acc | _ -> acc)
              [] popped
            |> List.rev
          in

          let rec compare l l1 l2 =
            match (l, l1, l2) with
            | [], [], [] -> true
            | [], _, _ -> false
            | _, [], _ -> l = l2
            | _, _, [] -> l = l1
            | x :: l', y :: l1', z :: l2' ->
                if x = y && x = z then compare l' l1 l2' || compare l' l1' l2
                else if x = y then compare l' l1' l2
                else if x = z then compare l' l1 l2'
                else false
          in

          (* Testing property :
             - no Close exception raised before the queue being actually closed
             - all pushed values are in the queue
          *)
          (not closed1) && (not closed2)
          && List.length popped_value = npush1 + npush2
          && compare popped_value lpush1 lpush2);
      (* TEST 2 - one consumer two producers:

         Two producers push and close the queue when one has finished
         pushing. At the same time a consumer popes.

         Checks that closing the queue prevent other producers to push
         and that popping at the same time works.
      *)
      Test.make ~name:"par_push_close_pop"
        (pair (list int) (list int))
        (fun (lpush1, lpush2) ->
          (* Initialization *)
          let npush1, npush2 = (List.length lpush1, List.length lpush2) in
          let queue = Mpsc_queue.create () in
          let sema = Semaphore.Counting.make 2 in

          let guard_push lpush =
            Semaphore.Counting.acquire sema;
            while Semaphore.Counting.try_acquire sema do
              Semaphore.Counting.release sema;
              Domain.cpu_relax ()
            done;
            let closed_when_pushing =
              try
                List.iter
                  (fun elt ->
                    Mpsc_queue.push queue elt;
                    Domain.cpu_relax ())
                  lpush;
                false
              with Mpsc_queue.Closed -> true
            in
            ( closed_when_pushing,
              try
                Mpsc_queue.close queue;
                true
              with Mpsc_queue.Closed -> false )
          in

          (* Producers pushes. *)
          let producer1 = Domain.spawn (fun () -> guard_push lpush1) in
          let producer2 = Domain.spawn (fun () -> guard_push lpush2) in

          (* Waiting to make sure the producers have time to
             start. However, as the consumer will [pop] until one of
             the producer closes the queue, it is not a requirement to wait here. *)
          while Semaphore.Counting.try_acquire sema do
            Semaphore.Counting.release sema;
            Domain.cpu_relax ()
          done;

          let popped = popped_until_empty_and_closed queue in

          let closed_when_pushing1, has_closed1 = Domain.join producer1 in
          let closed_when_pushing2, has_closed2 = Domain.join producer2 in

          let popped_value =
            List.fold_left
              (fun acc elt ->
                match elt with Some elt -> elt :: acc | _ -> acc)
              [] popped
            |> List.rev
          in

          let rec compare l l1 l2 =
            match (l, l1, l2) with
            | [], [], [] -> true
            | [], _, _ -> false
            | _, [], _ -> l = l2
            | _, _, [] -> l = l1
            | x :: l', y :: l1', z :: l2' ->
                if x = y && x = z then compare l' l1 l2' || compare l' l1' l2
                else if x = y then compare l' l1' l2
                else if x = z then compare l' l1 l2'
                else false
          in

          (* Testing property :
             - there should be only 4 workings combinaisons for the boolean values
             [closed_when_pushing] and [has_closed] :
               + CASE 1 : if producer 1 closed the queue before producer 2 have finised
             pushing. In this case returned values will be:
             1 -> false, true / 2 -> true, false
               + CASE 2 : if producer 1 closed the queue and producer 2 have finised
             pushing but have not closed the queue.
             1 -> false, true / 2 -> false, false
               + two symetrical cases.
             - in case 1, the closing producer should have pushed everything but not
             the other.
             - in case 2, both queues should have finished pushing. *)
          match
            ( closed_when_pushing1,
              has_closed1,
              closed_when_pushing2,
              has_closed2 )
          with
          | false, true, true, false ->
              (* CASE 1 *)
              let real_npush2 = List.length popped_value - npush1 in
              real_npush2 < npush2
              && compare popped_value lpush1 (keep_n_first real_npush2 lpush2)
          | true, false, false, true ->
              (* CASE 1, sym *)
              let real_npush1 = List.length popped_value - npush2 in
              real_npush1 < npush1
              && compare popped_value (keep_n_first real_npush1 lpush1) lpush2
          | false, true, false, false | false, false, false, true ->
              (* CASE 2*)
              List.length popped_value = npush1 + npush2
              && compare popped_value lpush1 lpush2
          | _, _, _, _ -> false);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Mpsc_queue"
    [
      ("one_consumer", to_alcotest tests_one_consumer);
      ("one_producer", to_alcotest tests_one_producer);
      ("one_cons_one_prod", to_alcotest tests_one_consumer_one_producer);
      ("one_cons_two_prod", to_alcotest tests_one_consumer_two_producers);
    ]
;;

main ()
