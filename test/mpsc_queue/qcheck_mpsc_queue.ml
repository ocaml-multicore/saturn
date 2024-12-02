module Mpsc_queue = Saturn.Single_consumer_queue

(* Mpsc_queue is a multiple producers, single consumer queue. *)
(* Producers can use the functions
   - [push]
*)
(* Consumer can use the functions
   - [pop],
   - [push],
   - [push_head],
   - [is_empty],
   - [close] *)

let extract_n q n close =
  let rec loop acc = function
    | 0 -> acc
    | m ->
        if m = n - close then Mpsc_queue.close q;
        let res =
          match Mpsc_queue.pop_opt q with
          | Some elt -> `Some elt
          | None -> `None
          | exception Mpsc_queue.Closed -> `Closed
        in
        Domain.cpu_relax ();
        loop (res :: acc) (m - 1)
  in
  if n < 0 then failwith "Number of pop should be positive.";
  loop [] n |> List.rev

let extract_n_with_peek q n close =
  let rec loop peeked popped = function
    | 0 -> (peeked, popped)
    | m ->
        if m = n - close then Mpsc_queue.close q;
        let peek =
          match Mpsc_queue.peek_opt q with
          | Some elt -> `Some elt
          | None -> `None
          | exception Mpsc_queue.Closed -> `Closed
        in
        let pop =
          match Mpsc_queue.pop_opt q with
          | Some elt -> `Some elt
          | None -> `None
          | exception Mpsc_queue.Closed -> `Closed
        in
        Domain.cpu_relax ();
        loop (peek :: peeked) (pop :: popped) (m - 1)
  in
  if n < 0 then failwith "Number of pop should be positive.";
  let peeked, popped = loop [] [] n in
  (List.rev peeked, List.rev popped)

let keep_n_first n = List.filteri (fun i _ -> i < n)
let keep_n_last n l = List.filteri (fun i _ -> i >= List.length l - n) l
let list_some = List.map (fun elt -> `Some elt)

(* With just one consumer, the [Mpsc_queue] is basically a LIFO. *)
let tests_one_consumer =
  QCheck.
    [
      (* TEST 1 - single consumer no producer:
         forall q and n, pop_opt (push_head q i; q) = Some i*)
      Test.make ~name:"push_head_pop_opt"
        (pair (list int) int)
        (fun (lpush, i) ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Testing property *)
          Mpsc_queue.push_head queue i;
          Mpsc_queue.pop_opt queue = Some i);
      (* TEST 1b - single consumer no producer:
          forall q and n, pop (push_head q i; q) = i*)
      Test.make ~name:"push_head_pop"
        (pair (list int) int)
        (fun (lpush, i) ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Testing property *)
          Mpsc_queue.push_head queue i;
          try Mpsc_queue.pop_exn queue = i with Mpsc_queue.Empty -> false);
      (* TEST 1c - single consumer no producer:
         forall q and n, peek_opt (push_head q i; q) = Some i*)
      Test.make ~name:"push_head_peek_opt"
        (pair (list int) int)
        (fun (lpush, i) ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Testing property *)
          Mpsc_queue.push_head queue i;
          Mpsc_queue.peek_opt queue = Some i);
      (* TEST 1d - single consumer no producer:
         forall q and n, peek (push_head q i; q) = Some i*)
      Test.make ~name:"push_head_peek"
        (pair (list int) int)
        (fun (lpush, i) ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Testing property *)
          Mpsc_queue.push_head queue i;
          try Mpsc_queue.peek_exn queue = i with Mpsc_queue.Empty -> false);
      (* TEST 2 - single consumer no producer:
         forall q, if is_empty q then pop_opt queue = None *)
      Test.make ~name:"pop_opt_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);
          (* Popping until [is_empty q] is true*)
          let count = ref 0 in
          while not (Mpsc_queue.is_empty queue) do
            incr count;
            ignore (Mpsc_queue.pop_opt queue)
          done;

          (* Testing property *)
          Mpsc_queue.pop_opt queue = None && !count = List.length lpush);
      (* TEST 2b - single consumer no producer:
         forall q, if is_empty q then pop queue raises Empty *)
      Test.make ~name:"pop_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);
          (* Popping until [is_empty q] is true*)
          let count = ref 0 in
          while not (Mpsc_queue.is_empty queue) do
            incr count;
            ignore (Mpsc_queue.pop_opt queue)
          done;

          (* Testing property *)
          (try
             ignore (Mpsc_queue.pop_exn queue);
             false
           with Mpsc_queue.Empty -> true)
          && !count = List.length lpush);
      (* TEST 2 - single consumer no producer:
         forall q, if is_empty q then peek_opt queue = None *)
      Test.make ~name:"peek_opt_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);
          (* Popping until [is_empty q] is true*)
          let count = ref 0 in
          while not (Mpsc_queue.is_empty queue) do
            incr count;
            ignore (Mpsc_queue.pop_opt queue)
          done;

          (* Testing property *)
          Mpsc_queue.peek_opt queue = None && !count = List.length lpush);
      (* TEST 2b - single consumer no producer:
         forall q, if is_empty q then peek queue raises Empty *)
      Test.make ~name:"peek_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);
          (* Popping until [is_empty q] is true*)
          let count = ref 0 in
          while not (Mpsc_queue.is_empty queue) do
            incr count;
            ignore (Mpsc_queue.pop_opt queue)
          done;

          (* Testing property *)
          (try
             ignore (Mpsc_queue.peek_exn queue);
             false
           with Mpsc_queue.Empty -> true)
          && !count = List.length lpush);
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
      Test.make ~name:"close_pop_opt" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          let is_empty = Mpsc_queue.is_empty queue in
          Mpsc_queue.close queue;

          (* Testing property *)
          if is_empty then
            try
              ignore (Mpsc_queue.pop_opt queue);
              false
            with Mpsc_queue.Closed -> true
          else
            try Mpsc_queue.pop_opt queue = Some (List.hd lpush)
            with Mpsc_queue.Closed -> false);
      (* TEST 7 - single consumer no producer:
         More complex test. Maybe redondant with tests 1 to 6.
         Sequentially does n [push_head] then m [pop_opt], [close] and may call [pop] again.
         Checks :
         - that closing the queue does not prevent [pop_opt]
         - [pop_opt] order (it's a LIFO)
         - [pop_opt] on a [close]d and empty queue raises [Closed]
      *)
      Test.make ~name:"pop_opt_order"
        (pair (list int) (pair small_nat small_nat))
        (fun (lpush, (npop, when_close)) ->
          (* Initialisation*)
          let npush = List.length lpush in
          let queue = Mpsc_queue.create () in

          (* Sequential [push_head] *)
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) (List.rev lpush);

          (* Call [pop_opt] [npop] times and [close] after [when_close] pops. *)
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
         Sequentially does n [push_head], followed by m [pop_opt] and n' more [push_head].
         Checks :
         - order of [pop_opt] and [push_head] -> LIFO
      *)
      Test.make ~name:"seq_push_pop_opt"
        (pair small_nat (pair (list int) (list int)))
        (fun (npop, (lpush1, lpush2)) ->
          (* Initialisation*)
          let queue = Mpsc_queue.create () in

          (* Sequential [push_head] *)
          List.iter (fun elt -> Mpsc_queue.push_head queue elt) lpush1;
          (* Call [pop_opt] [npop] times without closing. *)
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
         Sequential [push] then several [peek_opt] followed by [pop_opt].
         Checks [peek_opt] and [pop_opt] are in FIFO order. *)
      Test.make ~name:"seq_push_pop_opt_peek_opt"
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
          let peeked, popped = extract_n_with_peek queue npop (npop + 1) in

          (* Testing property *)
          let expected =
            (keep_n_first npop lpush |> list_some)
            @ List.init (Int.max 0 (npop - List.length lpush)) (fun _ -> `None)
          in
          popped = expected && peeked = expected);
      (* TEST 2 - one consumer one producer:
         Parallel [push], [pop_opt], [peek_opt]. *)
      Test.make ~name:"par_push_pop"
        (pair (list int) small_nat)
        (fun (lpush, npop) ->
          (* Initialization *)
          let queue = Mpsc_queue.create () in
          let barrier = Barrier.create 2 in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () ->
                Barrier.await barrier;
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
          Barrier.await barrier;

          (* Consumer pops. *)
          let peeked, popped = extract_n_with_peek queue npop (npop + 1) in
          let closed = Domain.join producer in
          let popped_value =
            List.filter (function `Some _ -> true | _ -> false) popped
          in

          let rec check pushed peeked popped =
            match (pushed, peeked, popped) with
            | _, [], [] -> true
            | _, `None :: peeked, `None :: popped -> check pushed peeked popped
            | push :: pushed, `None :: peeked, `Some pop :: popped
              when pop = push ->
                check pushed peeked popped
            | push :: pushed, `Some peek :: peeked, `Some pop :: popped
              when pop = push && pop = peek ->
                check pushed peeked popped
            | _, _, _ -> false
          in

          (* Testing property *)
          (not closed)
          && List.length popped = npop
          && popped_value
             = (keep_n_first (List.length popped_value) lpush |> list_some)
          && (List.for_all (function
               | `Some _ | `None -> true
               | `Closed -> false))
               popped
          && check lpush peeked popped);
      (* TEST 3 - one consumer one producer:
         Parallel [push] and [push_head]. *)
      Test.make ~name:"par_push_push_head"
        (pair (list int) (list int))
        (fun (lpush, lpush_head) ->
          (* Initialization *)
          let queue = Mpsc_queue.create () in
          let barrier = Barrier.create 2 in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () ->
                Barrier.await barrier;
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
          Barrier.await barrier;

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
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Mpsc_queue"
    [
      ("one_consumer", to_alcotest tests_one_consumer);
      ("one_producer", to_alcotest tests_one_producer);
      ("one_cons_one_prod", to_alcotest tests_one_consumer_one_producer);
    ]
;;

main ()
