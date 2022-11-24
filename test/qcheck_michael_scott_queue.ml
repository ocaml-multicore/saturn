module Mpsc_queue = Lockfree.Michael_scott_queue

let extract_n q n =
  let rec loop acc = function
    | 0 -> acc
    | m ->
        let res =
          match Mpsc_queue.pop q with Some elt -> `Some elt | None -> `None
        in
        Domain.cpu_relax ();
        loop (res :: acc) (m - 1)
  in
  if n < 0 then failwith "Number of pop should be positive.";
  loop [] n |> List.rev

let keep_n_first n = List.filteri (fun i _ -> i < n)
let list_some = List.map (fun elt -> `Some elt)

let tests_one_consumer =
  QCheck.
    [
      (* TEST 1 - single consumer no producer:
         forall q and n, pop (push queue i; queue) = Some i *)

      (* TEST 2 - single consumer no producer:
         forall q, if is_empty q then pop queue = None *)
      Test.make ~name:"pop_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push queue elt) (List.rev lpush);
          (* Popping until [is_empty q] is true *)
          let count = ref 0 in
          while not (Mpsc_queue.is_empty queue) do
            incr count;
            ignore (Mpsc_queue.pop queue)
          done;

          (* Testing property *)
          Mpsc_queue.pop queue = None && !count = List.length lpush);
      (* TEST 3 - single consumer no producer:
         forall q and i,  push q i; is_empty q = false *)
      Test.make ~name:"push_not_empty" (list int) (fun lpush ->
          assume (lpush <> []);
          (* Building a random queue *)
          let queue = Mpsc_queue.create () in
          List.iter (fun elt -> Mpsc_queue.push queue elt) (List.rev lpush);

          (* Testing property *)
          not (Mpsc_queue.is_empty queue));
      (* (\* TEST 4 - single consumer no producer: *)
      (*    forall q and i,  [close q; push_head q i] raises Closed <=> q is empty. *\) *)
      (* Test.make ~name:"close_push_head" *)
      (*   (pair (list int) int) *)
      (*   (fun (lpush, i) -> *)
      (*     (\* Building a random queue *\) *)
      (*     let queue = Mpsc_queue.create () in *)
      (*     List.iter (fun elt -> Mpsc_queue.push queue elt) (List.rev lpush); *)

      (*     (\* is_empty raises Close if the queue is closed and empty, *)
      (*        so we need to register its value before closing. Next *)
      (*        test checks [is_empty] behaviour on a closed queue. *\) *)
      (*     let is_empty = Mpsc_queue.is_empty queue in *)
      (*     Mpsc_queue.close queue; *)

      (*     (\* Testing property *\) *)
      (*     if is_empty then *)
      (*       try *)
      (*         Mpsc_queue.push queue i; *)
      (*         false *)
      (*       with Mpsc_queue.Closed -> true *)
      (*     else *)
      (*       try *)
      (*         Mpsc_queue.push queue i; *)
      (*         true *)
      (*       with Mpsc_queue.Closed -> false); *)
      (* (\* TEST 5 - single consumer no producer: *)
      (*    This test works also for one producer no consumer. *)
      (*    forall q and i, [close q; is_empty q] raises Closed <=> q is empty *\) *)
      (* Test.make ~name:"close_is_empty" (list int) (fun lpush -> *)
      (*     (\* Building a random queue *\) *)
      (*     let queue = Mpsc_queue.create () in *)
      (*     List.iter (fun elt -> Mpsc_queue.push queue elt) (List.rev lpush); *)

      (*     let is_empty = Mpsc_queue.is_empty queue in *)
      (*     Mpsc_queue.close queue; *)

      (*     (\* Testing property *\) *)
      (*     if is_empty then *)
      (*       try *)
      (*         ignore (Mpsc_queue.is_empty queue); *)
      (*         false *)
      (*       with Mpsc_queue.Closed -> true *)
      (*     else *)
      (*       try Mpsc_queue.is_empty queue = false *)
      (*       with Mpsc_queue.Closed -> false); *)
      (* (\* TEST 6 - single consumer no producer: *)
      (*    forall q and i, [close q; pop q] raises Closed <=> q is empty *\) *)
      (* Test.make ~name:"close_pop" (list int) (fun lpush -> *)
      (*     (\* Building a random queue *\) *)
      (*     let queue = Mpsc_queue.create () in *)
      (*     List.iter (fun elt -> Mpsc_queue.push queue elt) (List.rev lpush); *)

      (*     let is_empty = Mpsc_queue.is_empty queue in *)
      (*     Mpsc_queue.close queue; *)

      (*     (\* Testing property *\) *)
      (*     if is_empty then *)
      (*       try *)
      (*         ignore (Mpsc_queue.pop queue); *)
      (*         false *)
      (*       with Mpsc_queue.Closed -> true *)
      (*     else *)
      (*       try Mpsc_queue.pop queue = Some (List.hd lpush) *)
      (*       with Mpsc_queue.Closed -> false); *)
      (* (\* TEST 7 - single consumer no producer: *)
      (*    More complex test. Maybe redondant with tests 1 to 6. *)
      (*    Sequentially does n [push_head] then m [pops], [close] and may call [pop] again. *)
      (*    Checks : *)
      (*    - that closing the queue does not prevent [pop] *)
      (*    - [pop] order (it's a LIFO) *)
      (*    - [pop] on a [close]d and empty queue raises [Closed] *)
      (* *\) *)
      (* Test.make ~name:"pop_order" *)
      (*   (pair (list int) (pair small_nat small_nat)) *)
      (*   (fun (lpush, (npop, when_close)) -> *)
      (*     (\* Initialisation*\) *)
      (*     let npush = List.length lpush in *)
      (*     let queue = Mpsc_queue.create () in *)

      (*     (\* Sequential [push_head] *\) *)
      (*     List.iter (fun elt -> Mpsc_queue.push queue elt) (List.rev lpush); *)

      (*     (\* Call [pop] [npop] times and [close] after [when_close] pops. *\) *)
      (*     let popped = extract_n queue npop when_close in *)

      (*     let expected = *)
      (*       List.init npop (fun i -> *)
      (*           if npop <= npush then *)
      (*             (\* Closing the queue does not prevent popping *\) *)
      (*             `Some (List.nth lpush i) *)
      (*           else if npush <= npop && npop <= when_close then *)
      (*             if i < npush then `Some (List.nth lpush i) else `None *)
      (*           else if npush <= when_close && when_close <= npop then *)
      (*             if i < npush then `Some (List.nth lpush i) *)
      (*             else if i < when_close then `None *)
      (*             else `Closed *)
      (*           else if when_close <= npush && npush <= npop then *)
      (*             if i < npush then `Some (List.nth lpush i) else `Closed *)
      (*           else failwith "should not happen") *)
      (*     in *)
      (*     expected = popped); *)
      (* (\* TEST 8 - single consumer no producer: *)
      (*    More complex test. Maybe redondant with tests 1 to 6. *)
      (*    Sequentially does n [push_head], followed by m [pop] and n' more [push_head]. *)
      (*    Checks : *)
      (*    - order of [pop] and [push_head] -> LIFO *)
      (* *\) *)
      (* Test.make ~name:"seq_push_pop" *)
      (*   (pair small_nat (pair (list int) (list int))) *)
      (*   (fun (npop, (lpush1, lpush2)) -> *)
      (*     (\* Initialisation*\) *)
      (*     let queue = Mpsc_queue.create () in *)

      (*     (\* Sequential [push_head] *\) *)
      (*     List.iter (fun elt -> Mpsc_queue.push queue elt) lpush1; *)
      (*     (\* Call [pop] [npop] times without closing. *\) *)
      (*     let popped = extract_n queue npop (npop + 1) in *)
      (*     (\* Sequential [push_head] *\) *)
      (*     List.iter (fun elt -> Mpsc_queue.push queue elt) lpush2; *)
      (*     (\* Dequeue and closing *\) *)
      (*     let size_queue = *)
      (*       List.length lpush2 + Int.max 0 (List.length lpush1 - npop) *)
      (*     in *)
      (*     let final = extract_n queue size_queue 0 in *)

      (*     if npop <= List.length lpush1 then *)
      (*       let expected_final = *)
      (*         keep_n_first (List.length lpush1 - npop) lpush1 @ lpush2 *)
      (*         |> list_some *)
      (*       in *)
      (*       let expected_popped = *)
      (*         keep_n_first npop (List.rev lpush1) |> list_some *)
      (*       in *)
      (*       List.rev final = expected_final && popped = expected_popped *)
      (*     else *)
      (*       let expected_popped = *)
      (*         (List.rev lpush1 |> list_some) *)
      (*         @ List.init (npop - List.length lpush1) (fun _ -> `None) *)
      (*       in *)

      (*       List.rev final = list_some lpush2 && popped = expected_popped); *)
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
          let popped = extract_n queue npop in

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
                (* try *)
                List.iter
                  (fun elt ->
                    Mpsc_queue.push queue elt;
                    Domain.cpu_relax ())
                  lpush;
                false (* with Mpsc_queue.Closed -> true *))
          in

          (* Waiting to make sure the producer can start *)
          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;

          (* Consumer pops. *)
          let popped = extract_n queue npop in

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
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Mpsc_queue"
    [
      ("one_consumer", to_alcotest tests_one_consumer);
      ("one_cons_one_prod", to_alcotest tests_one_consumer_one_producer);
    ]
;;

main ()
