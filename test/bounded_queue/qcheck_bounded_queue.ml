module Qcheck_bounded_queue (Bounded_queue : Bounded_queues.Bounded_queue_tests) =
struct
  let tests_sequential =
    QCheck.
      [
        (* TEST 1: push *)
        Test.make ~name:"push" (list int) (fun lpush ->
            assume (lpush <> []);
            (* Building a random Bounded_queue *)
            let queue = Bounded_queue.create () in
            List.iter (Bounded_queue.push_exn queue) lpush;

            (* Testing property *)
            (not (Bounded_queue.is_empty queue))
            && Bounded_queue.length queue = List.length lpush);
        (* TEST 1: of_list *)
        Test.make ~name:"of_list_exn" (list int) (fun lpush ->
            assume (lpush <> []);
            (* Building a random Bounded_queue *)
            let queue = Bounded_queue.of_list_exn lpush in

            (* Testing property *)
            (not (Bounded_queue.is_empty queue))
            && Bounded_queue.length queue = List.length lpush);
        (* TEST 1bis: push *)
        Test.make ~name:"of_list_exn_raise_full"
          (pair (list int) small_nat)
          (fun (lpush, capacity) ->
            assume (lpush <> []);
            (* Building a random Bounded_queue *)
            match Bounded_queue.of_list_exn ~capacity lpush with
            | queue ->
                capacity >= List.length lpush
                && (not (Bounded_queue.is_empty queue))
                && Bounded_queue.length queue = List.length lpush
            | exception Bounded_queue.Full -> capacity <= List.length lpush);
        (* TEST 1: push and full *)
        Test.make ~name:"push_capacity" (list int) (fun lpush ->
            assume (lpush <> []);
            (* Building a random Bounded_queue *)
            let capacity = 10 in
            let queue = Bounded_queue.create ~capacity () in
            List.map
              (fun elt ->
                try
                  Bounded_queue.push_exn queue elt;
                  true
                with Bounded_queue.Full -> false)
              lpush
            |> List.filteri (fun i elt -> if i < capacity then not elt else elt)
            |> ( = ) []);
        (* TEST 2 - push, pop until empty *)
        Test.make ~name:"push_pop_opt_until_empty" (list int) (fun lpush ->
            (* Building a random Bounded_queue *)
            let queue = Bounded_queue.create () in
            List.iter (Bounded_queue.push_exn queue) lpush;

            (* Popping until [is_empty q] is true *)
            let count = ref 0 in
            while not (Bounded_queue.is_empty queue) do
              incr count;
              ignore (Bounded_queue.pop_opt queue)
            done;

            (* Testing property *)
            Bounded_queue.pop_opt queue = None && !count = List.length lpush);
        (* TEST 3 - push, pop_opt, check FIFO  *)
        Test.make ~name:"fifo" (list int) (fun lpush ->
            (* Building a random Bounded_queue *)
            let queue = Bounded_queue.create () in
            List.iter (Bounded_queue.push_exn queue) lpush;

            let out = ref [] in
            let insert v = out := v :: !out in

            for _ = 1 to List.length lpush do
              match Bounded_queue.pop_opt queue with
              | None -> assert false
              | Some v -> insert v
            done;

            (* Testing property *)
            lpush = List.rev !out);
        (* TEST 3 - push, pop_opt, peek_opt check FIFO  *)
        Test.make ~name:"fifo_peek_opt" (list int) (fun lpush ->
            (* Building a random Bounded_queue *)
            let queue = Bounded_queue.create () in
            List.iter (Bounded_queue.push_exn queue) lpush;

            let pop = ref [] in
            let peek = ref [] in
            let insert out v = out := v :: !out in

            for _ = 1 to List.length lpush do
              match Bounded_queue.peek_opt queue with
              | None -> assert false
              | Some v -> (
                  insert peek v;
                  match Bounded_queue.pop_opt queue with
                  | None -> assert false
                  | Some v -> insert pop v)
            done;

            (* Testing property *)
            lpush = List.rev !pop && lpush = List.rev !peek);
      ]

  let tests_one_consumer_one_producer =
    QCheck.
      [
        (* TEST 1 - one consumer one producer:
           Parallel [push] and [pop_opt]. *)
        Test.make ~name:"parallel_fifo" (list int) (fun lpush ->
            (* Initialization *)
            let queue = Bounded_queue.create () in
            let barrier = Barrier.create 2 in

            (* Producer pushes. *)
            let producer =
              Domain.spawn (fun () ->
                  Barrier.await barrier;
                  List.iter (Bounded_queue.push_exn queue) lpush)
            in

            Barrier.await barrier;
            let fifo =
              List.fold_left
                (fun acc item ->
                  let rec pop_one () =
                    match Bounded_queue.pop_opt queue with
                    | None ->
                        Domain.cpu_relax ();
                        pop_one ()
                    | Some item' -> acc && item = item'
                  in
                  pop_one ())
                true lpush
            in
            let empty = Bounded_queue.is_empty queue in

            (* Ensure nothing is left behind. *)
            Domain.join producer;
            fifo && empty);
        (* TEST 2 - one consumer one producer:
           Parallel [push] and [peek_opt] and [pop_opt]. *)
        Test.make ~name:"parallel_peek" (list int) (fun pushed ->
            (* Initialization *)
            let npush = List.length pushed in
            let queue = Bounded_queue.create () in
            let barrier = Barrier.create 2 in

            (* Producer pushes. *)
            let producer =
              Domain.spawn (fun () ->
                  Barrier.await barrier;
                  List.iter (Bounded_queue.push_exn queue) pushed)
            in

            let peeked = ref [] in
            let popped = ref [] in
            Barrier.await barrier;
            for _ = 1 to npush do
              peeked := Bounded_queue.peek_opt queue :: !peeked;
              popped := Bounded_queue.pop_opt queue :: !popped
            done;

            Domain.join producer;
            let rec check = function
              | _, [], [] -> true
              | pushed, None :: peeked, None :: popped ->
                  check (pushed, peeked, popped)
              | push :: pushed, None :: peeked, Some pop :: popped
                when push = pop ->
                  check (pushed, peeked, popped)
              | push :: pushed, Some peek :: peeked, Some pop :: popped
                when push = peek && push = pop ->
                  check (pushed, peeked, popped)
              | _, _, _ -> false
            in
            check (pushed, List.rev @@ !peeked, List.rev @@ !popped));
      ]

  let tests_two_domains =
    QCheck.
      [
        (* TEST 1 - two domains doing multiple times one push then one pop_opt.
           Parallel [push] and [pop_opt].
        *)
        Test.make ~name:"parallel_pop_opt_push" (pair small_nat small_nat)
          (fun (npush1, npush2) ->
            (* Initialization *)
            let queue = Bounded_queue.create () in
            let barrier = Barrier.create 2 in

            (* Using these lists instead of a random one enables to
               check for more properties. *)
            let lpush1 = List.init npush1 (fun i -> i) in
            let lpush2 = List.init npush2 (fun i -> i + npush1) in

            let work lpush =
              List.map
                (fun elt ->
                  Bounded_queue.push_exn queue elt;
                  Domain.cpu_relax ();
                  Bounded_queue.pop_opt queue)
                lpush
            in

            let domain1 =
              Domain.spawn (fun () ->
                  Barrier.await barrier;
                  work lpush1)
            in
            let popped2 =
              Barrier.await barrier;
              work lpush2
            in

            (* As a domain always pushs before popping, all pops
               succeeds. *)
            let popped1 = Domain.join domain1 |> List.map Option.get in
            let popped2 = popped2 |> List.map Option.get in

            (* Check 1 : no elements are missing (everyting is popped). *)
            let all_elt_in =
              List.sort compare (popped1 @ popped2) = lpush1 @ lpush2
            in
            (* filter the elements pushed and popped by domain 1 *)
            let push1_pop1 = List.filter (fun elt -> elt < npush1) popped1 in
            (* filter the elements pushed by domain 2 and popped by domain 1 *)
            let push2_pop1 = List.filter (fun elt -> elt >= npush1) popped1 in
            (* filter the elements pushed by domain 1 and popped by domain 2 *)
            let push1_pop2 = List.filter (fun elt -> elt < npush1) popped2 in
            (* filter the elements pushed and popped by domain 2 *)
            let push2_pop2 = List.filter (fun elt -> elt >= npush1) popped2 in

            (* all these lists must be sorted *)
            let is_sorted list = List.sort compare list = list in
            all_elt_in && is_sorted push1_pop1 && is_sorted push1_pop2
            && is_sorted push2_pop1 && is_sorted push2_pop2);
        (* TEST 2 -
           Parallel [push] and [pop_opt] with two domains

           Two domains randomly pushs and pops in parallel. They stop as
           soon as they have finished pushing a list of element to
           push. *)
        Test.make ~name:"parallel_pop_opt_push_random"
          (pair small_nat small_nat) (fun (npush1, npush2) ->
            (* Initialization *)
            let queue = Bounded_queue.create () in
            let barrier = Barrier.create 2 in

            let lpush1 = List.init npush1 (fun i -> i) in
            let lpush2 = List.init npush2 (fun i -> i + npush1) in

            let work lpush =
              let consecutive_pop = ref 0 in
              let rec loop lpush popped =
                let what_to_do = Random.int 2 in
                if what_to_do = 0 || !consecutive_pop > 10 then (
                  (* randomly choosing between pushing and popping except
                     if too many consecutive pops have already occurred *)
                  consecutive_pop := 0;
                  match lpush with
                  | [] -> popped
                  | elt :: xs ->
                      Bounded_queue.push_exn queue elt;
                      loop xs popped)
                else (
                  incr consecutive_pop;
                  let p = Bounded_queue.pop_opt queue in
                  loop lpush (p :: popped))
              in
              loop lpush []
            in

            let domain1 =
              Domain.spawn (fun () ->
                  Barrier.await barrier;
                  work lpush1)
            in
            let popped2 =
              Barrier.await barrier;
              work lpush2
            in

            let popped1 =
              Domain.join domain1
              |> List.filter (function None -> false | _ -> true)
              |> List.map Option.get
            in
            let popped2 =
              popped2
              |> List.filter (function None -> false | _ -> true)
              |> List.map Option.get
            in

            (* Pop everything that is still on the Bounded_queue *)
            let popped3 =
              let rec loop popped =
                match Bounded_queue.pop_opt queue with
                | None -> popped
                | Some v -> loop (v :: popped)
              in
              loop []
            in
            (* Check that no element is missing. *)
            let all_n_elt_in =
              List.sort compare (popped1 @ popped2 @ popped3) = lpush1 @ lpush2
            in

            all_n_elt_in);
      ]
end

let () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in

  let module Safe = Qcheck_bounded_queue (Bounded_queues.Bounded_queue) in
  let name = "safe" in
  let safe_tests =
    [
      ("test_sequential_" ^ name, to_alcotest Safe.tests_sequential);
      ( "one_cons_one_prod_" ^ name,
        to_alcotest Safe.tests_one_consumer_one_producer );
      ("two_domains_" ^ name, to_alcotest Safe.tests_two_domains);
    ]
  in
  let module Unsafe = Qcheck_bounded_queue (Bounded_queues.Bounded_queue_unsafe) in
  let name = "unsafe" in
  let unsafe_tests =
    [
      ("test_sequential_" ^ name, to_alcotest Unsafe.tests_sequential);
      ( "one_cons_one_prod_" ^ name,
        to_alcotest Unsafe.tests_one_consumer_one_producer );
      ("two_domains_" ^ name, to_alcotest Unsafe.tests_two_domains);
    ]
  in
  Alcotest.run "Bounded_queue" (safe_tests @ unsafe_tests)
