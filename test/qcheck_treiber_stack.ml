open Lockfree

let tests_sequential =
  QCheck.
    [
      (* TEST 1: push *)
      Test.make ~name:"push" (list int) (fun lpush ->
          assume (lpush <> []);
          (* Building a random stack *)
          let stack = Treiber_stack.create () in
          List.iter (Treiber_stack.push stack) lpush;

          (* Testing property *)
          not (Treiber_stack.is_empty stack));
      (* TEST 2 - push, pop until empty *)
      Test.make ~name:"push_pop_until_empty" (list int) (fun lpush ->
          (* Building a random stack *)
          let stack = Treiber_stack.create () in
          List.iter (Treiber_stack.push stack) lpush;

          (* Popping until [is_empty q] is true *)
          let count = ref 0 in
          while not (Treiber_stack.is_empty stack) do
            incr count;
            ignore (Treiber_stack.pop stack)
          done;

          (* Testing property *)
          Treiber_stack.pop stack = None && !count = List.length lpush);
      (* TEST 3 - push, pop, check LIFO  *)
      Test.make ~name:"lifo" (list int) (fun lpush ->
          (* Building a random stack *)
          let stack = Treiber_stack.create () in
          List.iter (Treiber_stack.push stack) lpush;

          let out = ref [] in
          let insert v = out := v :: !out in

          for _ = 1 to List.length lpush do
            match Treiber_stack.pop stack with
            | None -> assert false
            | Some v -> insert v
          done;

          Printf.printf "------\n";
          Printf.printf "lpush: %s\n"
            (List.map Int.to_string lpush |> String.concat ",");
          Printf.printf "out: %s\n"
            (List.map Int.to_string !out |> String.concat ",");

          (* Testing property *)
          lpush = !out);
    ]

let tests_one_consumer_one_producer =
  QCheck.
    [
      (* TEST 1 - one consumer one producer:
         Parallel [push] and [pop]. *)
      Test.make ~count:10_000 ~name:"parallel" (list int) (fun lpush ->
          (* Initialization *)
          let stack = Treiber_stack.create () in
          let sema = Semaphore.Binary.make false in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.iter (Treiber_stack.push stack) lpush)
          in

          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;
          for _ = 1 to List.length lpush do
            while Option.is_none (Treiber_stack.pop stack) do
              ()
            done
          done;

          (* Ensure nothing is left behind. *)
          Domain.join producer;
          Treiber_stack.is_empty stack);
    ]

let tests_two_domains =
  QCheck.
    [
      (* TEST 1 - two domains doing multiple times one push then one pop.
         Parallel [push] and [pop].
      *)
      Test.make ~count:10_000 ~name:"parallel_pop_push"
        (pair small_nat small_nat) (fun (npush1, npush2) ->
          (* Initialization *)
          let stack = Treiber_stack.create () in
          let sema = Semaphore.Binary.make false in

          let lpush1 = List.init npush1 (fun i -> i) in
          let lpush2 = List.init npush2 (fun i -> i + npush1) in

          let work lpush =
            List.map
              (fun elt ->
                Treiber_stack.push stack elt;
                Domain.cpu_relax ();
                Treiber_stack.pop stack)
              lpush
          in

          let domain1 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                work lpush1)
          in
          let popped2 =
            while not (Semaphore.Binary.try_acquire sema) do
              Domain.cpu_relax ()
            done;
            work lpush2
          in

          (* As a domain always pushs before popping, all pops
             succeeds. *)
          let popped1 = Domain.join domain1 |> List.map Option.get in
          let popped2 = List.map Option.get popped2 in

          (* Check 1 : no elements are missing (everyting is popped). *)
          let all_elt_in =
            List.sort compare (popped1 @ popped2) = lpush1 @ lpush2
          in

          all_elt_in
          && List.length popped1 = List.length lpush1
          && List.length popped2 = List.length lpush2);
      (* TEST 2 -
           Parallel [push] and [pop] with two domains

           Two domains randomly pushs and pops in parallel. They stop as
           soon as they have finished pushing a list of element to
           push. *)
      Test.make ~count:10_000 ~name:"parallel_pop_push_random"
        (pair small_nat small_nat) (fun (npush1, npush2) ->
          (* Initialization *)
          let stack = Treiber_stack.create () in

          let lpush1 = List.init npush1 (fun i -> i) in
          let lpush2 = List.init npush2 (fun i -> i + npush1) in
          let sema = Semaphore.Binary.make false in

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
                    Treiber_stack.push stack elt;
                    loop xs popped)
              else (
                incr consecutive_pop;
                let p = Treiber_stack.pop stack in
                loop lpush (p :: popped))
            in
            loop lpush []
          in

          let domain1 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                work lpush1)
          in
          let popped2 =
            while not (Semaphore.Binary.try_acquire sema) do
              Domain.cpu_relax ()
            done;
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

          (* Pop everything that is still on the queue *)
          let popped3 =
            let rec loop popped =
              match Treiber_stack.pop stack with
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

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Treiber_stack"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("one_cons_one_prod", to_alcotest tests_one_consumer_one_producer);
      ("two_domains", to_alcotest tests_two_domains);
    ]
;;

main ()
