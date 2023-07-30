module Lazy_skiplist = Lockbased.Lazy_skiplist.Make (struct include Int let hash t = t end)

let uniq ~compare l = List.length (List.sort_uniq compare l) = List.length l

let tests_sequential =
  QCheck.
    [
      (* TEST 1: add *)
      Test.make ~name:"add" (list int) (fun ladd ->
          assume (uniq ~compare:Int.compare ladd);
          assume (ladd <> []);
          (* Building a random skiplist *)
          let lst = Lazy_skiplist.create () in
          List.iter (fun i -> assert (Lazy_skiplist.add lst i)) ladd;

          (* Testing property *)
          List.for_all (Lazy_skiplist.contains lst) ladd
        );
      (* TEST 2 - push, pop until empty *)
      Test.make ~name:"add_remove_until_empty" (list int) (fun ladd ->
          (* Building a random skiplist *)
          let lst = Lazy_skiplist.create () in
          List.iter (fun i -> assert (Lazy_skiplist.add lst i)) ladd;

          (* Remove until [is_empty q] is true *)
          List.iter (fun i -> assert (Lazy_skiplist.remove lst i)) ladd;

          (* Testing property *)
          List.for_all (fun i -> not (Lazy_skiplist.contains lst i)) ladd
        )
    ]

let tests_one_consumer_one_producer =
  QCheck.
    [
      (* TEST 1 - one consumer one producer:
         Parallel [add] and [remove]. *)
      Test.make ~count:10_000 ~name:"parallel_add_remove" (list int) (fun ladd ->
          assume (uniq ~compare:Int.compare ladd);
          (* Initialization *)
          let lst = Lazy_skiplist.create () in

          (* Producer adds. *)
          let producer =
            Domain.spawn (fun () ->
                List.iter (fun i -> assert (Lazy_skiplist.add lst i)) ladd)
          in

          let correct =
            List.fold_left
              (fun acc item ->
                let found = ref false in
                while not !found do
                  found := Lazy_skiplist.remove lst item
                done;
                acc && !found)
              true ladd
          in

          (* Ensure nothing is left behind. *)
          Domain.join producer;
          correct);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Lazy Skiplist"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("one_cons_one_prod", to_alcotest tests_one_consumer_one_producer);
    ]
;;

main ()
