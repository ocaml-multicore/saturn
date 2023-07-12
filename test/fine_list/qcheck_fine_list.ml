open Saturn

let tests_sequential =
  QCheck.
    [
      (* TEST 1: insert *)
      Test.make ~name:"push" (list int) (fun lpush ->
          assume (lpush <> []);
          (* Building a random list *)
          let llist = Fine_list.create 0 in
          List.iter (fun ele -> ignore @@ Fine_list.add llist ele) lpush;

          (* Testing property *)
          not (Fine_list.is_empty llist));
      (* TEST 2 - insert, remove until empty *)
      Test.make ~name:"push_pop_until_empty" (list int) (fun lpush ->
          (* Building a random list *)
          let llist = Fine_list.create 0 in
          List.iter (fun ele -> ignore @@ Fine_list.add llist ele) lpush;

          (* Removing until [is_empty l] is true *)
          List.iter (fun ele -> ignore @@ Fine_list.remove llist ele) lpush;

          (* Testing property *)
          Fine_list.is_empty llist);
    ]

let tests_two_domains =
  QCheck.
    [
      (* TEST 1: insert double *)
      Test.make ~name:"duplicate add" small_nat (fun len ->
          let lpush1 = List.init len (fun i -> i + 1) in
          let lpush2 = List.rev lpush1 in
          let llist = Fine_list.create 0 in
          let p1 = ref 0 in
          let p2 = ref 0 in
          let producer1 =
            Domain.spawn (fun () ->
                List.iter
                  (fun ele -> if Fine_list.add llist ele then incr p1)
                  lpush1)
          in
          let producer2 =
            Domain.spawn (fun () ->
                List.iter
                  (fun ele -> if Fine_list.add llist ele then incr p2)
                  lpush2)
          in
          Domain.join producer1;
          Domain.join producer2;

          (* Testing property *)
          len = !p1 + !p2);
      (* TEST 2: remove double *)
      Test.make ~name:"duplicate remove" small_nat (fun len ->
          let lpush1 = List.init len (fun i -> i + 1) in
          let lpush2 = List.rev lpush1 in
          let llist = Fine_list.create 0 in
          List.iter (fun ele -> ignore @@ Fine_list.add llist ele) lpush1;
          let c1 = ref 0 in
          let c2 = ref 0 in
          let consumer1 =
            Domain.spawn (fun () ->
                List.iter
                  (fun ele -> if Fine_list.remove llist ele then incr c1)
                  lpush1)
          in
          let consumer2 =
            Domain.spawn (fun () ->
                List.iter
                  (fun ele -> if Fine_list.remove llist ele then incr c2)
                  lpush2)
          in
          Domain.join consumer1;
          Domain.join consumer2;

          (* Testing property *)
          len = !c1 + !c2);
      (* TEST 3: parallel add followed by parallel remove *)
      Test.make ~name:"parallel add remove" small_nat (fun len ->
          let lpush1 = List.init len (fun i -> i + 1) in
          let lpush2 = List.init len (fun i -> len + i + 1) in
          let llist = Fine_list.create 0 in
          let producer1 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> ignore @@ Fine_list.add llist ele) lpush1)
          in
          let producer2 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> ignore @@ Fine_list.add llist ele) lpush2)
          in
          Domain.join producer1;
          Domain.join producer2;

          let consumer1 =
            Domain.spawn (fun () ->
                List.iter
                  (fun ele -> ignore @@ Fine_list.remove llist ele)
                  lpush1)
          in
          let consumer2 =
            Domain.spawn (fun () ->
                List.iter
                  (fun ele -> ignore @@ Fine_list.remove llist ele)
                  lpush2)
          in
          Domain.join consumer1;
          Domain.join consumer2;
          (* Testing property *)
          Fine_list.is_empty llist);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Fine_list"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("two_domains", to_alcotest tests_two_domains);
    ]
;;

main ()
