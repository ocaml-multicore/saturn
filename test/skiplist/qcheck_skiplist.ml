module Skiplist = struct
  include Saturn.Skiplist

  let try_add s k = try_add s k ()
end

module IntSet = Set.Make (Int)

let[@tail_mod_cons] rec uniq ?(seen = IntSet.empty) = function
  | [] -> []
  | x :: xs ->
      if IntSet.mem x seen then uniq ~seen xs
      else x :: uniq ~seen:(IntSet.add x seen) xs

let tests_sequential =
  QCheck.
    [
      (* TEST 1: add*)
      Test.make ~name:"add" (list int) (fun lpush ->
          let sl = Skiplist.create ~compare:Int.compare () in
          let rec add_all_elems seen l =
            match l with
            | h :: t ->
                if Skiplist.try_add sl h <> IntSet.mem h seen then
                  add_all_elems (IntSet.add h seen) t
                else false
            | [] -> true
          in
          add_all_elems IntSet.empty lpush);
      (*TEST 2: add_remove*)
      Test.make ~name:"add_remove" (list int) (fun lpush ->
          let lpush = uniq lpush in
          let sl = Skiplist.create ~compare:Int.compare () in
          List.iter (fun key -> ignore (Skiplist.try_add sl key)) lpush;
          let rec remove_all_elems l =
            match l with
            | h :: t ->
                if Skiplist.try_remove sl h then remove_all_elems t else false
            | [] -> true
          in
          remove_all_elems lpush);
      (*TEST 3: add_find*)
      Test.make ~name:"add_find" (list int) (fun lpush ->
          let lpush = uniq lpush in
          let lpush = Array.of_list lpush in
          let sl = Skiplist.create ~compare:Int.compare () in
          let len = Array.length lpush in
          let pos = Array.sub lpush 0 (len / 2) in
          let neg = Array.sub lpush (len / 2) (len / 2) in
          Array.iter (fun key -> ignore @@ Skiplist.try_add sl key) pos;
          let rec check_pos index =
            if index < len / 2 then
              if Skiplist.mem sl pos.(index) then check_pos (index + 1)
              else false
            else true
          in
          let rec check_neg index =
            if index < len / 2 then
              if not @@ Skiplist.mem sl neg.(index) then check_neg (index + 1)
              else false
            else true
          in
          check_pos 0 && check_neg 0);
      (* TEST 4: add_remove_find *)
      Test.make ~name:"add_remove_find" (list int) (fun lpush ->
          let lpush = uniq lpush in
          let sl = Skiplist.create ~compare:Int.compare () in
          List.iter (fun key -> ignore @@ Skiplist.try_add sl key) lpush;
          List.iter (fun key -> ignore @@ Skiplist.try_remove sl key) lpush;
          let rec not_find_all_elems l =
            match l with
            | h :: t ->
                if not @@ Skiplist.mem sl h then not_find_all_elems t else false
            | [] -> true
          in

          not_find_all_elems lpush);
    ]

let tests_two_domains =
  QCheck.
    [
      (* TEST 1: Two domains doing multiple adds *)
      Test.make ~name:"parallel_add" (pair small_nat small_nat)
        (fun (npush1, npush2) ->
          let sl = Skiplist.create ~compare:Int.compare () in
          let barrier = Barrier.create 2 in
          let lpush1 = List.init npush1 (fun i -> i) in
          let lpush2 = List.init npush2 (fun i -> i + npush1) in
          let work lpush = List.map (Skiplist.try_add sl) lpush in

          let domain1 =
            Domain.spawn @@ fun () ->
            Barrier.await barrier;
            work lpush1
          in
          let popped2 =
            Barrier.await barrier;
            work lpush2
          in
          let popped1 = Domain.join domain1 in

          let rec compare_all_true l =
            match l with
            | true :: t -> compare_all_true t
            | false :: _ -> false
            | [] -> true
          in
          compare_all_true popped1 && compare_all_true popped2);
      (* TEST 2: Two domains doing multiple one push and one pop in parallel *)
      Test.make ~count:10000 ~name:"parallel_add_remove"
        (pair small_nat small_nat) (fun (npush1, npush2) ->
          let sl = Skiplist.create ~compare:Int.compare () in
          let barrier = Barrier.create 2 in

          let lpush1 = List.init npush1 (fun i -> i) in
          let lpush2 = List.init npush2 (fun i -> i + npush1) in

          let work lpush =
            List.iter
              (fun elt ->
                assert (Skiplist.try_add sl elt);
                assert (Skiplist.try_remove sl elt))
              lpush
          in

          let domain1 =
            Domain.spawn @@ fun () ->
            Barrier.await barrier;
            work lpush1
          in
          let () =
            Barrier.await barrier;
            work lpush2
          in
          let () = Domain.join domain1 in

          let rec check_none_present l =
            match l with
            | h :: t ->
                if Skiplist.mem sl h then false else check_none_present t
            | [] -> true
          in
          check_none_present lpush1 && check_none_present lpush2);
      (* TEST 3: Parallel push and pop using the same elements in two domains *)
      Test.make ~name:"parallel_add_remove_same_list" (list int) (fun lpush ->
          let sl = Skiplist.create ~compare:Int.compare () in
          let barrier = Barrier.create 2 in
          let add_all_elems l =
            List.iter (fun elt -> Skiplist.try_add sl elt |> ignore) l
          in
          let remove_all_elems l =
            List.iter (fun elt -> Skiplist.try_remove sl elt |> ignore) l
          in

          let domain1 =
            Domain.spawn @@ fun () ->
            Barrier.await barrier;
            add_all_elems lpush;
            remove_all_elems lpush
          in
          let () =
            Barrier.await barrier;
            add_all_elems lpush;
            remove_all_elems lpush
          in
          let () = Domain.join domain1 in

          let rec check_none_present l =
            match l with
            | h :: t ->
                if Skiplist.mem sl h then false else check_none_present t
            | [] -> true
          in
          check_none_present lpush);
    ]

let () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "QCheck Skiplist"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("tests_two_domains", to_alcotest tests_two_domains);
    ]
