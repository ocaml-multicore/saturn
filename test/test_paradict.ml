open Lockfree.Paradict.Make (struct
  type t = string

  let equal = ( = )
  let hash s = Hashtbl.hash s
end)

let nb = 10_000
let is_prime _ n = n = 2 (* I swear it's a valid prime-checker *)
let nb_domains = 8

let mem_empty =
  Alcotest.test_case "mem on empty map" `Quick @@ fun () ->
  let scores = create () in
  let res = mem "Among us" scores in
  Alcotest.(check bool) "Unknown entry shouldn't be found" false res;
  ()

let add_mem =
  Alcotest.test_case "add & mem" `Quick @@ fun () ->
  let scores = create () in
  add "Factorio" 10 scores;
  let res = mem "Factorio" scores in
  Alcotest.(check bool) "Added entry should be found" true res;
  ()

let add_find =
  Alcotest.test_case "add & find_opt" `Quick @@ fun () ->
  let scores = create () in
  add "Sekiro" 9 scores;
  let res = find_opt "Sekiro" scores in
  Alcotest.(check (option int))
    "Added entry should have correct value" (Some 9) res;
  ()

let basics = [ mem_empty; add_mem; add_find ]

let iter_test =
  Alcotest.test_case "iter" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to nb do
    add (string_of_int i) i numbers
  done;
  let module EntrySet = Set.Make (struct
    type t = string * int

    let compare = compare
  end) in
  let set = ref EntrySet.empty in
  iter (fun k v -> set := EntrySet.add (k, v) !set) numbers;
  for i = 1 to nb do
    let found = EntrySet.mem (string_of_int i, i) !set in
    Alcotest.(check bool) "Iter should have performed an insert" true found
  done

let mi =
  Alcotest.test_case "map inplace" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to nb do
    add (string_of_int i) i numbers
  done;
  filter_map_inplace (fun _ i -> Some (if i <= nb / 2 then i else -i)) numbers;
  for i = 1 to nb do
    let found = find_opt (string_of_int i) numbers in
    let expected = Some (if i <= nb / 2 then i else -i) in
    Alcotest.(check (option int))
      "FMI should perform in-place modifications" expected found
  done

let fmi =
  Alcotest.test_case "filter map inplace" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to nb do
    add (string_of_int i) i numbers
  done;
  filter_map_inplace
    (fun _ i -> if i <= nb / 2 then Some (2 * i) else None)
    numbers;
  for i = 1 to nb do
    let found = find_opt (string_of_int i) numbers in
    let expected = if i <= nb / 2 then Some (2 * i) else None in
    Alcotest.(check (option int))
      "FMI should perform in-place modifs & removals" expected found
  done

let fmi_with_bad_h =
  Alcotest.test_case "fmi with bad hash function" `Quick @@ fun () ->
  let open Lockfree.Paradict.Make (struct
    type t = string

    let equal = ( = )
    let hash _ = 42
  end) in
  let numbers = create () in
  add "one" 1 numbers;
  add "two" 2 numbers;
  filter_map_inplace (fun _ _ -> None) numbers;
  Alcotest.(check bool) "FMI should properly empty trie" true (is_empty numbers);
  Alcotest.(check int) "FMI should properly set size to 0" 0 (size numbers);
  ()

let fold_test =
  Alcotest.test_case "fold" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to 2 * nb do
    if i mod 2 = 0 then add (string_of_int i) i numbers
  done;
  let called = ref 0 in
  let result =
    fold
      (fun _ v acc ->
        called := !called + 1;
        acc + v)
      numbers 0
  in
  Alcotest.(check int)
    "Fold should call the function the correct amount of times" nb !called;
  let expected = nb * (nb + 1) in
  Alcotest.(check int) "Fold should compute the correct value" expected result;
  ()

let exists_test =
  Alcotest.test_case "exists" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to nb do
    add (string_of_int i) i numbers
  done;
  let result = exists is_prime numbers in
  Alcotest.(check bool) "Exists should find a prime number" true result;
  let is_big _ n = n = nb * 4 in
  let result = exists is_big numbers in
  Alcotest.(check bool) "Exists should not find absent number" false result;
  ()

let for_all_test =
  Alcotest.test_case "for all" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to nb do
    add (string_of_int i) i numbers
  done;
  let is_stringed str n = int_of_string str = n in
  let result = for_all is_stringed numbers in
  Alcotest.(check bool) "For all should validate trivial claim" true result;
  let is_even _ n = n mod 2 = 0 in
  let result = for_all is_even numbers in
  Alcotest.(check bool) "For all should invalidate false claim" false result;
  ()

let iterations =
  [ iter_test; mi; fmi; fmi_with_bad_h; fold_test; exists_test; for_all_test ]

let collision_mem =
  Alcotest.test_case "collision & mem" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to nb do
    add (string_of_int i) i numbers
  done;
  for i = 1 to nb do
    let found = find_opt (string_of_int i) numbers in
    Alcotest.(check (option int))
      (string_of_int i ^ " should be found even in case of collision")
      (Some i) found
  done;
  ()

let size_empty =
  Alcotest.test_case "size after full remove" `Quick @@ fun () ->
  let numbers = create () in
  for i = 1 to nb do
    add (string_of_int i) i numbers
  done;
  for i = 1 to nb do
    remove (string_of_int i) numbers
  done;
  Alcotest.(check bool)
    "After a full removal, root should be empty" true (is_empty numbers);
  Alcotest.(check int) "After a full removal, size should be 0" 0 (size numbers);
  ()

let collisions = [ collision_mem; size_empty ]

let para_add_mem =
  let add_bunch map i =
    for j = 1 to nb do
      add (Format.sprintf "(%d, %d)" i j) (j, i) map
    done;
    let all = ref true in
    for j = 1 to nb do
      all := !all && mem (Format.sprintf "(%d, %d)" i j) map
    done;
    !all
  in
  Alcotest.test_case "adds & mems" `Quick @@ fun () ->
  let scores = create () in
  let domains =
    Array.init nb_domains (fun i -> Domain.spawn (fun () -> add_bunch scores i))
  in
  let all_doms = Array.for_all Domain.join domains in
  Alcotest.(check bool)
    "Parallel adds and mems should all find their respective values" true
    all_doms;
  ()

let para_add_then_mem =
  let add_bunch map i =
    for j = 1 to nb do
      add (Format.sprintf "(%d, %d)" i j) (i, j) map
    done
  in
  Alcotest.test_case "adds then mem" `Quick @@ fun () ->
  let scores = create () in
  let domains =
    Array.init nb_domains (fun i -> Domain.spawn (fun () -> add_bunch scores i))
  in
  Array.iteri
    (fun i d ->
      Domain.join d;
      for j = 1 to nb do
        let res = mem (Format.sprintf "(%d, %d)" i j) scores in
        Alcotest.(check bool)
          "Parallel adds should all be found afterward" true res
      done)
    domains;
  ()

let para_add_contention =
  Alcotest.test_case "adds with contention" `Quick @@ fun () ->
  let scores = create () in
  let domains =
    Array.init nb_domains (fun _ ->
        Domain.spawn (fun () -> add "Celeste" 19 scores))
  in
  Array.iter Domain.join domains;
  let found = find_opt "Celeste" scores in
  match found with
  | None -> Alcotest.fail "Added value should be found even with contention"
  | Some k ->
      Alcotest.(check int) "Added value should be a really possible value" 19 k;
      ()

let parallel = [ para_add_mem; para_add_then_mem; para_add_contention ]

let basic_snap =
  Alcotest.test_case "snapshot" `Quick @@ fun () ->
  let dict = create () in
  add "Hello" "World" dict;
  let dict' = copy dict in
  add "Not in" "snap" dict;
  Alcotest.(check (option string))
    "Snapshot should not eat additions" (Some "World") (find_opt "Hello" dict');
  Alcotest.(check (option string))
    "Snapshot should not eat additions" None (find_opt "Not in" dict');
  ()

let snap_then_ops =
  Alcotest.test_case "snapshot then operations" `Quick @@ fun () ->
  let dict = create () in
  for i = 1 to nb do
    add (string_of_int i) i dict
  done;
  let dict' = copy dict in
  for i = 1 to nb do
    let stri = string_of_int i in
    remove stri dict;
    let found = find_opt stri dict' in
    Alcotest.(check (option int))
      "Added value should still be present in snapshotted version" (Some i)
      found
  done

let map_test =
  Alcotest.test_case "map then operations" `Quick @@ fun () ->
  let names =
    [| "Joe"; "Jack"; "William"; "Averell"; "Lucky Luke"; "Jolly Jumper" |]
  in
  let heights = create () in
  Array.iteri (fun i name -> add name i heights) names;
  let is_nice name _ = name = "Lucky Luke" || name = "Jolly Jumper" in
  let nices = map is_nice heights in
  remove "Jack" heights;
  add "You" true nices;
  Alcotest.(check int)
    "Map should not prevent removals on earlier dict" 5 (size heights);
  let result = for_all (fun name height -> names.(height) = name) heights in
  Alcotest.(check bool) "Map should not modify existing data" true result;

  Alcotest.(check int)
    "Map should not prevent insertions on later dict" 7 (size nices);
  iter
    (fun name niceness ->
      let expected = is_nice name niceness || name = "You" in
      Alcotest.(check bool)
        "Map should apply the function correctly" expected niceness)
    nices;
  ()

let snapshots = [ basic_snap; snap_then_ops; map_test ]

let save_single =
  Alcotest.test_case "save singleton trie" `Quick @@ fun () ->
  let scores = create () in
  add "Subnautica" 3 scores;
  save_as_dot (Fun.id, string_of_int) scores "singleton.dot";
  let gotten_ic = open_in "singleton.dot" in
  let expect_ic = open_in "../../../test/singleton_expected.dot" in
  let eof = ref true in
  while !eof do
    try
      let gotten_line = input_line gotten_ic in
      let expect_line = input_line expect_ic in
      Alcotest.(check string)
        "Lines should match expected file" expect_line gotten_line
    with End_of_file -> eof := false
  done;
  close_in gotten_ic;
  close_in expect_ic

let save_12 =
  Alcotest.test_case "save size 12 trie" `Quick @@ fun () ->
  let scores = create () in
  add "Warband" 3 scores;
  add "Mount & Blade" 4 scores;
  add "Skyrim" 11 scores;
  add "Devil May Cry" 0 scores;
  add "Rocket League" 6 scores;
  add "Luigi's Mansion" 14 scores;
  add "Unreal Tournament" 8 scores;
  add "Xenoblade" 16 scores;
  add "Mario Kart DS" 17 scores;
  add "Hollow Knight" 20 scores;
  add "Cult of the Lamb" 800 scores;
  add "Hellblade" 20 scores;
  save_as_dot (Fun.id, string_of_int) scores "size12.dot";
  let gotten_ic = open_in "size12.dot" in
  let expect_ic = open_in "../../../test/size12_expected.dot" in
  let eof = ref true in
  while !eof do
    try
      let gotten_line = input_line gotten_ic in
      let expect_line = input_line expect_ic in
      Alcotest.(check string)
        "Lines should match expected file" expect_line gotten_line
    with End_of_file -> eof := false
  done;
  close_in gotten_ic;
  close_in expect_ic

let savings = [ save_single; save_12 ]

let () =
  Alcotest.run "Everything"
    [
      ("Basic operations", basics);
      ("Sequential iterations", iterations);
      ("Sequential collisions", collisions);
      ("Hand-made parallel cases", parallel);
      ("Snapshot interactions", snapshots);
      ("Saving to .dot files", savings);
    ]
