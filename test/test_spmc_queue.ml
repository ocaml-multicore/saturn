open Utils
open Lockfree

let insert t n =
  for _ = 1 to n do
    Alcotest.(check bool)
      "queue should not be full"
      (Spmc_queue.Local.push t n)
      true
  done

let remove t n =
  for _ = 1 to n do
    Alcotest.(check bool)
      "queue should not be empty"
      (Option.is_some (Spmc_queue.Local.pop t))
      true
  done

let assert_empty t =
  Alcotest.(check (option int))
    "queue is empty but just popped item" (Spmc_queue.Local.pop t) None;
  Alcotest.(check bool)
    "queue is empty but something is left behind"
    (Spmc_queue.Local.is_empty t)
    true

let assert_full t =
  Alcotest.(check bool)
    "queue is full but just pushed item"
    (Spmc_queue.Local.push t 0)
    false

let steal_and_assert ~from t stole_n =
  Alcotest.(check int)
    "reported steal size does not match expected"
    (Spmc_queue.Local.steal ~from t)
    stole_n

let push_pop () =
  let t = Spmc_queue.create ~size_exponent:3 () in
  assert_empty t;
  insert t 4;
  remove t 4;
  assert_empty t;
  insert t 8;
  assert_full t;
  remove t 8;
  assert_empty t;
  ()

let push_steal () =
  let t = Spmc_queue.create ~size_exponent:3 () in
  assert_empty t;
  insert t 4;
  let from = Spmc_queue.create ~size_exponent:3 () in
  insert from 4;
  steal_and_assert ~from t 2;
  steal_and_assert ~from t 1;
  steal_and_assert ~from t 1;
  steal_and_assert ~from t 0;
  assert_empty from;
  assert_full t;
  ()

let push_resize () =
  let t = Spmc_queue.create ~size_exponent:1 () in
  for i = 1 to 128 do
    Spmc_queue.Local.push_with_autoresize t i
  done;
  assert_full t;
  for i = 1 to 128 do
    Alcotest.(check (option int))
      "fifo violated" (Spmc_queue.Local.pop t) (Some i)
  done;
  assert_empty t

let stress () =
  Random.init 123;
  let total_items = 1_000_000 in
  let t = Spmc_queue.create ~size_exponent:6 () in
  let push_next = ref 0 in
  let last_popped = ref (-1) in
  while !last_popped < total_items do
    (* insert some items *)
    let n = Random.int 100 in
    for _ = 1 to n do
      if !push_next <= total_items && Spmc_queue.Local.push t !push_next then
        push_next := !push_next + 1
    done;
    (* remove some items *)
    let n = Random.int 100 in
    for _ = 1 to n do
      match Spmc_queue.Local.pop t with
      | None -> ()
      | Some v ->
          Alcotest.(check int) "popped item out of order" v (!last_popped + 1);
          last_popped := v
    done
  done;
  assert_empty t

let stealer_domain from_queue wfo counter ~use_steal_one () =
  let t = Spmc_queue.create ~size_exponent:15 () in
  Wait_for_others.wait wfo;
  let last_popped = ref (-1) in
  (* start stealing *)
  while true do
    
    (* steal with [steal_one] *)
    (if use_steal_one then
     match Spmc_queue.M.steal from_queue with 
     | exception _ -> () 
     | item -> (
      Spmc_queue.Local.push_with_autoresize t item;
      Atomic.incr counter) 

    (* steal with [steal] *)
    else
      let stolen = Spmc_queue.Local.steal ~from:from_queue t in
      Atomic.fetch_and_add counter stolen |> ignore);

    (* drain local *)
    while not (Spmc_queue.Local.is_empty t) do
      match Spmc_queue.Local.pop t with
      | None -> ()
      | Some v ->
          let monotonic_increase = v > !last_popped in
          Alcotest.(check bool)
            "popped should increase monotonically" monotonic_increase true;
          last_popped := v
    done;
    ()
  done

let stress_with_stealers ?(with_resize = false) ?(use_steal_one = false) () =
  let wfo = Wait_for_others.init ~total_expected:5 in
  let t = Spmc_queue.create ~size_exponent:(if with_resize then 1 else 7) () in
  let stealer_counter = Atomic.make 0 in
  let total_items = 2_000_000 in
  let _domains =
    Array.init 4 (fun _ ->
        Domain.spawn (stealer_domain t wfo stealer_counter ~use_steal_one))
  in
  let push_f queue item =
    match with_resize with
    | false -> Spmc_queue.Local.push queue item
    | true ->
        Spmc_queue.Local.push_with_autoresize queue item;
        true
  in

  (* enqueuer thread starts here *)
  Wait_for_others.wait wfo;
  let i = ref 0 in
  let popped = ref 0 in
  while !i < total_items do
    if push_f t !i then i := !i + 1;

    (* throw in some popping *)
    if !popped < !i / 2 && Random.int 100 == 0 then
      for _ = 0 to Random.int 10 do
        match Spmc_queue.Local.pop t with
        | None -> ()
        | Some _ -> popped := !popped + 1
      done
  done;

  (* wait until stealer finish *)
  while Atomic.get stealer_counter + !popped < total_items do
    ()
  done;
  assert_empty t

let () =
  let open Alcotest in
  run "Spmc_queue"
    [
      ( "single-thread",
        [
          test_case "is it a queue" `Quick push_pop;
          test_case "stealing" `Quick push_steal;
          test_case "resize" `Quick push_resize;
          test_case "stress push pop" `Slow stress;
        ] );
      ( "multi-thread",
        [
          test_case "stress push pop steal" `Slow
            stress_with_stealers;
          test_case "stress push pop steal and resize " `Slow
            (stress_with_stealers ~with_resize:true);          
          test_case "stress push pop steal one " `Slow
            (stress_with_stealers ~use_steal_one:true);
        ] );
    ]
