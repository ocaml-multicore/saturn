(** Tests *)
open Lockfree

let test_empty () =
  let q = Spsc_queue.create ~size_exponent:3 in
  assert (Option.is_none (Spsc_queue.dequeue q)); 
  assert (Spsc_queue.size q == 0);
  print_string "test_spsc_queue_empty: ok\n"
;;


let test_full () =
  let q = Spsc_queue.create ~size_exponent:3 in
  for _ = 1 to 8 do 
    assert (Spsc_queue.enqueue q ()); 
  done;
  assert (not (Spsc_queue.enqueue q ())); 
  assert (Spsc_queue.size q == 8);
  print_string "test_spsc_queue_full: ok\n";
;;


let test_parallel () =
  let count = 100_000 in 
  let q = Spsc_queue.create ~size_exponent:2 in
  (* producer *)
  let producer = 
    Domain.spawn (fun () -> 
    for i = 1 to count do 
      while not (Spsc_queue.enqueue q i) do () done;
    done)
  in
  (* consumer *)
  let last_num = ref 0 in 
  while !last_num < count do 
    match Spsc_queue.dequeue q with 
    | None -> () 
    | Some v -> 
      (assert (v == !last_num + 1);
      last_num := v)
  done;
  assert (Option.is_none (Spsc_queue.dequeue q));
  assert (Spsc_queue.size q == 0);
  Domain.join producer;
  Printf.printf "test_spsc_queue_parallel: ok (transferred = %d)\n" !last_num;;


let _ =
  test_empty ();
  test_full ();
  test_parallel ();;