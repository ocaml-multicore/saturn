open Lockfree
(** Tests *)

let test_empty () =
  let q = Spsc_queue.create ~size_exponent:3 in
  assert (Option.is_none (Spsc_queue.pop q));
  assert (Spsc_queue.size q == 0);
  print_string "test_spsc_queue_empty: ok\n"

let push_not_full q elt =
  try
    Spsc_queue.push q elt;
    true
  with Spsc_queue.Full -> false

let test_full () =
  let q = Spsc_queue.create ~size_exponent:3 in
  while push_not_full q () do
    ()
  done;
  assert (Spsc_queue.size q == 8);
  print_string "test_spsc_queue_full: ok\n"

let test_parallel () =
  let count = 100_000 in
  let q = Spsc_queue.create ~size_exponent:2 in
  (* producer *)
  let producer =
    Domain.spawn (fun () ->
        for i = 1 to count do
          while not (push_not_full q i) do
            ()
          done
        done)
  in
  (* consumer *)
  let last_num = ref 0 in
  while !last_num < count do
    match Spsc_queue.pop q with
    | None -> ()
    | Some v ->
        assert (v == !last_num + 1);
        last_num := v
  done;
  assert (Option.is_none (Spsc_queue.pop q));
  assert (Spsc_queue.size q == 0);
  Domain.join producer;
  Printf.printf "test_spsc_queue_parallel: ok (transferred = %d)\n" !last_num

let _ =
  test_empty ();
  test_full ();
  test_parallel ()
