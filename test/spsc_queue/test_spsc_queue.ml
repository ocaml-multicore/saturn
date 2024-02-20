module Spsc_queue = Saturn_lockfree.Single_prod_single_cons_queue
(** Tests *)

let test_empty () =
  let q = Spsc_queue.create ~size_exponent:3 in
  assert (Option.is_none (Spsc_queue.pop_opt q));
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
    Domain.cpu_relax ()
  done;
  assert (Spsc_queue.size q == 8);
  print_string "test_spsc_queue_full: ok\n"

let test_parallel () =
  let count =
    let ocaml_4 = Char.code (String.get Sys.ocaml_version 0) < Char.code '5' in
    match ocaml_4 with true -> 100 | false -> 100_000
  in
  let q = Spsc_queue.create ~size_exponent:2 in
  (* producer *)
  let producer =
    Domain.spawn (fun () ->
        for i = 1 to count do
          while not (push_not_full q i) do
            Domain.cpu_relax ()
          done
        done)
  in
  (* consumer *)
  let last_num = ref 0 in
  while !last_num < count do
    match Spsc_queue.pop_opt q with
    | None -> Domain.cpu_relax ()
    | Some v ->
        assert (v == !last_num + 1);
        last_num := v
  done;
  assert (Option.is_none (Spsc_queue.pop_opt q));
  assert (Spsc_queue.size q == 0);
  Domain.join producer;
  Printf.printf "test_spsc_queue_parallel: ok (transferred = %d)\n" !last_num

let _ =
  test_empty ();
  test_full ();
  test_parallel ()
