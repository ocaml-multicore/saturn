(** Tests *)
open Lockfree
module Ws_deque = Ws_deque.M

let test_empty () =
  let q = Ws_deque.create () in
  assert (Ws_deque.is_empty q = true);
  Ws_deque.push q 42;
  assert (Ws_deque.is_empty q = false);
  print_string "test_empty: ok\n"

let test_exit () =
  let q = Ws_deque.create () in
  let r = try
            Ws_deque.pop q
          with Exit -> - 1
  in
  assert (r = -1);
  print_string "test_exit: ok\n"

let test_push_and_pop () =
  let q = Ws_deque.create () in
  Ws_deque.push q 1;
  Ws_deque.push q 10;
  Ws_deque.push q 100;
  assert (Ws_deque.pop q = 100);
  assert (Ws_deque.pop q = 10);
  assert (Ws_deque.pop q = 1);
  print_string "test_push_and_pop: ok\n"

let test_push_and_steal () =
  let q = Ws_deque.create () in
  Ws_deque.push q 1;
  Ws_deque.push q 10;
  Ws_deque.push q 100;
  let domains = Array.init 3 (fun _ ->
    Domain.spawn (fun _ -> let v =
      Ws_deque.steal q in assert (v = 1 || v = 10 || v = 100))) in
  Array.iter Domain.join domains;
  print_string "test_push_and_steal: ok\n"

let test_size () =
  let q = Ws_deque.create () in
  Ws_deque.push q 1;
  Ws_deque.push q 2;
  Ws_deque.push q 3;
  Ws_deque.push q 4;
  Ws_deque.push q 5;
  assert (Ws_deque.size q = 5);
  print_string "test_size: ok\n"

let _ =
  test_empty ();
  test_exit ();
  test_push_and_pop ();
  test_push_and_steal ();
  test_size ()
