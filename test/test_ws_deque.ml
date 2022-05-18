(** Tests *)
open Lockfree
module Ws_deque = Ws_deque.M

let test_empty () =
  let q = Ws_deque.create () in
  match Ws_deque.pop q with
  | exception Exit ->
      print_string "test_exit: ok\n"
  | _ ->
      assert false

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

let _ =
  test_empty ();
  test_push_and_pop ();
  test_push_and_steal ();
