(** Tests *)
open Lockfree.Ws_deque.M

let test_empty () =
  let q = create () in
  match pop q with
  | exception Exit ->
      print_string "test_exit: ok\n"
  | _ ->
      assert false

let test_push_and_pop () =
  let q = create () in
  push q 1;
  push q 10;
  push q 100;
  assert (pop q = 100);
  assert (pop q = 10);
  assert (pop q = 1);
  print_string "test_push_and_pop: ok\n"

let test_push_and_steal () =
  let q = create () in
  push q 1;
  push q 10;
  push q 100;
  let domains = Array.init 3 (fun _ ->
    Domain.spawn (fun _ -> let v =
      steal q in assert (v = 1 || v = 10 || v = 100))) in
  Array.iter Domain.join domains;
  print_string "test_push_and_steal: ok\n"

let _ =
  test_empty ();
  test_push_and_pop ();
  test_push_and_steal ();
