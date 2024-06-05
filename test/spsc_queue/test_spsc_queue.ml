module Tests_spsc (Spsc_queue : Spsc_queues.SPSC_tests) = struct
  (** Tests *)

  let test_empty () =
    let q = Spsc_queue.create ~size_exponent:3 in
    assert (Option.is_none (Spsc_queue.pop_opt q));
    assert (Spsc_queue.size q == 0);
    Printf.printf "test_%s_empty: ok\n" Spsc_queue.name

  let push_not_full q elt =
    try
      Spsc_queue.push_exn q elt;
      true
    with Spsc_queue.Full -> false

  let test_full () =
    let q = Spsc_queue.create ~size_exponent:3 in
    while push_not_full q () do
      Domain.cpu_relax ()
    done;
    assert (Spsc_queue.size q == 8);
    Printf.printf "test_%s_full: ok\n" Spsc_queue.name

  let test_parallel () =
    let count =
      let ocaml_4 =
        Char.code (String.get Sys.ocaml_version 0) < Char.code '5'
      in
      match ocaml_4 with true -> 100 | false -> 100_000
    in
    let q = Spsc_queue.create ~size_exponent:2 in
    (* producer *)
    let producer =
      Domain.spawn (fun () ->
          for i = 1 to count do
            while not (push_not_full q (Float.of_int i)) do
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
          assert (v = Float.of_int (!last_num + 1));
          last_num := Float.to_int v
    done;
    assert (Option.is_none (Spsc_queue.pop_opt q));
    assert (Spsc_queue.size q == 0);
    Domain.join producer;
    Printf.printf "test_%s_parallel: ok (transferred = %d)\n" Spsc_queue.name
      !last_num

  let test_float () =
    let q = Spsc_queue.create ~size_exponent:1 in
    assert (Spsc_queue.try_push q 1.01);
    assert (Spsc_queue.pop_opt q = Some 1.01)

  let run () =
    test_empty ();
    test_full ();
    test_parallel ();
    test_float ()
end

let () =
  let module Safe = Tests_spsc (Spsc_queues.Spsc_queue) in
  Safe.run ();
  let module Unsafe = Tests_spsc (Spsc_queues.Spsc_queue_unsafe) in
  Unsafe.run ()
