open Lockfree

let push_steal () =
  let queue = Spmc_queue.create ~size_exponent:5 () in
  let num_of_elements = 200_000 in
  (* start dequeuer *)
  let dequeuer =
    Domain.spawn (fun () ->
        let i = ref 0 in
        while !i < num_of_elements do
          match Spmc_queue.steal_one queue with
          | Some item ->
              Alcotest.(check int)
                "popped items should follow FIFO order" item !i;
              i := !i + 1
          | None -> ()
        done)
  in
  (* enqueue *)
  let i = ref 0 in
  while !i < num_of_elements do
    if Spmc_queue.local_push queue !i then i := !i + 1
  done;
  Domain.join dequeuer |> ignore;
  ()

let no_item_popped_twice () =
  let make_once () =
    let flag = Atomic.make true in
    fun () -> assert (Atomic.exchange flag false)
  in

  let queue = Spmc_queue.create ~size_exponent:17 () in
  let num_of_elements = 100_000 in
  (* start dequeuers *)
  let dequers =
    let dequer () =
      Domain.spawn (fun () ->
          let i = ref 0 in
          while !i < num_of_elements / 4 do
            match Spmc_queue.steal_one queue with
            | Some once ->
                once ();
                i := !i + 1
            | None -> ()
          done)
    in
    [ dequer (); dequer () ]
  in
  (* enqueue *)
  let i = ref 0 in
  while !i < num_of_elements do
    let once = make_once () in
    if Spmc_queue.local_push queue once then i := !i + 1
  done;

  let i = ref 0 in
  while !i < num_of_elements / 2 do
    match Spmc_queue.local_pop queue with
    | Some once ->
        once ();
        i := !i + 1
    | None -> ()
  done;
  List.iter Domain.join dequers;
  ()

let () =
  let open Alcotest in
  run "Spmc_queue"
    [
      ( "multicore",
        [
          test_case "push, steal" `Quick push_steal;
          test_case "no double pop" `Quick no_item_popped_twice;
        ] );
    ]
