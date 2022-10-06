open Lockfree.Mpmc_queue

let smoke_test () =
  let queue = create ~size_exponent:2 () in
  (* enqueue 4 *)
  for i = 1 to 4 do
    assert (push queue i)
  done;
  assert (not (push queue 0));
  let ({ tail; head; _ } : 'a t) = queue in
  assert (Atomic.get tail = 4);
  assert (Atomic.get head = 0);
  (* dequeue 4 *)
  for i = 1 to 4 do
    assert (Some i = pop queue)
  done;
  assert (Option.is_none (pop queue))
;;

smoke_test ()

let two_threads_test () =
  let queue = create ~size_exponent:2 () in
  let num_of_elements = 1_000_000 in
  (* start dequeuer *)
  let dequeuer =
    Domain.spawn (fun () ->
        let i = ref 0 in
        while !i < num_of_elements do
          match pop queue with
          | Some item ->
              assert (item = !i);
              i := !i + 1
          | None -> ()
        done)
  in
  (* enqueue *)
  let i = ref 0 in
  while !i < num_of_elements do
    if push queue !i then i := !i + 1
  done;
  Domain.join dequeuer |> ignore;
  ()
;;

two_threads_test ()

module Wait_for_others = struct
  type t = { currently : int Atomic.t; total_expected : int }

  let init ~total_expected = { currently = Atomic.make 0; total_expected }

  let wait { currently; total_expected } =
    Atomic.incr currently;
    while Atomic.get currently < total_expected do
      ()
    done
end

let taker wfo queue num_of_elements () =
  Wait_for_others.wait wfo;
  let i = ref 0 in
  while !i < num_of_elements do
    if Option.is_some (pop queue) then i := !i + 1
  done

let pusher wfo queue num_of_elements () =
  Wait_for_others.wait wfo;
  let i = ref 0 in
  while !i < num_of_elements do
    if push queue !i then i := !i + 1
  done

let run_test num_takers num_pushers =
  let queue = create ~size_exponent:6 () in
  let num_of_elements = 4_000_000 in
  let wfo = Wait_for_others.init ~total_expected:(num_takers + num_pushers) in
  let _ =
    let takers =
      assert (num_of_elements mod num_takers == 0);
      let items_per_taker = num_of_elements / num_takers in 
      List.init num_takers (fun _ ->
          Domain.spawn (taker wfo queue items_per_taker))
    in
    let pushers =
      assert (num_of_elements mod num_pushers == 0);
      let items_per_pusher = num_of_elements / num_pushers in 
      List.init num_pushers (fun _ ->
          Domain.spawn (pusher wfo queue items_per_pusher))
    in
    Sys.opaque_identity (List.map Domain.join (pushers @ takers))
  in
  let ({ array; head; tail; _ } : 'a t) = queue in
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  assert (head_val = tail_val);
  Array.iter (fun item -> assert (Option.is_none (Atomic.get item))) array;
  Stdlib.flush_all();
;;

let scenarios = [(4,4); (8,1); (1,8)];;

List.iter (fun (takers, pushers) -> run_test takers pushers) scenarios;;

