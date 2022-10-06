open Lockfree.Mpmc_queue

let _smoke_test () =
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

_smoke_test ()

let log s =
  Printf.printf "%s\n" s;
  Stdlib.flush_all ()

let _test_1 () =
  log "test start\n";
  let queue = create ~size_exponent:2 () in
  Domain.spawn (fun () ->
      while true do
        let ({ array; head; tail; _ } : 'a t) = queue in
        log
          (Printf.sprintf "\nhd: %d, tl:%d\n" (Atomic.get head)
             (Atomic.get tail));
        log
          (Array.to_list array |> List.map Atomic.get
          |> List.map (function
               | None -> "none"
               | Some v -> Printf.sprintf "%d" v)
          |> String.concat ",");
        Unix.sleepf 0.3
      done)
  |> ignore;
  let num_of_elements = 10_000_000 in
  (* start dequeuer *)
  let dequeuer =
    Domain.spawn (fun () ->
        let i = ref 0 in
        while !i < num_of_elements do
          if Option.is_some (pop queue) then i := !i + 1
        done;
        log "receiver done\n")
  in
  (* enqueue *)
  let i = ref 0 in
  while !i < num_of_elements do
    if push queue !i then i := !i + 1
  done;
  log "sender done\n";
  Domain.join dequeuer |> ignore;
  ()
;;

_test_1 ()
