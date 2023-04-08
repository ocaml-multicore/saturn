open Lockfree.Michael_scott_queue

let n_domains = 4

let single_push_and_pop () =
  let ms_q = create () in
  let item = 1 in
  push ms_q item;
  Printf.printf "single_push_and_pop: pushed %d\n" item;
  match pop ms_q with
  | None -> failwith "single_push_and_pop: queue is empty"
  | Some v -> Printf.printf "single_push_and_pop: popped %d\n" v

let do_work () =
    (* do some work *)
    for _ = 1 to Random.int 100_000 do
        Domain.cpu_relax ()
    done

let concurrent_push () =
  let ms_q = create () in

  (* push concurrently *)
  let pusher id item _ =
    do_work ();
    push ms_q item;
    Printf.printf "concurrent_push: pushed %d (pusher id: %d)\n" item id
  in
  let domains = Array.init n_domains (fun i -> Domain.spawn (pusher i (i+1))) in
  Array.iter Domain.join domains

let concurrent_pop () =
  let ms_q = create () in

  (* push sequentially *)
  for i = 1 to n_domains do
    push ms_q i
  done;

  (* pop concurrently *)
  let popper id _ =
    do_work ();
    match pop ms_q with
    | None -> failwith "concurrent_pop: list is empty"
    | Some v -> Printf.printf "concurrent_pop: popped %d (popper id: %d)\n" v id 
  in
  let domains = Array.init n_domains (fun i -> Domain.spawn (popper i)) in
  Array.iter Domain.join domains

let concurrent_push_and_pop () =
  let ms_q = create () in

  (* push and pop, both concurrently *)
  let pusher id item _ =
    do_work ();
    push ms_q item;
    Printf.printf "concurrent_push_and_pop: pushed %d (pusher id: %d)\n" item id
  in
  let rec pop_one id _ =
    do_work ();
    match pop ms_q with
    | None -> pop_one id () (* keep trying until an item is popped *)
    | Some v -> Printf.printf "concurrent_push_and_pop: popped %d (popper id: %d)\n" v id
  in

  (* n_domains/2 pushers, n_domains/2 poppers concurrently *)
  let popper_domains =
    Array.init (n_domains / 2) (fun i -> Domain.spawn (pop_one i))
  in
  let pusher_domains =
    Array.init (n_domains / 2) (fun i -> Domain.spawn (pusher i (i+1)))
  in
  Array.iter Domain.join (Array.append pusher_domains popper_domains)

let main () =
  single_push_and_pop ();
  concurrent_push ();
  concurrent_pop ();
  concurrent_push_and_pop ()

let _ = main ()
