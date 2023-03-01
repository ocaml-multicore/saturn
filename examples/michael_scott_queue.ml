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

let concurrent_push () =
  let ms_q = create () in
  let item = 42 in

  (* push concurrently *)
  let pusher _ =
    push ms_q item;
    Printf.printf "concurrent_push: pushed %d\n" item
  in
  let domains = Array.init n_domains (fun _ -> Domain.spawn pusher) in
  Array.iter Domain.join domains;

  (* pop sequentially and examine *)
  for _ = 1 to n_domains do
    match pop ms_q with
    | None -> failwith "concurrent_push: queue is empty"
    | Some v -> Printf.printf "concurrent_push: popped %d\n" v
  done

let concurrent_pop () =
  let ms_q = create () in
  let item = 42 in

  (* push sequentially *)
  for _ = 1 to n_domains do
    push ms_q item;
    Printf.printf "concurrent_pop: pushed %d\n" item
  done;

  (* pop concurrently *)
  let popper _ =
    match pop ms_q with
    | None -> failwith "concurrent_pop: list is empty"
    | Some v -> Printf.printf "concurrent_pop: popped %d\n" v
  in
  let domains = Array.init n_domains (fun _ -> Domain.spawn popper) in
  Array.iter Domain.join domains

let concurrent_push_and_pop () =
  let ms_q = create () in
  let item = 42 in

  (* push and pop, both concurrently *)
  let pusher _ =
    push ms_q item;
    Printf.printf "concurrent_push_and_pop: pushed %d\n" item
  in
  let rec pop_one _ =
    match pop ms_q with
    | None -> pop_one () (* keep trying until an item is popped *)
    | Some v -> Printf.printf "concurrent_push_and_pop: popped %d\n" v
  in

  (* n_domains/2 pushers, n_domains/2 poppers concurrently *)
  let popper_domains =
    Array.init (n_domains / 2) (fun _ -> Domain.spawn pop_one)
  in
  let pusher_domains =
    Array.init (n_domains / 2) (fun _ -> Domain.spawn pusher)
  in
  Array.iter Domain.join (Array.append pusher_domains popper_domains)

let main () =
  single_push_and_pop ();
  concurrent_push ();
  concurrent_pop ();
  concurrent_push_and_pop ()

let _ = main ()
