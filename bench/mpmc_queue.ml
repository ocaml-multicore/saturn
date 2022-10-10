open Lockfree

let taker queue num_of_elements () =
  let i = ref 0 in
  while !i < num_of_elements do
    if Option.is_some (Mpmc_queue.pop queue) then i := !i + 1
  done

let pusher queue num_of_elements () =
  let i = ref 0 in
  while !i < num_of_elements do
    if Mpmc_queue.push queue !i then i := !i + 1
  done

let num_of_elements = ref 20_000_000 
let num_of_pushers = ref 4 
let num_of_takers = ref 4 
let iterations = ref 10

let run_bench () =
  let queue = Mpmc_queue.create ~size_exponent:3 () in
  let orchestrator =
    Orchestrator.init ~total_domains:(!num_of_takers + !num_of_pushers)
  in
  (* define function to start domains *)
  let start_n_domains n f =
    assert (!num_of_elements mod n == 0);
    let items_per_pusher = !num_of_elements / n in
    List.init n (fun _ ->
        Domain.spawn (fun () ->
            Orchestrator.worker orchestrator (f queue items_per_pusher)))
  in
  (* start domains *)
  let _domains =
    let takers = start_n_domains !num_of_takers taker in
    let pushers = start_n_domains !num_of_pushers pusher in
    Sys.opaque_identity (pushers @ takers)
  in
  (* run test *)
  let results = Orchestrator.run orchestrator !iterations in 
  Printf.printf "iterations: %d, mean: %fs, stddev: %fs" (!iterations) (Stats.mean results) (Stats.stddev results)
;;

let speclist =
  [ ("-items", Arg.Set_int num_of_elements, "number of items to insert and remove");
    ("-pushers", Arg.Set_int num_of_pushers, "number of domains pushing items");
    ("-takers", Arg.Set_int num_of_takers, "number of domains taking times");
    ("-iterations", Arg.Set_int iterations, "run the benchmark this many times");
  ]

let () =
  Arg.parse speclist
    (fun _ -> ())
    "mpmc_queue.exe [-items INT] [-pushers INT] [-takers INT] [-iterations INT]";
  run_bench ();