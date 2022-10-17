open Lockfree

let num_of_elements = ref 2_000_000
let num_of_stealers = ref 0 
let iterations = ref 10
let owner_adds_only = ref false

let round_done = Atomic.make 0


let owner queue ~round:_ = 
  let left = ref !num_of_elements in 
  while !left > 0 do 
    (* insert items as owner*)
    for _ = 0 to Random.int 100 do 
      if Spmc_queue.Local.push queue 0
      then  
        left := !left - 1; 
    done;
    (* pop *)
    if not !owner_adds_only then (
    while Option.is_some (Spmc_queue.Local.pop queue) do () done);
  done;
  (* drain whatever remains in the queue *)
  while Option.is_some (Spmc_queue.Local.pop queue) do () done;
  Atomic.incr round_done;
;;

let stealer victim_queue ~round = 
  let queue = Spmc_queue.create ~size_exponent:10 () in 
  while Atomic.get round_done < round do 
    Spmc_queue.Local.steal ~from:victim_queue queue |> ignore;
    while Option.is_some (Spmc_queue.Local.pop queue) do () done;
  done;;

let run_bench () =
  let queue = Spmc_queue.create ~size_exponent:10 () in
  let orchestrator =
    Orchestrator.init ~total_domains:(!num_of_stealers + 1)
  in
  (* define function to start domains *)
  let start_n_domains n f =
    List.init n (fun _ ->
        Domain.spawn (fun () ->
            Orchestrator.worker orchestrator (f queue)))
  in
  (* start domains *)
  let _domains =
    let owner = start_n_domains 1 owner in
    let takers = start_n_domains !num_of_stealers stealer in
    Sys.opaque_identity (owner @ takers)
  in
  (* run test *)
  let results = Orchestrator.run orchestrator !iterations in 
  Printf.printf "iterations: %d, mean: %fs, stddev: %fs" (!iterations) (Stats.mean results) (Stats.stddev results)
;;

let speclist =
  [ ("-items", Arg.Set_int num_of_elements, "number of items to insert and remove");
    ("-stealers", Arg.Set_int num_of_stealers, "number of domains stealing items");
    ("-iterations", Arg.Set_int iterations, "run the benchmark this many times");
    ("-owner-adds-only", Arg.Set owner_adds_only, "queue owner is only adding items; all pops are by thieves")
  ]

let () =
  Arg.parse speclist
    (fun _ -> ())
    "spmc_queue.exe [-items INT] [-stealers INT] [-iterations INT]  [-owner-adds-only]";
  run_bench ();