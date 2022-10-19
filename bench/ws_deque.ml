open Lockfree

let num_of_elements = ref 10_000_000
let num_of_stealers = ref 0 
let iterations = ref 10
let owner_adds_only = ref false

module Benchmark(S : Ws_deque.S) = struct
  let round_done = Atomic.make 0

  let pop queue = 
    try Some (S.pop queue) with 
    | Exit -> None 

  let owner queue ~round:_ = 
    let left = ref !num_of_elements in 
    while !left > 0 do 
      (* insert items as owner*)
      for _ = 0 to Random.int 100 do 
        S.push queue 0;
        left := !left - 1;
      done;
      (* pop *)
      if not !owner_adds_only then (
      while Option.is_some (pop queue) do () done);
    done;
    (* drain whatever remains in the queue *)
    while Option.is_some (pop queue) do () done;
    Atomic.incr round_done;
  ;;

  let stealer victim_queue ~round = 
    while Atomic.get round_done < round do 
      pop victim_queue |> ignore;
    done;;

  let run_bench () =
    let queue = S.create () in
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

end

let speclist =
  [ ("-items", Arg.Set_int num_of_elements, "number of items to insert and remove");
    ("-stealers", Arg.Set_int num_of_stealers, "number of domains stealing items");
    ("-iterations", Arg.Set_int iterations, "run the benchmark this many times");
    ("-owner-adds-only", Arg.Set owner_adds_only, "queue owner is only adding items; all pops are by thieves")
  ]

let which_ds = ref None;;

let f () =
  Arg.parse speclist
    (fun v -> which_ds := Some v)
    "ws_deque.exe [-items INT] [-stealers INT] [-iterations INT] [-owner-adds-only] (deque|queue)";
  let module M = (val 
    (match !which_ds with 
    | Some "deque"-> (module Lockfree.Ws_deque.M : Ws_deque.S)
    | Some "queue" -> (module Lockfree.Spmc_queue.M : Ws_deque.S)
    | Some v -> failwith ("unknown structure name passed: " ^ v)
    | _ -> failwith "you need to specify data structure to bench as anon param"
    ))
  in
  let module B = Benchmark(M) in
  B.run_bench ();;

f () 