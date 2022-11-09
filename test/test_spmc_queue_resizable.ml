open Utils
open Lockfree

let assert_empty t =
  Alcotest.(check (option int))
    "queue is empty but just popped item" (Spmc_queue.Resizable.Local.pop t) None;
  Alcotest.(check bool)
    "queue is empty but something is left behind"
    (Spmc_queue.Resizable.Local.is_empty t)
    true

let assert_full t =
  Alcotest.(check bool)
    "queue is full but just pushed item"
    (Spmc_queue.Resizable.Local.push t 0)
    false

let push_resize () =
  let t = Spmc_queue.Resizable.create ~size_exponent:1 () in
  for i = 1 to 128 do
    Spmc_queue.Resizable.Local.push_with_autoresize t i
  done;
  assert_full t;
  for i = 1 to 128 do
    Alcotest.(check (option int))
      "fifo violated" (Spmc_queue.Resizable.Local.pop t) (Some i)
  done;
  assert_empty t


let stealer_domain from_queue wfo counter ~use_steal_one () =
  let t = Spmc_queue.Resizable.create ~size_exponent:15 () in
  Wait_for_others.wait wfo;
  let last_popped = ref (-1) in
  (* start stealing *)
  while true do
    
    let stolen = 
      (* steal with [steal_one] *)
      (if use_steal_one then
        match Spmc_queue.Resizable.Local.steal_one from_queue with 
        | exception _ -> 0
        | _ -> 1

        (* steal with [steal] *)
      else
        Spmc_queue.Resizable.Local.steal ~from:from_queue t)
    in
    Atomic.fetch_and_add counter stolen |> ignore;

    (* drain local *)
    while not (Spmc_queue.Resizable.Local.is_empty t) do
      match Spmc_queue.Resizable.Local.pop t with
      | None -> ()
      | Some v ->
          let monotonic_increase = v > !last_popped in
          Alcotest.(check bool)
            (Printf.sprintf "popped should increase monotonically !(%d > %d)" v !last_popped) monotonic_increase true;
          last_popped := v
    done;
    ()
  done

let stress_with_stealers ~use_steal_one () =
  let wfo = Wait_for_others.init ~total_expected:5 in
  let t = Spmc_queue.Resizable.create ~size_exponent:1 () in
  let stealer_counter = Atomic.make 0 in
  let total_items = 500_000 in
  let _domains =
    Array.init 4 (fun _ ->
        Domain.spawn (stealer_domain t wfo stealer_counter ~use_steal_one))
  in

  (* enqueuer thread starts here *)
  Wait_for_others.wait wfo;
  let i = ref 0 in
  let popped = ref 0 in
  while !i < total_items do
    Spmc_queue.Resizable.Local.push_with_autoresize t !i;
    i := !i + 1;

    (* throw in some popping *)
    if !popped < !i / 2 && Random.int 100 == 0 then
      for _ = 0 to Random.int 10 do
        match Spmc_queue.Resizable.Local.pop t with
        | None -> ()
        | Some _ -> popped := !popped + 1
      done
  done;

  (* wait until stealer finish *)
  while Atomic.get stealer_counter + !popped < total_items do
    ()
  done;
  assert_empty t


let stress_resize_and_steal () =
  let f () = 
    let size_exp = 4 in
    let queue = Spmc_queue.Resizable.create ~size_exponent:size_exp () in
    let size = 1 lsl size_exp in
    let sema = Semaphore.Binary.make false in

    for i = 0 to size / 2 do
      Spmc_queue.Resizable.Local.push queue i |> ignore
    done;
      
    let d1 =
      Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        Spmc_queue.Resizable.Local.resize queue)
    in
    let d2 =
      Domain.spawn (fun () ->
        let q' = Spmc_queue.Resizable.create ~size_exponent:size_exp () in
        Semaphore.Binary.release sema;        
        Spmc_queue.Resizable.Local.steal ~from:queue q' |> ignore; 
        q')
    in
    Domain.join d1;
    let _q' = Domain.join d2 in
    ()
  in 
  for _ = 0 to 100_000 do 
    f () 
  done;;



let () =
  let open Alcotest in
  run "Spmc_queue_resizable"
    [
      ( "single-thread",
        [
          test_case "resize" `Quick push_resize;
        ] );
      ( "multi-thread",
        [
          test_case "stress steal and resize" `Slow 
            stress_resize_and_steal;
          test_case "stress push pop steal" `Slow
            (stress_with_stealers  ~use_steal_one:false);
          test_case "stress push pop steal one " `Slow
            (stress_with_stealers ~use_steal_one:true);
        ] );
    ]
