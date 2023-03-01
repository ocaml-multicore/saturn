open Lockfree.Michael_scott_queue

let push_work_pop queue item =
    push queue item;
    for _ = 1 to 100_000_000 do 
        (* do other work *)
        ignore ()
    done;
    match pop queue with
    | Some v -> Printf.printf "pushed %d and popped %d\n" item v
    | None -> failwith "pop failed: empty list"

let main () =
    let ms_q = create () in
    let d1 = Domain.spawn(fun _ -> push_work_pop ms_q 1) in
    let d2 = Domain.spawn(fun _ -> push_work_pop ms_q 2) in
    let d3 = Domain.spawn(fun _ -> push_work_pop ms_q 3) in
    Domain.join d1; Domain.join d2; Domain.join d3

let _ = main ()
