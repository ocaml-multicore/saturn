open Lockfree.Mpmc_queue


let log ~thr s = 
  let s = 
    match s with 
    | None -> "Empty?" 
    | Some s -> s    
  in
  Printf.printf "%s: %s\n" thr s;
  Stdlib.flush Stdlib.stdout;;


let test_1 () =
  let queue = create ~size_exponent:3 () in
  let a = 
    let log = log ~thr:"A" in 
    Domain.spawn (fun () ->
      assert (push queue "a");
      assert (push queue "b");
      assert (push queue "c");
      pop queue |> log;
      log (Some "done a")) 
  in 
  let b = 
    let log = log ~thr:"B" in
    Domain.spawn (fun () ->
      pop queue |> log;
      assert (push queue "d");
      pop queue |> log;
      pop queue |> log;
      log (Some "done b"))
  in 
  Domain.join b |> ignore; 
  Domain.join a |> ignore;
  ();;

test_1 ();;