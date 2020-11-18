module Msqueue = Lockfree.MSQueue

let num_domains = try int_of_string Sys.argv.(1) with _ -> 4 
let num_items = try int_of_string Sys.argv.(2) with _ -> 10000000
let read_percent = try int_of_string Sys.argv.(3) with _ -> 50
let items_per_domain = num_items / num_domains

let k = Domain.DLS.new_key Random.State.make_self_init 

let enqueue_or_dequeue msq n () =
  let state = Domain.DLS.get k in
  for i = 1 to n do
    let r = Random.State.int state 100 in
    if (r > read_percent) then
      Msqueue.push msq i
    else
      Msqueue.pop msq |> ignore
  done

let queue = Msqueue.create ()

let _ =
  let d = Array.init (num_domains - 1) (fun _ -> Domain.spawn(enqueue_or_dequeue queue items_per_domain)) in
  enqueue_or_dequeue queue items_per_domain ();
  Array.iter Domain.join d
