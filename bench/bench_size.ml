open Multicore_bench
module Size = Saturn.Size

let run_one ~budgetf ~n_domains ?(n_ops = 250 * n_domains * Util.iter_factor) ()
    =
  let t = Size.create () in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ = Atomic.set n_ops_todo n_ops in
  let work _ () =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then begin
            let incr = Size.new_once t Size.incr in
            Size.update_once t incr;
            let decr = Size.new_once t Size.decr in
            Size.update_once t decr;
            loop (n - 2)
          end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~config ~singular:"operation"

let run_suite ~budgetf =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains -> run_one ~n_domains ~budgetf ()
