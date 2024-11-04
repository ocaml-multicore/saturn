open Multicore_bench

let run_one ~unsafe ~budgetf ?(size_exponent = 3)
    ?(n_msgs = 80 * Util.iter_factor) () =
  let init _ = () in
  let work, before =
    if unsafe then
      let module Queue = Saturn.Single_prod_single_cons_queue_unsafe in
      let t = Queue.create ~size_exponent in

      let before () =
        while Queue.size t <> 0 do
          Queue.pop_exn t |> ignore
        done;
        let n = Random.int ((1 lsl size_exponent) + 1) in
        for i = 1 to n do
          Queue.push_exn t i
        done
      in
      let work i () =
        if i = 0 then
          let rec loop n =
            if 0 < n then
              if Queue.try_push t n then loop (n - 1)
              else begin
                Domain.cpu_relax ();
                loop n
              end
          in
          loop n_msgs
        else
          let rec loop n =
            if 0 < n then
              match Queue.pop_opt t with
              | Some _ -> loop (n - 1)
              | None ->
                  Domain.cpu_relax ();
                  loop n
          in
          loop n_msgs
      in
      (work, before)
    else
      let module Queue = Saturn.Single_prod_single_cons_queue in
      let t = Queue.create ~size_exponent in

      let before () =
        while Queue.size t <> 0 do
          Queue.pop_exn t |> ignore
        done;
        let n = Random.int ((1 lsl size_exponent) + 1) in
        for i = 1 to n do
          Queue.push_exn t i
        done
      in
      let work i () =
        if i = 0 then
          let rec loop n =
            if 0 < n then
              if Queue.try_push t n then loop (n - 1)
              else begin
                Domain.cpu_relax ();
                loop n
              end
          in
          loop n_msgs
        else
          let rec loop n =
            if 0 < n then
              match Queue.pop_opt t with
              | Some _ -> loop (n - 1)
              | None ->
                  Domain.cpu_relax ();
                  loop n
          in
          loop n_msgs
      in
      (work, before)
  in

  let config =
    Printf.sprintf "2 workers, capacity %d%s" (1 lsl size_exponent)
      (if unsafe then " (unsafe)" else "")
  in
  Times.record ~budgetf ~n_domains:2 ~before ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  let run ~unsafe =
    [ 0; 3; 6; 9; 12; 15 ]
    |> List.concat_map @@ fun size_exponent ->
       run_one ~budgetf ~size_exponent ~unsafe ()
  in
  List.fold_right2
    (fun safe unsafe acc -> safe :: unsafe :: acc)
    (run ~unsafe:false) (run ~unsafe:true) []
