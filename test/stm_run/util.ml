let run_with_budget ~budgetf ~count run =
  let state = Random.State.make_self_init () in
  let start = Unix.gettimeofday () in
  let rec loop ~total n =
    let current = Unix.gettimeofday () in
    if current -. start <= budgetf && total < count then begin
      let count =
        if total = 0 then n
        else
          let per_test = (current -. start) /. Float.of_int total in
          let max_count =
            Float.to_int ((start +. budgetf -. current) /. per_test)
          in
          Int.min (Int.min n (count - total)) max_count |> Int.max 32
      in
      let seed = Random.State.full_int state Int.max_int in
      QCheck_base_runner.set_seed seed;
      let error_code = run count in
      if error_code = 0 then loop ~total:(total + count) (n * 2) else error_code
    end
    else 0
  in
  loop ~total:0 32
