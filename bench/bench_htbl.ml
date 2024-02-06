open Multicore_bench
module Htbl = Saturn_lockfree__.Lf_htbl

let run_one ~budgetf ~n_domains ?(n_ops = 20 * Util.iter_factor)
    ?(n_keys = 10000) ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) () =
  let percent_rem = 100 - (percent_mem + percent_add) in

  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Htbl.create ~size_exponent:16 in
  if prepopulate then
    for _ = 1 to n_keys do
      let value = Random.bits () in
      let key = value mod n_keys in
      Htbl.add t key value
    done;

  let n_ops = (100 + percent_mem) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    Atomic.set n_ops_todo n_ops;
    Random.State.make_self_init ()
  in
  let work _ state =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let value = Random.State.bits state in
            let op = (value asr 20) mod 100 in
            let key = value mod n_keys in
            if op < limit_mem then begin
              Htbl.mem t key |> ignore;
              loop (n - 1)
            end
            else if op < limit_add then begin
              Htbl.add t key value |> ignore;
              loop (n - 1)
            end
            else begin
              Htbl.try_remove t key |> ignore;
              loop (n - 1)
            end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d workers, %d%% mem %d%% add %d%% rem" n_domains
      percent_mem percent_add percent_rem
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Util.thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 10; 50; 90 ] [ 1; 2; 4 ]
  |> List.concat_map @@ fun (percent_mem, n_domains) ->
     run_one ~budgetf ~n_domains ~percent_mem ()
