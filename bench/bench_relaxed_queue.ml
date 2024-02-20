open Multicore_bench
module Queue = Saturn.Relaxed_queue
module Spin = Queue.Spin
module Not_lockfree = Queue.Not_lockfree
module CAS_interface = Queue.Not_lockfree.CAS_interface

let run_one ~budgetf ~n_adders ~n_takers ?(n_msgs = 50 * Util.iter_factor)
    ?(api = `Spin) () =
  let n_domains = n_adders + n_takers in

  let t = Queue.create ~size_exponent:10 () in

  let n_msgs_to_take = Atomic.make 0 |> Multicore_magic.copy_as_padded in
  let n_msgs_to_add = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    assert (Not_lockfree.pop t == None);
    Atomic.set n_msgs_to_take n_msgs;
    Atomic.set n_msgs_to_add n_msgs
  in
  let work i () =
    if i < n_adders then
      let rec work () =
        let n = Util.alloc n_msgs_to_add in
        if n <> 0 then begin
          match api with
          | `Spin ->
              for i = 1 to n do
                Spin.push t i
              done;
              work ()
          | `Not_lockfree ->
              let rec loop n =
                if 0 < n then
                  if Not_lockfree.push t i then loop (n - 1)
                  else begin
                    Domain.cpu_relax ();
                    loop n
                  end
                else work ()
              in
              loop n
          | `CAS_interface ->
              let rec loop n =
                if 0 < n then
                  if CAS_interface.push t i then loop (n - 1)
                  else begin
                    Domain.cpu_relax ();
                    loop n
                  end
                else work ()
              in
              loop n
        end
      in
      work ()
    else
      let rec work () =
        let n = Util.alloc n_msgs_to_take in
        if n <> 0 then
          match api with
          | `Spin ->
              for _ = 1 to n do
                Spin.pop t |> ignore
              done;
              work ()
          | `Not_lockfree ->
              let rec loop n =
                if 0 < n then begin
                  match Not_lockfree.pop t with
                  | None ->
                      Domain.cpu_relax ();
                      loop n
                  | Some _ -> loop (n - 1)
                end
                else work ()
              in
              loop n
          | `CAS_interface ->
              let rec loop n =
                if 0 < n then begin
                  match CAS_interface.pop t with
                  | None ->
                      Domain.cpu_relax ();
                      loop n
                  | Some _ -> loop (n - 1)
                end
                else work ()
              in
              loop n
      in
      work ()
  in

  let config =
    let plural role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s (%s)" (plural "adder" n_adders)
      (plural "taker" n_takers)
      (match api with
      | `Spin -> "spin"
      | `Not_lockfree -> "not lf"
      | `CAS_interface -> "cas")
  in

  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  Util.cross
    [ `Spin; `Not_lockfree; `CAS_interface ]
    (Util.cross [ 1; 2 ] [ 1; 2 ])
  |> List.concat_map @@ fun (api, (n_adders, n_takers)) ->
     run_one ~budgetf ~n_adders ~n_takers ~api ()
