open Multicore_bench

module type BENCHQUEUE = sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val take_opt : 'a t -> 'a option
  val is_empty : 'a t -> bool
end

module LFQueue = struct
  type 'a t = 'a Saturn.Queue.t

  let create = Saturn.Queue.create
  let push elt q = Saturn.Queue.push q elt
  let take_opt = Saturn.Queue.pop_opt
  let is_empty = Saturn.Queue.is_empty
end

module RunOneDomain (Queue : BENCHQUEUE) = struct
  let run ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
    let t = Queue.create () in

    let op push =
      if push then Queue.push 101 t else Queue.take_opt t |> ignore
    in

    let init _ =
      assert (Queue.is_empty t);
      Util.generate_push_and_pop_sequence n_msgs
    in
    let work _ bits = Util.Bits.iter op bits in

    Times.record ~budgetf ~n_domains:1 ~init ~work ()
    |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message"
         ~config:"one domain"
end

module Queue = Saturn.Queue

let run_one ~budgetf ?(n_adders = 2) ?(n_takers = 2)
    ?(n_msgs = 50 * Util.iter_factor) () =
  let n_domains = n_adders + n_takers in

  let t = Queue.create () in

  let n_msgs_to_take = Atomic.make 0 |> Multicore_magic.copy_as_padded in
  let n_msgs_to_add = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    assert (Queue.is_empty t);
    Atomic.set n_msgs_to_take n_msgs;
    Atomic.set n_msgs_to_add n_msgs
  in
  let work i () =
    if i < n_adders then
      let rec work () =
        let n = Util.alloc n_msgs_to_add in
        if 0 < n then begin
          for i = 1 to n do
            Queue.push t i
          done;
          work ()
        end
      in
      work ()
    else
      let rec work () =
        let n = Util.alloc n_msgs_to_take in
        if n <> 0 then
          let rec loop n =
            if 0 < n then begin
              match Queue.pop_opt t with
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
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s"
      (format "nb adder" n_adders)
      (format "nb taker" n_takers)
  in

  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

module LFRunOneDomain = RunOneDomain (LFQueue)
module StdRunOneDomain = RunOneDomain (Stdlib.Queue)

let run_suite ~budgetf =
  LFRunOneDomain.run ~budgetf ()
  @ (Util.cross [ 1; 2 ] [ 1; 2 ]
    |> List.concat_map @@ fun (n_adders, n_takers) ->
       run_one ~budgetf ~n_adders ~n_takers ())

let run_suite_std ~budgetf = StdRunOneDomain.run ~budgetf ()
