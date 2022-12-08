module Atomic = Dscheck.TracedAtomic
open Lockfree

let drain queue =
  let remaining = ref 0 in
  while not (Michael_scott_queue.is_empty queue) do
    remaining := !remaining + 1;
    assert (Option.is_some (Michael_scott_queue.pop queue))
  done;
  !remaining

let producer_consumer () =
  Atomic.trace (fun () ->
      let queue = Michael_scott_queue.create () in
      let items_total = 4 in

      (* producer *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            Michael_scott_queue.push queue i
          done);

      (* consumer *)
      let popped = ref 0 in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            match Michael_scott_queue.pop queue with
            | None -> ()
            | Some v ->
                assert (v == !popped + 1);
                popped := !popped + 1
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              !popped + remaining = items_total)))

let two_producers () =
  Atomic.trace (fun () ->
      let queue = Michael_scott_queue.create () in
      let items_total = 4 in

      (* producers *)
      for _ = 1 to 2 do
        Atomic.spawn (fun () ->
            for _ = 1 to items_total / 2 do
              Michael_scott_queue.push queue 0
            done)
      done;

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              remaining = items_total)))

let two_domains () =
  Atomic.trace (fun () ->
      let stack = Michael_scott_queue.create () in
      let items_by_domain = 2 in

      (* two producers *)
      let lists =
        [
          (List.init items_by_domain (fun i -> i), ref []);
          (List.init items_by_domain (fun i -> i + items_by_domain), ref []);
        ]
      in
      List.iter
        (fun (lpush, lpop) ->
          Atomic.spawn (fun () ->
              List.iter
                (fun elt ->
                  (* even nums belong to thr 1, odd nums to thr 2 *)
                  Michael_scott_queue.push stack elt;
                  lpop := Option.get (Michael_scott_queue.pop stack) :: !lpop)
                lpush)
          |> ignore)
        lists;

      (* checks*)
      Atomic.final (fun () ->
          let lpop1 = !(List.nth lists 0 |> snd) in
          let lpop2 = !(List.nth lists 1 |> snd) in

          (* got the same number of items out as in *)
          Atomic.check (fun () -> items_by_domain = List.length lpop1);
          Atomic.check (fun () -> items_by_domain = List.length lpop2);

          (* no element are missing *)
          Atomic.check (fun () ->
              let l1 = List.filter (fun i -> i < items_by_domain) lpop1 in
              let l2 = List.filter (fun i -> i >= items_by_domain) lpop1 in
              let l3 = List.filter (fun i -> i < items_by_domain) lpop2 in
              let l4 = List.filter (fun i -> i >= items_by_domain) lpop2 in
              let is_sorted l = List.sort (fun a b -> -compare a b) l = l in
              is_sorted l1 && is_sorted l2 && is_sorted l3 && is_sorted l4)))

let two_domains_more_pop () =
  Atomic.trace (fun () ->
      let stack = Michael_scott_queue.create () in
      let items_by_domain = 2 in

      (* two producers *)
      let lists =
        [
          (List.init items_by_domain (fun i -> i), ref []);
          (List.init items_by_domain (fun i -> i + items_by_domain), ref []);
        ]
      in
      List.iter
        (fun (lpush, lpop) ->
          Atomic.spawn (fun () ->
              List.iter
                (fun elt ->
                  Michael_scott_queue.push stack elt;
                  lpop := Michael_scott_queue.pop stack :: !lpop;
                  lpop := Michael_scott_queue.pop stack :: !lpop)
                lpush)
          |> ignore)
        lists;

      (* checks*)
      Atomic.final (fun () ->
          let lpop1 =
            !(List.nth lists 0 |> snd)
            |> List.filter Option.is_some |> List.map Option.get
          in
          let lpop2 =
            !(List.nth lists 1 |> snd)
            |> List.filter Option.is_some |> List.map Option.get
          in

          (* got the same number of items out as in *)
          Atomic.check (fun () ->
              2 * items_by_domain = List.length lpop1 + List.length lpop2);

          (* no element are missing *)
          Atomic.check (fun () ->
              List.sort Int.compare (lpop1 @ lpop2)
              = List.init (items_by_domain * 2) (fun i -> i))))

let () =
  let open Alcotest in
  run "michael_scott_queue_dscheck"
    [
      ( "basic",
        [
          test_case "1-producer-1-consumer" `Slow producer_consumer;
          test_case "2-producers" `Slow two_producers;
          test_case "2-domains" `Slow two_domains;
          test_case "2-domains-more-pops" `Slow two_domains_more_pop;
        ] );
    ]
