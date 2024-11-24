module Atomic = Dscheck.TracedAtomic

module Dscheck_ms_queue (Queue : Michael_scott_queue_intf.MS_QUEUE) = struct
  let drain cue =
    let rec pop_until_empty acc =
      match Queue.pop_opt cue with
      | None -> acc |> List.rev
      | Some v -> pop_until_empty (v :: acc)
    in
    pop_until_empty []

  let push_pop () =
    Atomic.trace (fun () ->
        let queue = Queue.create () in
        let items_total = 4 in

        (* producer *)
        Atomic.spawn (fun () ->
            for i = 1 to items_total do
              Queue.push queue i
            done);

        (* consumer *)
        let popped = ref [] in
        Atomic.spawn (fun () ->
            for _ = 1 to items_total do
              begin
                match Queue.pop_opt queue with
                | None -> ()
                | Some v -> popped := v :: !popped
              end;
              (* Ensure is_empty does not interfere with other functions *)
              Queue.is_empty queue |> ignore
            done);

        (* checks*)
        Atomic.final (fun () ->
            Atomic.check (fun () ->
                let remaining = drain queue in
                let pushed = List.init items_total (fun x -> x + 1) in
                List.sort Int.compare (!popped @ remaining) = pushed)))

  let is_empty () =
    Atomic.trace (fun () ->
        let queue = Queue.create () in

        (* producer *)
        Atomic.spawn (fun () -> Queue.push queue 1);

        (* consumer *)
        let res = ref false in
        Atomic.spawn (fun () ->
            match Queue.pop_opt queue with
            | None -> res := true
            | Some _ -> res := Queue.is_empty queue);

        (* checks*)
        Atomic.final (fun () -> Atomic.check (fun () -> !res)))

  let push_drop () =
    Atomic.trace (fun () ->
        let queue = Queue.create () in
        let items_total = 4 in

        (* producer *)
        Atomic.spawn (fun () ->
            for i = 1 to items_total do
              Queue.push queue i
            done);

        (* consumer *)
        let dropped = ref 0 in
        Atomic.spawn (fun () ->
            for _ = 1 to items_total do
              match Queue.drop_exn queue with
              | () -> dropped := !dropped + 1
              | exception Queue.Empty -> ()
            done);

        (* checks*)
        Atomic.final (fun () ->
            Atomic.check (fun () ->
                let remaining = drain queue in
                remaining
                = List.init (items_total - !dropped) (fun x -> x + !dropped + 1))))

  let push_push () =
    Atomic.trace (fun () ->
        let queue = Queue.create () in
        let items_total = 6 in

        (* two producers *)
        for i = 0 to 1 do
          Atomic.spawn (fun () ->
              for j = 1 to items_total / 2 do
                (* even nums belong to thr 1, odd nums to thr 2 *)
                Queue.push queue (i + (j * 2))
              done)
        done;

        (* checks*)
        Atomic.final (fun () ->
            let items = drain queue in

            (* got the same number of items out as in *)
            Atomic.check (fun () -> items_total = List.length items);

            (* they are in fifo order *)
            let odd, even = List.partition (fun v -> v mod 2 == 0) items in

            Atomic.check (fun () -> List.sort Int.compare odd = odd);
            Atomic.check (fun () -> List.sort Int.compare even = even)))

  let push_pop_of_list () =
    Atomic.trace (fun () ->
        let items_total = 4 in
        let pushed = List.init items_total (fun x -> x + 1) in
        let cue = Queue.of_list pushed in

        Atomic.spawn (fun () -> Queue.push cue 42);

        (* consumer *)
        let popped = ref [] in
        Atomic.spawn (fun () ->
            for _ = 1 to items_total do
              begin
                match Queue.pop_opt cue with
                | None -> ()
                | Some v -> popped := v :: !popped
              end
            done);

        (* checks*)
        Atomic.final (fun () ->
            Atomic.check (fun () ->
                let remaining = drain cue in
                let pushed = pushed @ [ 42 ] in
                List.sort Int.compare (!popped @ remaining) = pushed)))

  let pop_pop () =
    Atomic.trace (fun () ->
        let items_total = 4 in
        let queue = Queue.of_list (List.init items_total (fun x -> x + 1)) in

        (* two consumers *)
        let lists = [ ref []; ref [] ] in
        List.iter
          (fun list ->
            Atomic.spawn (fun () ->
                for _ = 1 to items_total / 2 do
                  (* even nums belong to thr 1, odd nums to thr 2 *)
                  list := Option.get (Queue.pop_opt queue) :: !list
                done)
            |> ignore)
          lists;

        (* checks*)
        Atomic.final (fun () ->
            let l1 = !(List.nth lists 0) in
            let l2 = !(List.nth lists 1) in

            (* got the same number of items out as in *)
            Atomic.check (fun () ->
                items_total = List.length l1 + List.length l2);

            (* they are in fifo order *)
            Atomic.check (fun () -> List.sort Int.compare l1 = List.rev l1);
            Atomic.check (fun () -> List.sort Int.compare l2 = List.rev l2)))

  let two_domains () =
    Atomic.trace (fun () ->
        let stack = Queue.create () in
        let n1, n2 = (2, 1) in

        (* two producers *)
        let lists =
          [
            (List.init n1 (fun i -> i), ref []);
            (List.init n2 (fun i -> i + n1), ref []);
          ]
        in
        List.iter
          (fun (lpush, lpop) ->
            Atomic.spawn (fun () ->
                List.iter
                  (fun elt ->
                    (* even nums belong to thr 1, odd nums to thr 2 *)
                    Queue.push stack elt;
                    lpop := Option.get (Queue.pop_opt stack) :: !lpop)
                  lpush)
            |> ignore)
          lists;

        (* checks*)
        Atomic.final (fun () ->
            let lpop1 = !(List.nth lists 0 |> snd) in
            let lpop2 = !(List.nth lists 1 |> snd) in

            (* got the same number of items out as in *)
            Atomic.check (fun () -> List.length lpop1 = n1);
            Atomic.check (fun () -> List.length lpop2 = n2);

            (* no element are missing *)
            Atomic.check (fun () ->
                let l1 = List.filter (fun i -> i < n1) lpop1 in
                let l2 = List.filter (fun i -> i >= n1) lpop1 in
                let l3 = List.filter (fun i -> i < n2) lpop2 in
                let l4 = List.filter (fun i -> i >= n2) lpop2 in
                let is_sorted l = List.sort (fun a b -> -compare a b) l = l in
                is_sorted l1 && is_sorted l2 && is_sorted l3 && is_sorted l4)))

  let two_domains_more_pop () =
    Atomic.trace (fun () ->
        let queue = Queue.create () in
        let n1, n2 = (2, 1) in

        (* two producers *)
        let lists =
          [
            (List.init n1 (fun i -> i), ref []);
            (List.init n2 (fun i -> i + n1), ref []);
          ]
        in
        List.iter
          (fun (lpush, lpop) ->
            Atomic.spawn (fun () ->
                List.iter
                  (fun elt ->
                    Queue.push queue elt;
                    lpop := Queue.pop_opt queue :: !lpop;
                    lpop := Queue.pop_opt queue :: !lpop)
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
                n1 + n2 = List.length lpop1 + List.length lpop2);

            (* no element are missing *)
            Atomic.check (fun () ->
                List.sort Int.compare (lpop1 @ lpop2)
                = List.init (n1 + n2) (fun i -> i))))

  let tests name =
    let open Alcotest in
    [
      ( "basic_" ^ name,
        [
          test_case "1-producer-1-consumer" `Slow push_pop;
          test_case "2-domains-is_empty" `Slow is_empty;
          test_case "1-push-1-drop" `Slow push_drop;
          test_case "1-push-1-pop-of_list" `Slow push_pop_of_list;
          test_case "2-producers" `Slow push_push;
          test_case "2-consumers" `Slow pop_pop;
          test_case "2-domains" `Slow two_domains;
          test_case "2-domains-more-pops" `Slow two_domains_more_pop;
        ] );
    ]
end

let () =
  let module Safe = Dscheck_ms_queue (Michael_scott_queue) in
  let safe_test = Safe.tests "safe" in
  let module Unsafe = Dscheck_ms_queue (Michael_scott_queue_unsafe) in
  let unsafe_test = Unsafe.tests "unsafe" in

  let open Alcotest in
  run "michael_scott_queue_dscheck" (safe_test @ unsafe_test)
