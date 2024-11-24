[@@@warning "-32"]

module Atomic = Dscheck.TracedAtomic
module Cue = Bounded_queue

(* Dscheck only tests the safe implementation of Bounded_queue. To make Bounded_queue_unsafe compatible with Dscheck, it needs to be modified to essentially become the safe version. *)

let drain cue =
  let rec pop_until_empty acc =
    match Cue.pop_opt cue with
    | None -> acc |> List.rev
    | Some v -> pop_until_empty (v :: acc)
  in
  pop_until_empty []

let push_pop () =
  Atomic.trace (fun () ->
      let cue = Cue.create () in
      let items_total = 4 in

      (* producer *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            Cue.try_push cue i |> ignore
          done);

      (* consumer *)
      let popped = ref [] in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            begin
              match Cue.pop_opt cue with
              | None -> ()
              | Some v -> popped := v :: !popped
            end;
            (* Ensure is_empty does not interfere with other functions *)
            Cue.is_empty cue |> ignore
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain cue in
              let pushed = List.init items_total (fun x -> x + 1) in
              List.sort Int.compare (!popped @ remaining) = pushed)))

let is_empty () =
  Atomic.trace (fun () ->
      let cue = Cue.create () in

      (* producer *)
      Atomic.spawn (fun () -> Cue.try_push cue 1 |> ignore);

      (* consumer *)
      let res = ref false in
      Atomic.spawn (fun () ->
          match Cue.pop_opt cue with
          | None -> res := true
          | Some _ -> res := Cue.is_empty cue);

      (* checks*)
      Atomic.final (fun () -> Atomic.check (fun () -> !res)))

let push_length_is_full () =
  Atomic.trace (fun () ->
      let cue = Cue.create () in
      let items_total = 4 in

      Cue.try_push cue 0 |> ignore;

      (* producer *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            Cue.try_push cue i |> ignore
          done);

      (* consumer *)
      let length_res = ref [] in
      let is_full_res = ref false in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            length_res := Cue.length cue :: !length_res;
            is_full_res := Cue.is_full cue || !is_full_res
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () -> not !is_full_res);

          Atomic.check (fun () ->
              let pushed = drain cue in
              pushed = List.init (items_total + 1) Fun.id);

          Atomic.check (fun () ->
              !length_res |> List.rev = List.sort compare !length_res
              && List.for_all
                   (fun x -> x >= 1 && x <= items_total + 1)
                   !length_res)))

let push_length_is_full_with_capacity () =
  Atomic.trace (fun () ->
      let capacity = 3 in
      let cue = Cue.create ~capacity () in
      let items_total = 5 in

      Cue.try_push cue 0 |> ignore;

      (* producer *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total - 1 do
            Cue.try_push cue i |> ignore
          done);

      (* consumer *)
      let length_res = ref [] in
      let test = ref true in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            let len = Cue.length cue in
            length_res := len :: !length_res;
            test := if len < capacity then !test else !test && Cue.is_full cue
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () -> !test);

          Atomic.check (fun () ->
              let pushed = drain cue in
              pushed = List.init capacity Fun.id);

          Atomic.check (fun () ->
              !length_res |> List.rev = List.sort compare !length_res
              && List.for_all (fun x -> x >= 1 && x <= capacity) !length_res)))

let push_drop () =
  Atomic.trace (fun () ->
      let cue = Cue.create () in
      let items_total = 4 in

      (* producer *)
      Atomic.spawn (fun () ->
          for i = 1 to items_total do
            Cue.try_push cue i |> ignore
          done);

      (* consumer *)
      let dropped = ref 0 in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            match Cue.drop_exn cue with
            | () -> dropped := !dropped + 1
            | exception Cue.Empty -> ()
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain cue in
              remaining
              = List.init (items_total - !dropped) (fun x -> x + !dropped + 1))))

let push_pop_with_capacity () =
  Atomic.trace (fun () ->
      let cue = Cue.create ~capacity:2 () in
      let items_total = 4 in

      (* producer *)
      let pushed = Array.make items_total false in
      Atomic.spawn (fun () ->
          Array.iteri (fun i _ -> pushed.(i) <- Cue.try_push cue i) pushed);

      (* consumer *)
      let popped = Array.make items_total None in
      Atomic.spawn (fun () ->
          Array.iteri (fun i _ -> popped.(i) <- Cue.pop_opt cue) popped);
      (* checks*)
      Atomic.final (fun () ->
          let popped = Array.to_list popped |> List.filter_map Fun.id in
          let remaining = drain cue in
          Atomic.check (fun () ->
              let xor a b = (a && not b) || ((not a) && b) in
              try
                Array.iteri
                  (fun i elt ->
                    if elt then begin
                      if not @@ xor (List.mem i remaining) (List.mem i popped)
                      then raise Exit
                    end
                    else if List.mem i remaining || List.mem i popped then
                      raise Exit)
                  pushed;
                true
              with _ -> false)))

let push_push () =
  Atomic.trace (fun () ->
      let cue = Cue.create () in
      let items_total = 6 in

      (* two producers *)
      for i = 0 to 1 do
        Atomic.spawn (fun () ->
            for j = 1 to items_total / 2 do
              (* even nums belong to thr 1, odd nums to thr 2 *)
              Cue.try_push cue (i + (j * 2)) |> ignore
            done)
      done;

      (* checks*)
      Atomic.final (fun () ->
          let items = drain cue in

          (* got the same number of items out as in *)
          Atomic.check (fun () -> items_total = List.length items);

          (* they are in fifo order *)
          let odd, even = List.partition (fun v -> v mod 2 == 0) items in

          Atomic.check (fun () -> List.sort Int.compare odd = odd);
          Atomic.check (fun () -> List.sort Int.compare even = even)))

let push_push_with_capacity () =
  Atomic.trace (fun () ->
      let capacity = 3 in
      let cue = Cue.create ~capacity () in
      let items_total = 6 in

      (* two producers *)
      for i = 0 to 1 do
        Atomic.spawn (fun () ->
            for j = 1 to items_total / 2 do
              (* even nums belong to thr 1, odd nums to thr 2 *)
              Cue.try_push cue (i + (j * 2)) |> ignore
            done)
      done;

      (* checks*)
      Atomic.final (fun () ->
          let items = drain cue in

          (* got the same number of items out as in *)
          Atomic.check (fun () -> capacity = List.length items)))

let push_pop_of_list () =
  Atomic.trace (fun () ->
      let items_total = 4 in
      let pushed = List.init items_total (fun x -> x + 1) in
      let cue = Cue.of_list_exn pushed in

      Atomic.spawn (fun () -> Cue.try_push cue 42 |> ignore);

      (* consumer *)
      let popped = ref [] in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            begin
              match Cue.pop_opt cue with
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
      let cue = Cue.of_list_exn (List.init items_total (fun x -> x + 1)) in

      (* two consumers *)
      let lists = [ ref []; ref [] ] in
      List.iter
        (fun list ->
          Atomic.spawn (fun () ->
              for _ = 1 to items_total / 2 do
                (* even nums belong to thr 1, odd nums to thr 2 *)
                list := Option.get (Cue.pop_opt cue) :: !list
              done)
          |> ignore)
        lists;

      (* checks*)
      Atomic.final (fun () ->
          let l1 = !(List.nth lists 0) in
          let l2 = !(List.nth lists 1) in

          (* got the same number of items out as in *)
          Atomic.check (fun () -> items_total = List.length l1 + List.length l2);

          (* they are in fifo order *)
          Atomic.check (fun () -> List.sort Int.compare l1 = List.rev l1);
          Atomic.check (fun () -> List.sort Int.compare l2 = List.rev l2)))

let two_domains () =
  Atomic.trace (fun () ->
      let cue = Cue.create () in
      let n1, n2 = (1, 2) in

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
                  Cue.try_push cue elt |> ignore;
                  lpop := Option.get (Cue.pop_opt cue) :: !lpop)
                lpush)
          |> ignore)
        lists;

      (* checks*)
      Atomic.final (fun () ->
          let lpop1 = !(List.nth lists 0 |> snd) in
          let lpop2 = !(List.nth lists 1 |> snd) in

          (* got the same number of items out as in *)
          Atomic.check (fun () -> List.length lpop1 = 1);
          Atomic.check (fun () -> List.length lpop2 = 2);

          (* no element are missing *)
          Atomic.check (fun () ->
              List.sort Int.compare (lpop1 @ lpop2)
              = List.init (n1 + n2) (fun i -> i))))

let two_domains_more_pop () =
  Atomic.trace (fun () ->
      let cue = Cue.create () in
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
                  Cue.try_push cue elt |> ignore;
                  lpop := Cue.pop_opt cue :: !lpop;
                  lpop := Cue.pop_opt cue :: !lpop)
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

let tests =
  let open Alcotest in
  [
    ( "basic",
      [
        test_case "1-producer-1-consumer" `Slow push_pop;
        test_case "push-length-is_full" `Slow push_length_is_full;
        test_case "push-length-is_full-capacity" `Slow
          push_length_is_full_with_capacity;
        test_case "2-domains-is_empty" `Slow is_empty;
        test_case "1-producer-1-consumer-capacity" `Slow push_pop_with_capacity;
        test_case "1-push-1-drop" `Slow push_drop;
        test_case "1-push-1-pop-of_list" `Slow push_pop_of_list;
        test_case "2-producers" `Slow push_push;
        test_case "2-producers-capacity" `Slow push_push_with_capacity;
        test_case "2-consumers" `Slow pop_pop;
        test_case "2-domains" `Slow two_domains;
        test_case "2-domains-more-pops" `Slow two_domains_more_pop;
      ] );
  ]

let () =
  let open Alcotest in
  run "dscheck_bounded_queue" tests
