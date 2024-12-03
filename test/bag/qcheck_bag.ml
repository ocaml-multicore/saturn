module Bag = Saturn.Bag

let pop_all bag =
  let rec loop acc =
    match Bag.pop_opt bag with Some x -> loop (x :: acc) | None -> acc
  in
  loop []

let tests =
  QCheck.
    [
      Test.make ~name:"sequential"
        (pair small_nat (list int))
        (fun (npop, lpush) ->
          let bag = Bag.create () in
          List.iter (Bag.push bag) lpush;
          let popped = List.init npop (fun _ -> Bag.pop_opt bag) in
          let popped =
            List.filter Option.is_some popped |> List.map Option.get
          in

          let remaining = pop_all bag in

          List.for_all (fun x -> List.mem x lpush) popped
          && List.for_all (fun x -> List.mem x lpush) remaining
          && List.sort Int.compare (popped @ remaining)
             = List.sort Int.compare lpush);
      Test.make ~name:"parallel"
        (pair small_nat (list int))
        (fun (npop, lpush) ->
          let bag = Bag.create () in
          let barrier = Barrier.create 2 in

          let domain1 =
            Domain.spawn @@ fun () ->
            Barrier.await barrier;
            List.iter
              (fun elt ->
                Bag.push bag elt;
                Domain.cpu_relax ())
              lpush
          in
          let domain2 =
            Domain.spawn @@ fun () ->
            Barrier.await barrier;
            List.init npop (fun _ ->
                Domain.cpu_relax ();
                Bag.pop_opt bag)
          in

          let () = Domain.join domain1 in
          let popped = Domain.join domain2 in

          let popped =
            List.filter Option.is_some popped |> List.map Option.get
          in

          let remaining = pop_all bag in

          List.for_all (fun x -> List.mem x lpush) popped
          && List.for_all (fun x -> List.mem x lpush) remaining
          && List.sort Int.compare (popped @ remaining)
             = List.sort Int.compare lpush);
    ]

let () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "QCheck Bag" [ ("test_sequential", to_alcotest tests) ]
