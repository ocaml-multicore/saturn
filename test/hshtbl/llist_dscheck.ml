module Llist = Llist.Llist

let two_domains_remove_add () =
  Atomic.trace (fun () ->
      let l = Llist.init () in
      let item = [ 0; 1; 3; 4 ] in

      List.iter (fun elt -> Llist.add elt Llist.Dummy l |> ignore) item;

      Atomic.spawn (fun () ->
          Llist.remove 1 l |> ignore;
          Llist.remove 3 l |> ignore);

      Atomic.spawn (fun () -> Llist.add 2 Llist.Dummy l |> ignore);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              Llist.mem 0 l && Llist.mem 2 l && Llist.mem 4 l)))

let two_domains_remove_same () =
  Atomic.trace (fun () ->
      let l = Llist.init () in
      let item = [ 0; 1; 3; 4 ] in
      let removed1, removed2 = (ref false, ref false) in

      List.iter (fun elt -> Llist.add elt Llist.Dummy l |> ignore) item;

      Atomic.spawn (fun () -> removed1 := Llist.remove 3 l);

      Atomic.spawn (fun () -> removed2 := Llist.remove 3 l);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              List.for_all (fun elt -> Llist.mem elt l) [ 0; 1; 4 ]
              && (not @@ Llist.mem 3 l)
              && ((!removed1 && not !removed2) || (!removed2 && not !removed1)))))

let two_domains_add_remove_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let l = Llist.init () in
      let nelt = 3 in
      let item_seq = List.init nelt (fun i -> i * 2) in
      let item_par =
        (* all elements are only once in the list *)
        List.init nelt (fun _i -> Random.int (nelt * 3))
        |> List.fold_left
             (fun acc elt -> if List.mem elt acc then acc else elt :: acc)
             []
      in
      let added = ref [] in
      let removed = ref [] in

      List.iter (fun elt -> Llist.add elt Llist.Dummy l |> ignore) item_seq;

      Atomic.spawn (fun () ->
          List.iter
            (fun elt -> added := Llist.add elt Llist.Dummy l :: !added)
            item_par);

      Atomic.spawn (fun () ->
          List.iter
            (fun elt -> removed := Llist.remove elt l :: !removed)
            item_par);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let res = List.combine !added !removed |> List.rev in
              List.for_all2
                (fun elt (added, removed) ->
                  match (List.mem elt item_seq, added, removed) with
                  | true, false, true | false, false, false | false, true, true
                    ->
                      not @@ Llist.mem elt l
                  | false, true, false -> Llist.mem elt l
                  | true, true, true -> true
                  | _, _, _ -> false)
                item_par res)))

let () =
  let open Alcotest in
  run "hshtbl_dscheck"
    [
      ( "basic",
        [
          test_case "2-domains_remove_add" `Slow two_domains_remove_add;
          test_case "2-domains_remove_same" `Slow two_domains_remove_same;
          test_case "2-domains_add_remove_same" `Slow
            two_domains_add_remove_same;
        ] );
    ]
