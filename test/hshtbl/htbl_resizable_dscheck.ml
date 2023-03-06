module Htbl = Htbl_resizable.Htbl_resizable

let two_domains_add () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:2 in
      let item1 = [ 0; 1 ] in
      let item2 = [ 3; 4; 1 ] in

      Atomic.spawn (fun () ->
          List.iter (fun elt -> Htbl.add_no_resize elt elt htbl |> ignore) item1);

      Atomic.spawn (fun () ->
          List.iter (fun elt -> Htbl.add_no_resize elt elt htbl |> ignore) item2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              List.for_all (fun i -> Htbl.mem i htbl) (item1 @ item2))))

let two_domains_add_resize () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:1 in
      let item_seq = [ 6; 7 ] in
      let item1 = [ 0; 1; 2 ] in
      let item2 = [ 3; 4; 5 ] in

      List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) item_seq;

      Atomic.spawn (fun () ->
          List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) item1);

      Atomic.spawn (fun () ->
          List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) item2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              List.for_all (fun i -> Htbl.mem i htbl) (item_seq @ item1 @ item2))))

let two_domains_remove () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:2 in
      let items = [ 0; 1; 2 ] in
      let res1 = ref [] in
      let res2 = ref [] in

      (* Initialization*)
      List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) items;

      let remove_all items = List.map (fun elt -> Htbl.remove elt htbl) items in

      Atomic.spawn (fun () -> res1 := remove_all items);
      Atomic.spawn (fun () -> res2 := remove_all items);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              List.for_all (fun elt -> Htbl.mem elt htbl |> not) items);

          Atomic.check (fun () ->
              List.for_all2
                (fun removed_by_1 removed_by_2 ->
                  (removed_by_1 && not removed_by_2)
                  || (removed_by_2 && not removed_by_1))
                !res1 !res2)))

let two_domains_add_remove () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:3 in
      let items_seq = [ 0; 1; 2 ] in
      let to_add = [ 4; 5; 6 ] in
      let to_remove = [ 5; 0; 4; 10 ] in

      (* Sequential adds *)
      List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) items_seq;

      let removed = ref [] in
      let added = ref [] in
      Atomic.spawn (fun () ->
          removed := List.map (fun elt -> (elt, Htbl.remove elt htbl)) to_remove);

      Atomic.spawn (fun () ->
          added := List.map (fun elt -> Htbl.add elt elt htbl) to_add);

      Atomic.final (fun () ->
          Atomic.check (fun () -> List.for_all (fun elt -> elt) !added);
          Atomic.check (fun () ->
              List.for_all
                (fun key ->
                  if List.mem key to_remove then
                    let is_removed = List.assoc key !removed in
                    is_removed = not @@ Htbl.mem key htbl
                  else Htbl.mem key htbl)
                (items_seq @ to_add))))

let () =
  let open Alcotest in
  run "hshtbl_resizable_dscheck"
    [
      ( "basic",
        [
          test_case "2-domains_add" `Slow two_domains_add;
          test_case "2-domains_add_resize" `Slow two_domains_add_resize;
          test_case "2-domains_remove" `Slow two_domains_remove;
          test_case "2-domains_add_remove" `Slow two_domains_add_remove;
        ] );
    ]
