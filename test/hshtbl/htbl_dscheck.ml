module Htbl = Htbl.Htbl

let two_domains_add () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:2 in
      let item1 = [ 0; 1; 2 ] in
      let item2 = [ 3; 4; 1 ] in

      Atomic.spawn (fun () ->
          List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) item1);

      Atomic.spawn (fun () ->
          List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) item2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              List.for_all
                (fun i ->
                  match Htbl.find i htbl with None -> false | Some j -> i = j)
                (item1 @ item2))))

let two_domains_replace () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:2 in
      let item1 = [ (0, "a"); (1, "b"); (2, "c") ] in
      let item2 = [ (0, "d"); (4, "e"); (1, "f") ] in
      let append = List.sort_uniq (fun (k, _) (k', _) -> compare k k') (item1 @ item2) in

      Atomic.spawn (fun () ->
          List.iter (fun (k, v) -> Htbl.replace k v htbl) item1);

      Atomic.spawn (fun () ->
          List.iter (fun (k, v) -> Htbl.replace k v htbl) item2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              List.for_all
                (fun (k, _) ->
                  match Htbl.find k htbl with
                  | None -> false
                  | Some v -> (
                      match
                        (List.assoc_opt k item1, List.assoc_opt k item2)
                      with
                      | None, Some v' | Some v', None -> v = v'
                      | Some v1, Some v2 -> v = v1 || v = v2
                      | None, None -> false))
              append)))

let two_domains_remove () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:2 in
      let items = [ 0; 1; 2; 3 ] in
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

let two_domains_remove_bis () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:2 in
      let items = [ 0; 1; 2; 3 ] in
      let to_remove = [ 0; 0; 1; 1 ] in
      let res1 = ref [] in
      let res2 = ref [] in

      (* Initialization*)
      List.iter (fun elt -> Htbl.add elt elt htbl |> ignore) items;

      let remove_all items = List.map (fun elt -> Htbl.remove elt htbl) items in

      Atomic.spawn (fun () -> res1 := remove_all to_remove);
      Atomic.spawn (fun () -> res2 := remove_all to_remove);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              List.for_all (fun elt -> Htbl.mem elt htbl |> not) to_remove);

          Atomic.check (fun () ->
              match (!res1, !res2) with
              | [ true; false; true; false ], [ false; false; false; false ]
              | [ false; false; false; false ], [ true; false; true; false ]
              | [ true; false; false; false ], [ false; false; true; false ]
              | [ false; false; true; false ], [ true; false; false; false ] ->
                  true
              | _ -> false)))

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
  run "hshtbl_dscheck"
    [
      ( "basic",
        [
          test_case "2-domains_add" `Slow two_domains_add;
          test_case "2-domains_replace" `Slow two_domains_replace;
          test_case "2-domains_remove" `Slow two_domains_remove;
          test_case "2-domains_remove_bis" `Slow two_domains_remove_bis;
          test_case "2-domains_add_remove" `Slow two_domains_add_remove;
        ] );
    ]
