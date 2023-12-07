let try_add ll elt = Llist.try_add ll elt ()

module Sint = Set.Make (struct
  type t = int

  let compare = compare
end)

let xor a b = match (a, b) with true, false | false, true -> true | _ -> false

(* [random_uniq_list n factor] generate a list of n unique elements that
   are in between 0 and (n*factor-1) *)
let random_uniq_list n factor =
  let max = factor * n in
  let rec loop () =
    let r =
      List.init (2 * n) (fun _i -> Random.int max)
      |> List.fold_left
           (fun acc elt -> if List.mem elt acc then acc else elt :: acc)
           []
    in
    if List.length r < n then loop () else List.filteri (fun i _ -> i < n) r
  in
  loop ()

let nitems = 8

let two_domains_remove_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let ll = Llist.create ~compare () in
      let nadd_seq = nitems in
      let factor = 3 in
      let items_add_seq = random_uniq_list nadd_seq factor in

      let npar = nadd_seq / 2 in
      let factor = 2 * factor in
      let items_remove_par = random_uniq_list npar factor in
      let items_add_par = random_uniq_list npar factor in

      (* Sequential add *)
      let added_seq =
        List.fold_left
          (fun acc elt -> if try_add ll elt then elt :: acc else acc)
          [] items_add_seq
      in

      (* Parallel remove *)
      let removed = ref [] in
      Atomic.spawn (fun () ->
          removed :=
            List.fold_left
              (fun acc elt ->
                if Llist.try_remove ll elt then elt :: acc else acc)
              [] items_remove_par);

      (* Paralled add *)
      let added_par = ref [] in
      Atomic.spawn (fun () ->
          added_par :=
            List.fold_left
              (fun acc elt -> if try_add ll elt then elt :: acc else acc)
              [] items_add_par);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let try_add_seq = Sint.of_list items_add_seq in
              let try_add_par = Sint.of_list items_add_par in
              let try_remove = Sint.of_list items_remove_par in
              let added_seq = Sint.of_list added_seq in
              let removed = Sint.of_list !removed in
              let added_par = Sint.of_list !added_par in
              Sint.for_all
                (fun elt ->
                  match
                    ( Sint.mem elt try_add_seq,
                      Sint.mem elt try_add_par,
                      Sint.mem elt try_remove )
                  with
                  | true, true, false ->
                      Llist.mem ll elt
                      && (not @@ Sint.mem elt added_par)
                      && Sint.mem elt added_seq
                  | true, false, false ->
                      Llist.mem ll elt && Sint.mem elt added_seq
                  | false, true, false ->
                      Llist.mem ll elt && Sint.mem elt added_par
                  | false, false, true ->
                      (not @@ Llist.mem ll elt) && (not @@ Sint.mem elt removed)
                  | true, false, true ->
                      (not @@ Llist.mem ll elt) && Sint.mem elt removed
                  | false, true, true ->
                      Sint.mem elt added_par
                      && xor (Llist.mem ll elt) (Sint.mem elt removed)
                  | true, true, true ->
                      Sint.mem elt removed
                      && (not @@ xor (Sint.mem elt added_par) (Llist.mem ll elt))
                  | _ -> false)
                (Sint.union try_add_seq (Sint.union try_add_par try_remove)))))

let two_domains_add_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let ll = Llist.create ~compare () in
      let nadd_seq = nitems in
      let factor = 3 in
      let items_add_seq = random_uniq_list nadd_seq factor in

      let npar = nadd_seq / 2 in
      let factor = 2 * factor in
      let items_add1_par = random_uniq_list npar factor in
      let items_add2_par = random_uniq_list npar factor in

      (* Sequential add *)
      let _added_seq =
        List.fold_left
          (fun acc elt -> if try_add ll elt then elt :: acc else acc)
          [] items_add_seq
      in

      (* Domain 1 : Parallel add  *)
      let added1 = ref [] in
      Atomic.spawn (fun () ->
          added1 :=
            List.fold_left
              (fun acc elt -> if try_add ll elt then elt :: acc else acc)
              [] items_add1_par);

      (* Domain 2 : Paralled add *)
      let added2 = ref [] in
      Atomic.spawn (fun () ->
          added2 :=
            List.fold_left
              (fun acc elt -> if try_add ll elt then elt :: acc else acc)
              [] items_add2_par);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let try_add_seq = Sint.of_list items_add_seq in
              let try_add1 = Sint.of_list items_add1_par in
              let try_add2 = Sint.of_list items_add2_par in
              let added1 = Sint.of_list !added1 in
              let added2 = Sint.of_list !added2 in
              Sint.for_all
                (fun elt ->
                  match
                    ( Sint.mem elt try_add_seq,
                      Sint.mem elt try_add1,
                      Sint.mem elt try_add2 )
                  with
                  | true, true, false -> not @@ Sint.mem elt added1
                  | true, false, true -> not @@ Sint.mem elt added2
                  | true, true, true ->
                      (not @@ Sint.mem elt added1)
                      && (not @@ Sint.mem elt added2)
                  | false, true, false -> Sint.mem elt added1
                  | false, false, true -> Sint.mem elt added2
                  | false, true, true ->
                      xor (Sint.mem elt added1) (Sint.mem elt added2)
                  | _ -> Llist.mem ll elt)
                (Sint.union try_add_seq (Sint.union try_add1 try_add2)))))

let two_domains_remove_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let ll = Llist.create ~compare () in
      let nadd_seq = 6 in
      let factor = 3 in
      let items_add_seq = random_uniq_list nadd_seq factor in

      let npar = nadd_seq / 2 in
      let factor = 2 * factor in
      let items_remove1_par = random_uniq_list npar factor in
      let items_remove2_par = random_uniq_list npar factor in

      (* Sequential add *)
      let _added_seq =
        List.fold_left
          (fun acc elt -> if try_add ll elt then elt :: acc else acc)
          [] items_add_seq
      in

      (* Domain 1 : Parallel remove  *)
      let removed1 = ref [] in
      Atomic.spawn (fun () ->
          removed1 :=
            List.fold_left
              (fun acc elt ->
                if Llist.try_remove ll elt then elt :: acc else acc)
              [] items_remove1_par);

      (* Domain 2 : Paralled remove *)
      let removed2 = ref [] in
      Atomic.spawn (fun () ->
          removed2 :=
            List.fold_left
              (fun acc elt ->
                if Llist.try_remove ll elt then elt :: acc else acc)
              [] items_remove2_par);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let try_add_seq = Sint.of_list items_add_seq in
              let try_remove1 = Sint.of_list items_remove1_par in
              let try_remove2 = Sint.of_list items_remove2_par in
              let removed1 = Sint.of_list !removed1 in
              let removed2 = Sint.of_list !removed2 in
              Sint.for_all
                (fun elt ->
                  match
                    ( Sint.mem elt try_add_seq,
                      Sint.mem elt try_remove1,
                      Sint.mem elt try_remove2 )
                  with
                  | true, false, false -> Llist.mem ll elt
                  | false, true, false -> not @@ Sint.mem elt removed1
                  | false, false, true -> not @@ Sint.mem elt removed2
                  | true, true, false ->
                      Sint.mem elt removed1 && (not @@ Llist.mem ll elt)
                  | true, false, true ->
                      Sint.mem elt removed2 && (not @@ Llist.mem ll elt)
                  | true, true, true ->
                      (not (Llist.mem ll elt))
                      && (Sint.mem elt removed1 || Sint.mem elt removed2)
                  | false, true, true ->
                      (not (Sint.mem elt removed1))
                      && not (Sint.mem elt removed2)
                  | _ -> false)
                (Sint.union try_add_seq (Sint.union try_remove1 try_remove2)))))

let () =
  let open Alcotest in
  Random.self_init ();
  run "llist_dscheck"
    [
      ( "basic",
        [
          test_case "2-domains_remove_add" `Slow two_domains_remove_add;
          test_case "2-domains_add_add" `Slow two_domains_add_add;
          test_case "2-domains_remove_remove" `Slow two_domains_remove_remove;
        ] );
    ]
