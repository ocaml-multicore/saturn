module Sint = Set.Make (struct
  type t = int

  let compare = compare
end)

module Mint = Map.Make (struct
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

let random_list n factor =
  let max = factor * n in
  List.init (2 * n) (fun _i -> Random.int max)

let nitems = 7

let simple_two_domains_remove_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let ll = Llist.create ~compare () in

      (* Sequential add *)
      Llist.add ll 2 2 |> ignore;

      (* Parallel remove *)
      let removed1 = ref false in
      Atomic.spawn (fun () -> removed1 := Llist.try_remove ll 2);

      (* Paralled add *)
      let removed2 = ref false in
      Atomic.spawn (fun () ->
          Llist.add ll 2 3;
          removed2 := Llist.try_remove ll 2);

      Atomic.final (fun () -> Atomic.check (fun () -> !removed1 && !removed2)))

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
      List.iter (fun elt -> Llist.add ll elt 0) items_add_seq;

      (* Parallel remove *)
      let removed = ref [] in
      Atomic.spawn (fun () ->
          removed :=
            List.fold_left
              (fun acc elt ->
                if Llist.try_remove ll elt then elt :: acc else acc)
              [] items_remove_par);

      (* Paralled add *)
      Atomic.spawn (fun () ->
          List.iter (fun elt -> Llist.add ll elt 1) items_add_par);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let add_seq = Sint.of_list items_add_seq in
              let add_par = Sint.of_list items_add_par in
              let try_remove = Sint.of_list items_remove_par in
              let removed = Sint.of_list !removed in

              Sint.for_all
                (fun elt ->
                  match
                    ( Sint.mem elt add_seq,
                      Sint.mem elt add_par,
                      Sint.mem elt try_remove )
                  with
                  | true, true, false ->
                      Llist.mem ll elt && Llist.find_all ll elt = [ 1; 0 ]
                  | true, false, false ->
                      Llist.mem ll elt && Llist.find_all ll elt = [ 0 ]
                  | false, true, false ->
                      Llist.mem ll elt && Llist.find_all ll elt = [ 1 ]
                  | false, false, true ->
                      (not @@ Llist.mem ll elt) && (not @@ Sint.mem elt removed)
                  | true, false, true ->
                      (not @@ Llist.mem ll elt) && Sint.mem elt removed
                  | false, true, true ->
                      xor (Llist.mem ll elt) (Sint.mem elt removed)
                  | true, true, true ->
                      Sint.mem elt removed && Llist.mem ll elt
                      &&
                      let r = Llist.find_all ll elt in
                      r = [ 0 ] || r = [ 1 ]
                  | _ -> false)
                (Sint.union add_seq (Sint.union add_par try_remove)))))

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
      List.iter (fun elt -> Llist.add ll elt 0) items_add_seq;

      (* Domain 1 : Parallel add  *)
      Atomic.spawn (fun () ->
          List.iter (fun elt -> Llist.add ll elt 1) items_add1_par);

      (* Domain 2 : Paralled add *)
      Atomic.spawn (fun () ->
          List.iter (fun elt -> Llist.add ll elt 2) items_add2_par);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let add_seq = Sint.of_list items_add_seq in
              let add1 = Sint.of_list items_add1_par in
              let add2 = Sint.of_list items_add2_par in
              let all = Sint.union add_seq (Sint.union add1 add2) in
              Sint.for_all
                (fun elt ->
                  Llist.mem ll elt
                  && begin
                       match
                         ( Sint.mem elt add_seq,
                           Sint.mem elt add1,
                           Sint.mem elt add2 )
                       with
                       | true, false, false -> Llist.find_all ll elt = [ 0 ]
                       | true, true, false -> Llist.find_all ll elt = [ 1; 0 ]
                       | true, false, true -> Llist.find_all ll elt = [ 2; 0 ]
                       | true, true, true ->
                           let b = Llist.find_all ll elt in
                           b = [ 2; 1; 0 ] || b = [ 1; 2; 0 ]
                       | false, true, false -> Llist.find_all ll elt = [ 1 ]
                       | false, false, true -> Llist.find_all ll elt = [ 2 ]
                       | false, true, true ->
                           let b = Llist.find_all ll elt in
                           b = [ 2; 1 ] || b = [ 1; 2 ]
                       | _ -> false
                     end)
                all)))

let two_domains_remove_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let ll = Llist.create ~compare () in
      let nadd_seq = 6 in
      let factor = 3 in
      let items_add_seq = random_list nadd_seq factor in

      let npar = nadd_seq / 2 in
      let factor = 2 * factor in
      let items_remove1_par = random_uniq_list npar factor in
      let items_remove2_par = random_uniq_list npar factor in

      (* Sequential add *)
      List.iter (fun elt -> Llist.add ll elt 0) items_add_seq;

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
              let hadd =
                List.fold_left
                  (fun map elt ->
                    Mint.update elt
                      (function None -> Some 1 | Some n -> Some (n + 1))
                      map)
                  Mint.empty items_add_seq
              in

              let add_seq = Sint.of_list items_add_seq in
              let try_remove1 = Sint.of_list items_remove1_par in
              let try_remove2 = Sint.of_list items_remove2_par in
              let removed1 = Sint.of_list !removed1 in
              let removed2 = Sint.of_list !removed2 in
              Sint.for_all
                (fun elt ->
                  let in_add = Mint.find_opt elt hadd in
                  let len_bindings = Llist.find_all ll elt |> List.length in
                  match
                    ( Sint.mem elt add_seq,
                      Sint.mem elt try_remove1,
                      Sint.mem elt try_remove2 )
                  with
                  | true, false, false ->
                      Llist.mem ll elt && len_bindings = Option.get in_add
                  | false, true, false -> not @@ Sint.mem elt removed1
                  | false, false, true -> not @@ Sint.mem elt removed2
                  | true, true, false ->
                      Sint.mem elt removed1
                      && Option.get in_add = len_bindings + 1
                  | true, false, true ->
                      Sint.mem elt removed2
                      && Option.get in_add = len_bindings + 1
                  | true, true, true -> begin
                      (if Option.get in_add >= 2 then
                         Sint.mem elt removed1 && Sint.mem elt removed2
                       else Sint.mem elt removed1 || Sint.mem elt removed2)
                      && Option.get in_add = len_bindings + 2
                    end
                  | false, true, true ->
                      (not (Sint.mem elt removed1))
                      && not (Sint.mem elt removed2)
                  | _ -> false)
                (Sint.union add_seq (Sint.union try_remove1 try_remove2)))))

let () =
  let open Alcotest in
  Random.self_init ();
  run "llist_dscheck"
    [
      ( "basic",
        [
          test_case "2-domains_remove_add_simple" `Slow
            simple_two_domains_remove_add;
          test_case "2-domains_add_add" `Slow two_domains_add_add;
          test_case "2-domains_remove_add" `Slow two_domains_remove_add;
          test_case "2-domains_remove_remove" `Slow two_domains_remove_remove;
        ] );
    ]
