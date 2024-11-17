module Ws_deque = Saturn.Work_stealing_deque

(* Sequential building of a deque *)
let deque_of_list l =
  let deque = Ws_deque.create () in
  List.iter (Ws_deque.push deque) l;
  deque

(* [extract_n_from_d q f n] extract [n] elements of [q] by calling [n]
   times the function [f] on [q]. *)
let extract_n_of_deque q fextract n =
  let rec loop acc = function
    | 0 -> acc
    | n ->
        let a = fextract q in
        loop (a :: acc) (n - 1)
  in
  loop [] n |> List.rev

let keep_some l = List.filter Option.is_some l |> List.map Option.get
let keep_n_first n = List.filteri (fun i _ -> i < n)

let tests_one_producer =
  [
    (* TEST 1 - single producer no stealer:
       forall l, l' and with q built by pushing in order (l@l')
                pop q :: pop q :: pop q :: ... :: [] = List.rev l' *)
    QCheck.(
      Test.make ~name:"pops_are_in_order"
        (pair (list int) (list int))
        (fun (l, l') ->
          assume (l' <> []);
          let deque = deque_of_list (l @ l') in

          let pop_list =
            extract_n_of_deque deque Ws_deque.pop_exn (List.length l')
          in
          pop_list = List.rev l'));
    (* TEST 2 - single producer no stealer :
       forall q of size n, forall m > n,  poping m times raises Exit (m-n) times. *)
    QCheck.(
      Test.make ~name:"pop_on_empty_deque_raises_exit" ~count:1
        (pair (list int) small_nat)
        (fun (l, m) ->
          assume (m > 0);
          let n = List.length l in
          let m = m + n in
          let count = ref 0 in
          let deque = deque_of_list l in

          for _i = 0 to m - 1 do
            try ignore (Ws_deque.pop_exn deque) with Exit -> incr count
          done;

          !count = m - n));
  ]

let tests_one_producer_one_stealer =
  [
    (* TEST 1 with 1 producer, 1 stealer and sequential execution.
       Producer domain pushes a list of value THEN a stealer domain
       steals.

       This checks :
       - order is preserved (first push = first steal)
       - Exit is raised only when the deque is empty *)
    QCheck.(
      Test.make ~name:"steals_are_in_order"
        (pair (list int) small_nat)
        (fun (l, n) ->
          (* Main domain pushes all elements of [l] in order. *)
          let deque = deque_of_list l in

          (* Then the stealer domain steals [n] times. The output list
             is composed of all stolen value. If an [Exit] is raised,
             it is register as a [None] value in the returned list.*)
          let stealer =
            Domain.spawn (fun () ->
                let steal' deque =
                  match Ws_deque.steal_exn deque with
                  | value -> Some value
                  | exception Exit ->
                      Domain.cpu_relax ();
                      None
                in
                extract_n_of_deque deque steal' n)
          in
          let steal_list = Domain.join stealer in

          (* The stolen values should be the [n]th first elements of [l]*)
          (let expected_stolen = keep_n_first n l in
           let nfirst = keep_n_first (List.length l) steal_list in

           List.for_all2
             (fun found_opt expected ->
               match found_opt with
               | Some found -> found = expected
               | None -> false)
             nfirst expected_stolen)
          &&
          (* The [n - (List.length l)] last values of [steal_list]
             should be [None] (i.e. the [steal] function had raised [Exit]). *)
          let exits = List.filteri (fun i _ -> i >= List.length l) steal_list in
          List.for_all (function None -> true | _ -> false) exits));
    (* TEST 2 with 1 producer, 1 stealer and parallel execution.

       Producer domain does pushes. Simultaneously the stealer domain steals.

       This test checks :
       - order is preserved (first push = first steal)
       - Exit is raised only when the deque is empty *)
    QCheck.(
      Test.make ~name:"parallel_pushes_and_steals"
        (pair (list small_int) (int_bound 200))
        (fun (l, n) ->
          (* Initialization *)
          let deque = Ws_deque.create () in
          let barrier = Barrier.create 2 in

          (* The stealer domain steals n times. If a value [v] is stolen,
             it is registered as [Some v] in the returned list whereas any
             [Exit] raised is registered as a [None].*)
          let stealer =
            Domain.spawn (fun () ->
                Barrier.await barrier;
                let steal' deque =
                  match Ws_deque.steal_exn deque with
                  | value -> Some value
                  | exception Exit ->
                      Domain.cpu_relax ();
                      None
                in
                extract_n_of_deque deque steal' n)
          in

          Barrier.await barrier;

          (* Main domain pushes.*)
          List.iter
            (fun elt ->
              Ws_deque.push deque elt;
              Domain.cpu_relax ())
            l;

          let steal_list = Domain.join stealer in

          (* We don't know how the pushes and the steals are interleaved
             but we can check that if [m] values have been stolen, they are
             the [m] first pushed values. *)
          List.length steal_list = n
          &&
          let stolen = keep_some steal_list in
          let expected_stolen = keep_n_first (List.length stolen) l in
          stolen = expected_stolen));
    (* TEST 3 with 1 producer, 1 stealer and parallel execution.

       Main domain does sequential pushes and then pops at the same time that a
       stealer domain steals.

       This test checks :
       - order is preserved (first push = first steal, first push = last pop)
       - no value is both popped and stolen.

       We actually have a strong property here, as all the [push] calls are done before
       [pop] and [steal] calls :

       stolen_values @ (List.rev popped_values) = pushed_values *)
    QCheck.(
      Test.make ~name:"parallel_pops_and_steals"
        (pair (list small_int) (pair small_nat small_nat))
        (fun (l, (nsteal, npop)) ->
          assume (nsteal + npop > List.length l);
          (* Initialization - sequential pushes*)
          let deque = deque_of_list l in
          let barrier = Barrier.create 2 in
          Random.self_init ();
          let pop' deque =
            match Ws_deque.pop_exn deque with
            | value -> Some value
            | exception Exit ->
                Domain.cpu_relax ();
                None
          in

          (* The stealer domain steals [nsteal] times. If a value [v] is stolen,
             it is registered as [Some v] in the returned list whereas any [Exit]
             raised, it is registered as a [None].*)
          let stealer =
            Domain.spawn (fun () ->
                Barrier.await barrier;
                let steal' deque =
                  match Ws_deque.steal_exn deque with
                  | value -> Some value
                  | exception Exit ->
                      Domain.cpu_relax ();
                      None
                in
                extract_n_of_deque deque steal' nsteal)
          in

          Barrier.await barrier;

          (* Main domain pops and builds a list of popped values. *)
          let pop_list = extract_n_of_deque deque pop' npop in

          let steal_list = Domain.join stealer in

          (* All the pushes are done sequentially before the run so whatever
             how pops and steals are interleaved if [npop + nsteal > npush]
             we should have stolen @ (List.rev popped) = pushed . *)
          List.length steal_list = nsteal
          && List.length pop_list = npop
          &&
          let stolen = keep_some steal_list in
          let popped = keep_some pop_list in
          stolen @ List.rev popped = l));
  ]

let tests_one_producer_two_stealers =
  [
    (* TEST 1 with 1 producer, 2 stealers and parallel steal calls.

       Producer domain does sequential pushes. Two stealers steal simultaneously.

       This test checks :
       - order is preserved (first push = first steal)
       - no element is stolen by both stealers
       - Exit is raised only when the deque is empty *)
    QCheck.(
      Test.make ~name:"parallel_steals"
        (pair (list small_int) (pair small_nat small_nat))
        (fun (l, (ns1, ns2)) ->
          (* Initialization *)
          let deque = deque_of_list l in
          let barrier = Barrier.create 2 in

          (* Steal calls *)
          let multiple_steal deque nsteal =
            Barrier.await barrier;

            let res = Array.make nsteal None in

            for i = 0 to nsteal - 1 do
              res.(i) <-
                (match Ws_deque.steal_exn deque with
                | value -> Some value
                | exception Exit ->
                    Domain.cpu_relax ();
                    None)
            done;
            res
          in

          let stealer1 = Domain.spawn (fun () -> multiple_steal deque ns1) in
          let stealer2 = Domain.spawn (fun () -> multiple_steal deque ns2) in

          let steal_list1 = Domain.join stealer1 in
          let steal_list2 = Domain.join stealer2 in

          let stolen1 = keep_some (Array.to_list steal_list1) in
          let stolen2 = keep_some (Array.to_list steal_list2) in

          (* We expect the stolen values to be the first ones that have been
             pushed. *)
          let expected_stolen = keep_n_first (ns1 + ns2) l in

          (* [compare l l1 l2] checks that there exists an interlacing of
             the stolen values [l1] and [l2] that is equal to the beginning
             of the push list [l]. *)
          let rec compare l l1 l2 =
            match (l, l1, l2) with
            | [], [], [] -> true
            | [], _, _ -> false
            | _, [], _ -> l = l2
            | _, _, [] -> l = l1
            | x :: l', y :: l1', z :: l2' ->
                if x = y && x = z then compare l' l1 l2' || compare l' l1' l2
                else if x = y then compare l' l1' l2
                else if x = z then compare l' l1 l2'
                else false
          in

          Array.length steal_list1 = ns1
          && Array.length steal_list2 = ns2
          && compare expected_stolen stolen1 stolen2));
  ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Ws_deque"
    [
      ("one_producer", to_alcotest tests_one_producer);
      ("one_producer_one_stealer", to_alcotest tests_one_producer_one_stealer);
      ("one_producer_two_stealers", to_alcotest tests_one_producer_two_stealers);
    ]
;;

main ()
