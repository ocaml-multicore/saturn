module Spsc_queue = Saturn_lockfree.Single_prod_single_cons_queue

let keep_some l = List.filter Option.is_some l |> List.map Option.get
let keep_n_first n = List.filteri (fun i _ -> i < n)

let pop_opt_n_times q n =
  let rec loop count acc =
    if count = 0 then acc
    else
      let v = Spsc_queue.pop_opt q in
      Domain.cpu_relax ();
      loop (count - 1) (v :: acc)
  in
  loop n [] |> List.rev

let pop_n_times q n =
  let rec loop count acc =
    if count = 0 then acc
    else
      try
        let v = Spsc_queue.pop_exn q in
        Domain.cpu_relax ();
        loop (count - 1) (Some v :: acc)
      with Spsc_queue.Empty -> loop (count - 1) (None :: acc)
  in
  loop n [] |> List.rev

let tests =
  [
    (* TEST 1 - one producer, one consumer:
       Sequential pushes then pops. Checks that the behaviour is similar to
       one of a FIFO queue. *)
    QCheck.(
      Test.make ~name:"seq_pop_opt_push"
        (pair (list int) small_nat)
        (fun (l, npop) ->
          (* Making sure we do not create a too big queue. Other
             tests are checking the behaviour of a full queue.*)
          let size_exponent = 8 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (List.length l < size_max);

          (* Initialization *)
          let q = Spsc_queue.create ~size_exponent in

          (* Sequential pushed : not Full exception should be
             raised. *)
          let not_full_queue =
            try
              List.iter (Spsc_queue.push_exn q) l;
              true
            with Spsc_queue.Full -> false
          in

          (* Consumer domain pops *)
          let consumer = Domain.spawn (fun () -> pop_opt_n_times q npop) in
          let pops = Domain.join consumer in

          (* Property *)
          not_full_queue
          && List.length pops = npop
          && keep_some pops = keep_n_first (min (List.length l) npop) l));
    (* TEST 1b - one producer, one consumer:
       Same than previous with pop instead of pop_opt *)
    QCheck.(
      Test.make ~name:"seq_pop_push"
        (pair (list int) small_nat)
        (fun (l, npop) ->
          (* Making sure we do not create a too big queue. Other
             tests are checking the behaviour of a full queue.*)
          let size_exponent = 8 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (List.length l < size_max);

          (* Initialization *)
          let q = Spsc_queue.create ~size_exponent in

          (* Sequential pushed : not Full exception should be
             raised. *)
          let not_full_queue =
            try
              List.iter (Spsc_queue.push_exn q) l;
              true
            with Spsc_queue.Full -> false
          in

          (* Consumer domain pops *)
          let consumer = Domain.spawn (fun () -> pop_n_times q npop) in
          let pops = Domain.join consumer in

          (* Property *)
          not_full_queue
          && List.length pops = npop
          && keep_some pops = keep_n_first (min (List.length l) npop) l));
    (* TEST 1b - one producer, one consumer:
       Same than TEST1 with try_push instead of push *)
    QCheck.(
      Test.make ~name:"seq_pop_try_push"
        (pair (list int) small_nat)
        (fun (l, npop) ->
          (* Making sure we do not create a too big queue. Other
             tests are checking the behaviour of a full queue.*)
          let size_exponent = 8 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (List.length l < size_max);

          (* Initialization *)
          let q = Spsc_queue.create ~size_exponent in

          (* Sequential pushed : [try_push] should always returns
             true. *)
          let not_full_queue = List.for_all (Spsc_queue.try_push q) l in

          (* Consumer domain pops *)
          let consumer = Domain.spawn (fun () -> pop_opt_n_times q npop) in
          let pops = Domain.join consumer in

          (* Property *)
          not_full_queue
          && List.length pops = npop
          && keep_some pops = keep_n_first (min (List.length l) npop) l));
    (* TEST 2 - one producer, one consumer:
       Parallel pushes and pops. Checks that the behaviour is similar to
       one of a FIFO queue. *)
    QCheck.(
      Test.make ~name:"par_pop_push"
        (pair (pair (list int) (list int)) small_nat)
        (fun ((l, l'), npop) ->
          (* Making sure we do not create a too big queue. Other
               tests are checking the behaviour of a full queue.*)
          let size_exponent = 8 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (List.length l + List.length l' < size_max);

          (* Initialization *)
          let barrier = Barrier.create 2 in
          let q = Spsc_queue.create ~size_exponent in
          List.iter (Spsc_queue.push_exn q) l;

          (* Consumer pops *)
          let consumer =
            Domain.spawn (fun () ->
                Barrier.await barrier;
                pop_opt_n_times q npop)
          in

          let producer =
            Domain.spawn (fun () ->
                Barrier.await barrier;
                (* Main domain pushes.*)
                List.iter
                  (fun elt ->
                    Spsc_queue.push_exn q elt;
                    Domain.cpu_relax ())
                  l')
          in

          let popped = Domain.join consumer in
          let _ = Domain.join producer in
          let popped_val = popped |> keep_some in

          (* Property *)
          List.length popped = npop
          && popped_val = keep_n_first (List.length popped_val) (l @ l')));
    (* TEST 3 - one producer, one consumer:
       Checks that pushing too many elements raise exception Full. *)
    QCheck.(
      Test.make ~name:"push_full" (list int) (fun l ->
          let size_exponent = 4 in
          let size_max = Int.shift_left 1 size_exponent in

          (* Initialization *)
          let q = Spsc_queue.create ~size_exponent in
          let is_full =
            try
              List.iter (Spsc_queue.push_exn q) l;
              false
            with Spsc_queue.Full -> true
          in

          (* Property *)
          (List.length l > size_max && is_full)
          || (List.length l <= size_max && not is_full)));
    (* TEST 4 - one producer, one consumer:
        Sequential checks that [peek_opt] read the next value. *)
    QCheck.(
      Test.make ~name:"seq_peek_opt" (list int) (fun l ->
          let size_exponent = 10 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (size_max > List.length l);

          (* Initialisation : pushing l in a new spsc queue. *)
          let q = Spsc_queue.create ~size_exponent in
          List.iter (Spsc_queue.push_exn q) l;

          (* Test : we consecutively peek and pop and check both
             matches with pushed elements. *)
          let rec loop pushed =
            match (pushed, Spsc_queue.peek_opt q) with
            | [], None -> (
                match Spsc_queue.pop_opt q with None -> true | Some _ -> false)
            | x :: pushed, Some y when x = y -> (
                match Spsc_queue.pop_opt q with
                | None -> false
                | Some z when y = z -> loop pushed
                | _ -> false)
            | _, _ -> false
          in
          loop l));
    (* TEST 4b - one producer, one consumer:
        Same then previous one for [peek] instead of [peek_one]. *)
    QCheck.(
      Test.make ~name:"seq_peek" (list int) (fun l ->
          let size_exponent = 10 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (size_max > List.length l);

          (* Initialisation : pushing l in a new spsc queue. *)
          let q = Spsc_queue.create ~size_exponent in
          List.iter (Spsc_queue.push_exn q) l;

          (* Test : we consecutively peek and pop and check both
             matches with pushed elements. *)
          let rec loop pushed =
            let peeked =
              try Some (Spsc_queue.peek_exn q) with Spsc_queue.Empty -> None
            in
            match (pushed, peeked) with
            | [], None -> (
                match Spsc_queue.pop_opt q with None -> true | Some _ -> false)
            | x :: pushed, Some y when x = y -> (
                match Spsc_queue.pop_opt q with
                | None -> false
                | Some z when y = z -> loop pushed
                | _ -> false)
            | _, _ -> false
          in
          loop l));
    (* TEST 5 - one producer, one consumer:
        Parallel test of [peek_opt] with [try_push]. *)
    QCheck.(
      Test.make ~name:"par_peek_opt" (list int) (fun pushed ->
          let size_exponent = 10 in
          let size_max = Int.shift_left 1 size_exponent in
          let npush = List.length pushed in
          assume (size_max > npush);
          let barrier = Barrier.create 2 in

          (* Initialisation : pushing l in a new spsc queue. *)
          let q = Spsc_queue.create ~size_exponent in

          (* Test :
             - domain1 pushes a list of element
             - in parallel, domain2 peeks then pops. *)
          let domain1 =
            Domain.spawn (fun () ->
                Barrier.await barrier;
                List.iter
                  (fun elt ->
                    Domain.cpu_relax ();
                    Spsc_queue.push_exn q elt)
                  pushed)
          in

          let domain2 =
            Domain.spawn (fun () ->
                let peeked = ref [] in
                let popped = ref [] in
                Barrier.await barrier;
                for _ = 0 to npush - 1 do
                  Domain.cpu_relax ();
                  (* peek then pop *)
                  let peek = Spsc_queue.peek_opt q in
                  let pop = Spsc_queue.pop_opt q in
                  peeked := peek :: !peeked;
                  popped := pop :: !popped
                done;
                (!peeked, !popped))
          in
          Domain.join domain1;
          let peeked, popped = Domain.join domain2 in
          let peeked = List.rev peeked in
          let popped = List.rev popped in

          let rec check pushed peeked popped =
            match (pushed, peeked, popped) with
            | _, [], [] ->
                (* pushed can not be empty if the consumer
                   finished before the producer *)
                true
            | _, None :: peeked, None :: popped ->
                (* consumer tries to peek then pop when the queue was empty *)
                check pushed peeked popped
            | push :: pushed, Some peek :: peeked, Some pop :: popped
              when push = pop && push = peek ->
                (* consumer peeks and pops on an non-empty queue. The
                   peeked and the popped element must be the same. *)
                check pushed peeked popped
            | push :: pushed, None :: peeked, Some pop :: popped when push = pop
              ->
                (* consumer peeks when the queue was empty, then
                   producer pushes at least once and then consumer
                   pops. *)
                check pushed peeked popped
            | _, _, _ -> false
          in
          check pushed peeked popped));
  ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Spsc_queue" [ ("spsc_queue", to_alcotest tests) ]
;;

main ()
