open Lockfree.Ws_deque.M
(** Tests *)

let test_empty () =
  let q = create () in
  match pop q with
  | exception Exit -> print_string "test_exit: ok\n"
  | _ -> assert false

let test_push_and_pop () =
  let q = create () in
  push q 1;
  push q 10;
  push q 100;
  assert (pop q = 100);
  assert (pop q = 10);
  assert (pop q = 1);
  print_string "test_push_and_pop: ok\n"

let test_push_and_steal () =
  let q = create () in
  push q 1;
  push q 10;
  push q 100;
  let domains =
    Array.init 3 (fun _ ->
        Domain.spawn (fun _ ->
            let v = steal q in
            assert (v = 1 || v = 10 || v = 100)))
  in
  Array.iter Domain.join domains;
  print_string "test_push_and_steal: ok\n"

let tailrec_concat l1 l2 = List.rev_append (List.rev l1) l2

let test_concurrent_workload () =
  (* The desired number of push events. *)
  let n = ref 100000 in
  (* The desired number of steal attempts per thief. *)
  let attempts = 100000 in
  (* The number of thieves. *)
  let thieves = 16 in
  (* The queue. *)
  let q = create () in
  (* A generator of fresh elements. *)
  let c = ref 0 in
  let fresh () =
    let x = !c in
    c := x + 1;
    x
  in
  (* A history of pushed elements. *)
  let pushed = ref []
  (* A history of popped elements. *)
  and popped = ref []
  (* Histories of stolen elements. *)
  and stolen = Array.make thieves [] in
  (* The owner thread. *)
  let owner =
    Domain.spawn (fun () ->
        let push () =
          let x = fresh () in
          push q x;
          pushed := x :: !pushed;
          decr n
        and pop () =
          match pop q with
          | exception Exit -> false
          | x ->
              popped := x :: !popped;
              true
        in

        let rec loop () =
          if !n > 0 then (
            (* More pushes are allowed. *)
            (* Choose between pushing and popping; then continue. *)
            if Random.bool () then push () else ignore (pop ());
            loop ())
          else if (* No more pushes are allowed. Pop and continue. *)
                  pop () then loop ()
        in
        loop ())
  in
  (* The thief threads. *)
  let thieves =
    Array.init thieves (fun i ->
        Domain.spawn (fun () ->
            let steal () =
              match steal q with
              | exception Exit -> ()
              | x -> stolen.(i) <- x :: stolen.(i)
            in

            for _i = 1 to attempts do
              (* Should we somehow wait between two steal attempts? *)
              steal ()
            done))
  in
  (* Wait for every thread to complete. *)
  Domain.join owner;
  Array.iter Domain.join thieves;
  (* Check that the elements that have been popped or stolen are exactly the
     elements that have been pushed. Thus, no element is lost, duplicated,
     or created out of thin air. *)
  let pushed = !pushed and popped = !popped in
  let npushed = List.length pushed
  and npopped = List.length popped
  and nstolen =
    Array.fold_left (fun accu stolen -> accu + List.length stolen) 0 stolen
  in
  assert (npushed = npopped + nstolen);
  let sort xs = List.sort compare xs in
  let stolen =
    Array.fold_left (fun accu stolen -> tailrec_concat accu stolen) [] stolen
  in
  assert (sort pushed = sort (tailrec_concat popped stolen));
  (* Print a completion message. *)
  Printf.printf
    "test_concurrent_workload: ok (pushed = %d, popped = %d, stolen = %d)\n"
    npushed npopped nstolen

let _ =
  test_empty ();
  test_push_and_pop ();
  test_push_and_steal ();
  test_concurrent_workload ()
