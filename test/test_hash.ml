(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module Hash = Lf_hash.M;;
module Queue = Lf_msqueue.M;;
module Cas = Kcas.W1;;

let print t = print_endline (Hash.to_string t (sprintf "%d"));;

(*let cprint t = print_endline (Hash.to_string_clean t);;
*)
let gen_elem nb m =
  Random.self_init ();
  let rec loop i out =
    if i < nb then begin
      let new_elem = Random.int m in
      loop (i+1) (new_elem::out)
    end else
      out
  in loop 0 []
;;

let gen_elem_bis nb m =
  let rec loop i out =
    if i < nb then begin
      loop (i+1) (i::out)
    end else
      out
  in loop 0 []
;;

let gen_multi_list l nb_thread =
  let len = 1 + (List.length l / nb_thread) in
  let rec loop_l l i out =
    match l with
    |[] -> ([], out)
    |hd::tl ->
      if i > 0 then
        loop_l tl (i-1) (hd::out)
      else
        (l, out)
  in
  let rec loop l out =
    match loop_l l len [] with
    |[], elem_l -> elem_l::out
    |l', elem_l -> loop l' (elem_l::out)
  in loop l []
;;

let gen_queue l =
  let out = Queue.create () in
  let rec loop l =
    match l with
    |h::t -> Queue.push out h; loop t
    |[] -> out
  in loop l; out
;;

let split_odd_even l =
  let rec loop l odd even =
    match l with
    |[] -> (odd, even)
    |hd::tl ->
      if hd mod 2 = 0 then
        loop tl odd (hd::even)
      else
        loop tl (hd::odd) even
  in loop l [] []
;;

let insert_hash t q =
  let rec loop () =
(*    print_endline (sprintf "TH%d : POP DEB" (Domain.self ()));*)
    match Queue.pop q with
    |Some(v) ->
(*    print_endline (sprintf "TH%d : POP END" (Domain.self ()));*)
    Hash.add t v v; loop ()
    |None -> ()
  in loop ()
;;

let rec insert_hash_l t l =
  match l with
  |[] -> ()
  |hd::tl ->
  print_endline (sprintf "Insertion of %d (TH%d)" hd (Domain.self ()));
  Hash.add t hd hd; insert_hash_l t tl
;;

let remove_hash t q =
  let rec loop () =
    match Queue.pop q with
    |Some(v) ->
      print_endline (sprintf "Deletion of %d (TH%d)" v (Domain.self ()));
      if Hash.remove t v then
        loop ()
      else
        (print_endline (sprintf "Deletion FAILED of %d (TH%d), retry later" v (Domain.self ()));
        Queue.push q v; loop ())
    |None -> ()
  in loop ()
;;

let rec check_valid t elem_in elem_out =
  let rec loop_in elem =
    match elem with
    |[] -> true
    |hd::tl -> Hash.mem t hd && loop_in tl
  in
  let rec loop_out elem =
    match elem with
    |[] -> true
    |hd::tl -> (not (Hash.mem t hd)) && loop_out tl
  in
  loop_in elem_in
;;

let run () =
  let t1 = Hash.create () in
  let t2 = Hash.create () in
  let nb_thread = 16 in
  let nb_init = 50 in
  let nb_end = 5 in
  let m = 1000 in
  let wait_time = 2 in
  let elem_init = gen_elem nb_init m in
  let elem_end  = gen_elem nb_end m in

(*  let l_elem_init = gen_multi_list elem_init nb_thread in*)
  let (odd, even) = split_odd_even elem_init in

  let q_init1 = gen_queue elem_init in
  let q_init2 = gen_queue elem_init in
  let q_end1 = gen_queue elem_end in
  let q_end2 = gen_queue elem_end in
  let q_remove1 = gen_queue elem_init in
  let q_remove2 = gen_queue elem_init in

  print t1;
  print t2;

  print_endline (sprintf "NB Thread : %d" nb_thread);
  print_endline (sprintf "Parallele insertion of %d elements" (List.length elem_init));

  let t1_par_ins = Unix.gettimeofday () in
  let t2_par_ins = ref (t1_par_ins *. 1000000.0) in
  for i = 1 to nb_thread do
    Domain.spawn (fun () ->
                    insert_hash t1 q_init1;
                    t2_par_ins := min !t2_par_ins (Unix.gettimeofday () -. t1_par_ins);
                    print_endline (sprintf "Insertion TH%d END" (Domain.self ())))
  done;

  Unix.sleep wait_time;

  print_endline (sprintf "Parallele insertion of %d elements and deletion of %d elements" (List.length elem_end) (List.length elem_init));

  let t1_par_ins2 = Unix.gettimeofday () in
  let t2_par_ins2 = ref (t1_par_ins2 *. 1000000.0) in
  let t1_par_rem = Unix.gettimeofday () in
  let t2_par_rem = ref (t1_par_rem *. 1000000.0) in
  for i = 1 to nb_thread do
    if i mod 2 = 0 then
      Domain.spawn (fun () ->
                      insert_hash t1 q_end1;
                      t2_par_ins2 := min !t2_par_ins2 (Unix.gettimeofday () -. t1_par_ins2);
                      print_endline (sprintf "Insertion2 TH%d END" (Domain.self ())))
    else
      Domain.spawn (fun () ->
                      remove_hash t1 q_remove1;
                      t2_par_rem := min !t2_par_rem (Unix.gettimeofday () -. t1_par_rem);
                      print_endline (sprintf "Deletion TH%d END" (Domain.self ())))
  done;

  Unix.sleep wait_time;


  print_endline (sprintf "Beginning sequential insertion/deletion");
  print_endline (sprintf "Sequential insertion of %d elements" (List.length elem_init));
  let t1_seq_ins = Unix.gettimeofday () in
  insert_hash t2 q_init2;
  let t2_seq_ins = Unix.gettimeofday () -. t1_seq_ins in
  print_endline (sprintf "Sequential insertion of %d elements" (List.length elem_end));
  let t1_seq_ins2 = Unix.gettimeofday () in
  insert_hash t2 q_end2;
  let t2_seq_ins2 = Unix.gettimeofday () -. t1_seq_ins2 in
  print_endline (sprintf "Sequential deletion of %d elements" (List.length elem_init));
  let t1_seq_rem = Unix.gettimeofday () in
  remove_hash t2 q_remove2;
  let t2_seq_rem = Unix.gettimeofday () -. t1_seq_rem in

(*  Domain.spawn (fun () -> insert_hash_l t1 odd; print_endline (sprintf "Insertion TH%d ODD END" (Domain.self ())));
  Domain.spawn (fun () -> insert_hash_l t1 even; print_endline (sprintf "Insertion TH%d EVEN END" (Domain.self ())));
*)
(*
  print_endline "Elem Init :";
  List.iter (fun i -> printf "%d, " i) elem_init; print_endline "";
*)
  print t1;
  print t2;
(*  print_endline (sprintf "Are they equal ? %b" (Hash.equal t1 t2));*)
(*  print_endline (sprintf "Still valid split order ? %b" (Hash.still_split_order t1));*)
  print_endline (sprintf "T1 Elem valid ? %b" (check_valid t1 elem_end elem_init));
  print_endline (sprintf "T2 Elem valid ? %b" (check_valid t2 elem_end elem_init));
  print_endline (sprintf "T1    Queue IN Empty : %b    Queue OUT Empty : %b    Queue REMAIN Empty : %b" (Queue.is_empty q_init1) (Queue.is_empty q_remove1) (Queue.is_empty q_end1));
  print_endline (sprintf "T2    Queue IN Empty : %b    Queue OUT Empty : %b    Queue REMAIN Empty : %b" (Queue.is_empty q_init2) (Queue.is_empty q_remove2) (Queue.is_empty q_end2));
  print_endline (sprintf "Parallel Time :   %f (insertion : %f) (insertion2 : %f) (deletion : %f)" (!t2_par_ins +. !t2_par_ins2 +. !t2_par_rem) !t2_par_ins !t2_par_ins2 !t2_par_rem);
  print_endline (sprintf "Sequential Time : %f (insertion : %f) (insertion2 : %f) (deletion : %f)" (t2_seq_ins +. t2_seq_ins2 +. t2_seq_rem) t2_seq_ins t2_seq_ins2 t2_seq_rem);
  (*List.iter (fun l -> List.iter (fun i -> printf "%d, " i) l) l_elem_init; print_endline "";*)
  let (is_equal, elem_dif) = Hash.equal t1 t2 in
  print_endline (sprintf "T1 EQUAL T2 ? %b" is_equal);
  printf "[";
  List.iter (fun i -> printf "%d, " i) elem_dif; print_endline "]";
  ()
;;

let () = run ();;
