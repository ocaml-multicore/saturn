(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module Hash = Lf_hash.M;;
module Queue = Lf_list.M;;
module Cas = Kcas.W1;;

let print t = print_endline (Hash.to_string t (sprintf "%d"));;

(*let cprint t = print_endline (Hash.to_string_clean t);;
*)
let gen_elem nb m =
  Random.self_init ();
  let rec loop i out =
    if i < nb then begin
      let new_elem = 1 + Random.int m in
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

let gen_elem_same nb =
  let rec loop i out =
    if i < nb then
      loop (i+1) ((0)::out)
    else
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
    print_endline (sprintf "TH%d : POP DEB" (Domain.self ()));
    match Queue.pop q with
    |Some(v) ->
    print_endline (sprintf "TH%d : POP END" (Domain.self ()));
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

let benchmark f nb_thread message wait_time =
  let t1 = Unix.gettimeofday () in
  let t2 = ref (t1 *. 1000000.0) in
  for i = 1 to nb_thread do
    Domain.spawn (fun () ->
      f (); t2 := min !t2 (Unix.gettimeofday () -. t1); print_endline (sprintf message (Domain.self ())))
  done;
  Unix.sleep wait_time;
  !t2
;;

let benchmark_seq f =
  let t1 = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. t1
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
  let nb_init = 500 in
  let nb_end = 10 in
  let m = 1000 in
  let wait_time = 1 in
  let elem_init = gen_elem nb_init m in
  let elem_end  = gen_elem_same nb_end in

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
  let t_par_ins = benchmark (fun () -> insert_hash t1 q_init1) nb_thread "Insertion TH%d END" wait_time in

  print_endline (sprintf "Parallele insertion of %d elements and deletion of %d elements" (List.length elem_end) (List.length elem_init));
  let t_par_ins2 = benchmark (fun () -> insert_hash t1 q_end1) nb_thread "Insertion2 TH%d END" 0 in
  let t_par_rem = benchmark (fun () -> remove_hash t1 q_remove1) nb_thread "Deletion TH%d END" wait_time in


  print_endline (sprintf "Beginning sequential insertion/deletion");
  print_endline (sprintf "Sequential insertion of %d elements" (List.length elem_init));
  let t_seq_ins = benchmark_seq (fun () -> insert_hash t2 q_init2) in

  print_endline (sprintf "Sequential insertion of %d elements" (List.length elem_end));
  let t_seq_ins2 = benchmark_seq (fun () -> insert_hash t2 q_end2) in

  print_endline (sprintf "Sequential deletion of %d elements" (List.length elem_init));
  let t_seq_rem = benchmark_seq (fun () -> remove_hash t2 q_remove2) in

  print_endline (sprintf "FINISHED");

(*  Domain.spawn (fun () -> insert_hash_l t1 odd; print_endline (sprintf "Insertion TH%d ODD END" (Domain.self ())));
  Domain.spawn (fun () -> insert_hash_l t1 even; print_endline (sprintf "Insertion TH%d EVEN END" (Domain.self ())));
*)
(*
  print_endline "Elem Init :";
  List.iter (fun i -> printf "%d, " i) elem_init; print_endline "";
*)

  let elem_t1 = Hash.elem_of t1 in
  let elem_t2 = Hash.elem_of t2 in

  print t1;
  print t2;
(*  print_endline (sprintf "Are they equal ? %b" (Hash.equal t1 t2));*)
(*  print_endline (sprintf "Still valid split order ? %b" (Hash.still_split_order t1));*)
  print_endline (sprintf "T1 Elem valid ? %b" (check_valid t1 elem_end elem_init));
  print_endline (sprintf "T2 Elem valid ? %b" (check_valid t2 elem_end elem_init));
  print_endline (sprintf "T1    Queue IN Empty : %b    Queue OUT Empty : %b    Queue REMAIN Empty : %b" (Queue.is_empty q_init1) (Queue.is_empty q_remove1) (Queue.is_empty q_end1));
  print_endline (sprintf "T2    Queue IN Empty : %b    Queue OUT Empty : %b    Queue REMAIN Empty : %b" (Queue.is_empty q_init2) (Queue.is_empty q_remove2) (Queue.is_empty q_end2));
  print_endline (sprintf "Parallel Time :   %f (insertion : %f) (insertion2 : %f) (deletion : %f)" (t_par_ins +. t_par_ins2 +. t_par_rem) t_par_ins t_par_ins2 t_par_rem);
  print_endline (sprintf "Sequential Time : %f (insertion : %f) (insertion2 : %f) (deletion : %f)" (t_seq_ins +. t_seq_ins2 +. t_seq_rem) t_seq_ins t_seq_ins2 t_seq_rem);
  (*List.iter (fun l -> List.iter (fun i -> printf "%d, " i) l) l_elem_init; print_endline "";*)
  let (is_equal, elem_dif) = Hash.equal t1 t2 in
  print_endline (sprintf "T1 EQUAL T2 ? %b" is_equal);
  printf "[";
  List.iter (fun i -> printf "%d, " i) elem_t1; print_endline "]";
  printf "[";
  List.iter (fun i -> printf "%d, " i) elem_t2; print_endline "]";
  printf "[";
  List.iter (fun i -> printf "%d, " i) elem_dif; print_endline "]";
  Unix.sleep 2;
  elem_dif
;;

let rec exec () =
  if run () = [] then
    exec ()
;;

let () = exec ();;
