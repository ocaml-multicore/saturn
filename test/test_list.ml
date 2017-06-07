(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module STD_List = List;;
module List = Lockfree.List;;
module Queue = Lf_msqueue.M;;

let lprint l = List.print l (printf "%d");;

let rec split_compare a b =
  if a = 0 && b = 0 then
    0
  else
    let bit_a = a land 1 in
    let bit_b = b land 1 in
    if bit_a = bit_b then
      split_compare (a lsr 1) (b lsr 1)
    else if bit_a < bit_b then
      -1
    else
      1
;;

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

let select_elem l m n =
  let rec loop l out1 out2  =
    match l with
    |h::t ->
      if Random.int n < m then
        loop t (h::out1) out2
      else
        loop t out1 (h::out2)
    |[] -> out1, out2
  in loop l [] []
;;

let gen_queue l =
  let out = Queue.create () in
  let rec loop l =
    match l with
    |h::t -> Queue.push out h; loop t
    |[] -> out
  in loop l
;;

let insert_list l q =
  let rec loop () =
    match Queue.pop q with
    |Some(v) -> List.sinsert l v split_compare; loop ()
    |None -> ()
  in loop ()
;;

let rec remove_list l q =
  let b = Kcas.Backoff.create () in
  let rec loop () =
    match Queue.pop q with
    |Some(v) ->
      let out =  List.sdelete l v split_compare in
(*      printf "deletion %d : %b\n" v out;
      lprint l;*)
      if out then
        loop ()
      else
        (Queue.push q v; loop ())
    |None -> ()
  in loop ()
;;

let length l =
  let rec loop out =
    match List.pop l with
    |Some(_) -> loop (out+1)
    |None -> out
  in loop 0
;;

let to_list l =
  let rec loop out =
    match List.pop l with
    |Some(v) -> loop (v::out)
    |None -> STD_List.sort compare out
  in loop []
;;

let rec check_consistency l elem_l = l = STD_List.sort compare elem_l;;

let dif_list l1 l2 =
  let rec loop l out =
    match l with
    |h::t ->
      if STD_List.mem h l2 then
        loop t out
      else
        loop t (h::out)
    |[] -> out
  in loop l1 []
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

let run2 () =
  let l1 = List.create () in
  let l2 = List.create () in
  let nb_thread = 16 in
  let nb_init = 50 in
  let nb_end = 5 in
  let m = 1000 in
  let wait_time = 1 in
  let elem_init = gen_elem nb_init m in
  let elem_end  = gen_elem nb_end m in

  let elem_insert1 = gen_queue elem_init in
  let elem_delete1 = gen_queue elem_init in
  let elem_end1    = gen_queue elem_end in
  let elem_insert2 = gen_queue elem_init in
  let elem_delete2 = gen_queue elem_init in
  let elem_end2    = gen_queue elem_end in

  print_endline "Parallel Insertion Phase";
  let t_par_ins1 = benchmark (fun () -> insert_list l1 elem_insert1) nb_thread "Insertion TH%d END" wait_time in

  print_endline "Parallel Insertion/Deletion Phase";
  let t_par_del = benchmark (fun () -> remove_list l1 elem_delete1) nb_thread "Deletion TH%d END" 0 in
  let t_par_ins2 = benchmark (fun () -> insert_list l1 elem_end1) nb_thread "Deletion TH%d END" wait_time in

  print_endline "Sequential Insertion Phase";
  let t_seq_ins1 = benchmark_seq (fun () -> insert_list l2 elem_insert2) in

  print_endline "Sequential Insertion/Deletion Phase";
  let t_seq_del = benchmark_seq (fun () -> remove_list l2 elem_delete2) in
  let t_seq_ins2 = benchmark_seq (fun () -> insert_list l2 elem_end2) in

  let list_equal = List.equal l1 l2 in
  let elem_l1 = to_list l1 in
  let elem_l2 = to_list l2 in
  let elem_dif = dif_list elem_l1 elem_l2 in
  let consistency1 = check_consistency elem_l1 elem_end in
  let consistency2 = check_consistency elem_l2 elem_end in
  let len1 = STD_List.length elem_l1 in
  let len2 = STD_List.length elem_l2 in

  printf "["; STD_List.iter (printf "%d, ") elem_l1; print_endline "]";
  printf "["; STD_List.iter (printf "%d, ") elem_l2; print_endline "]";
  printf "["; STD_List.iter (printf "%d, ") elem_dif; print_endline "]";
  print_endline (sprintf "Insertions/Deletions : %d    Remain : %d" (STD_List.length elem_init) (STD_List.length elem_end));
  print_endline (sprintf "L1 equal L2 : %b" list_equal);
  print_endline (sprintf "Length L1 : %d    Length L2 : %d" len1 len2);
  print_endline (sprintf "Data consistency L1 : %b    Data consistency L2 : %b" consistency1 consistency2);
  print_endline (sprintf "Parallel Time : %f (Insertion : %f) (Deletion : %f) (Remain : %f)" (t_par_ins1 +. t_par_del +. t_par_ins2) t_par_ins1 t_par_del t_par_ins2);
  print_endline (sprintf "Sequential Time : %f (Insertion : %f) (Deletion : %f) (Remain : %f)" (t_seq_ins1 +. t_seq_del +. t_seq_ins2) t_seq_ins1 t_seq_del t_seq_ins2);



  ()
;;

let run () =
  let l1 = List.create () in
  let l2 = List.create () in
  let nb_thread = 2 in
  let n = 300 in
  let m = 10000 in
  let prob_m = 1 in
  let prob_n = 2 in
  let wait_time = 2 in
  let elem_insert = gen_elem n m in
  let (elem_delete, elem_remain) = select_elem elem_insert prob_m prob_n in
  let elem_insert1 = gen_queue elem_insert in
  let elem_delete1 = gen_queue elem_delete in
  let elem_insert2 = gen_queue elem_insert in
  let elem_delete2 = gen_queue elem_delete in

  print_endline (sprintf "Beginning parallel insertion/deletion");

  let t1_par = Unix.gettimeofday () in
  let t2_par = ref (t1_par *. 1000000.0) in
  for i = 1 to nb_thread do
    if i mod 2 = 0 then
      Domain.spawn (fun () -> insert_list l1 elem_insert1; print_endline (sprintf "Insertion TH%d END" (Domain.self ())))
    else
      Domain.spawn (fun () ->
                      remove_list l1 elem_delete1;
                      t2_par := min !t2_par (Unix.gettimeofday () -. t1_par);
                      print_endline (sprintf "Deletion TH%d END" (Domain.self ())))
  done;

  Unix.sleep wait_time;

  print_endline (sprintf "Beginning sequential insertion/deletion");
  let t1_seq = Unix.gettimeofday () in
  insert_list l2 elem_insert2;
  remove_list l2 elem_delete2;
  let t2_seq = Unix.gettimeofday () -. t1_seq in

  lprint l1;
  lprint l2;

  let list_equal = List.equal l1 l2 in
  let consistency = false(*check_consistency l1 elem_remain*) in
  let len1 = length l1 in
  let len2 = length l2 in

  print_endline (sprintf "Insertions : %d    Deletion : %d    Remain : %d" (STD_List.length elem_insert) (STD_List.length elem_delete) (STD_List.length elem_remain));
  print_endline (sprintf "L1 equal L2 : %b" list_equal);
  print_endline (sprintf "Length L1 : %d    Length L2 : %d" len1 len2);
  print_endline (sprintf "Data consistency : %b" consistency);
  print_endline (sprintf "Parallel Time : %f" !t2_par);
  print_endline (sprintf "Sequential Time : %f" t2_seq);


  ()
;;

let () = run2 ();;
