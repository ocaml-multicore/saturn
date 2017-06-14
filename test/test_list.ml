(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module STD_List = List;;
module List = Lockfree.List;;
module Queue = Lf_list.M;;

let lprint l = print_string (List.to_string l (sprintf "%d"));;

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
    |Some(v) ->
      List.sinsert l v split_compare;
      loop ()
    |None -> ()
  in loop ()
;;

let rec remove_list l q =
  let b = Kcas.Backoff.create () in
  let rec loop () =
    match Queue.pop q with
    |Some(v) ->
      let out =  List.sdelete l v split_compare in
      loop ()
    |None -> ()
  in loop ()
;;

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

let benchmark f nb_thread message wait_time out verbose =
  let t1 = Unix.gettimeofday () in
  out := t1 *. 1000000.0;
  for i = 1 to nb_thread do
    Domain.spawn (fun () ->
      f (); out := min !out (Unix.gettimeofday () -. t1); if verbose then print_endline (sprintf message (Domain.self ())))
  done;
  Unix.sleep wait_time;
  ()
;;

let benchmark_seq f =
  let t1 = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. t1
;;

let run num_test verbose =
  print_endline (sprintf "TEST N°%d" num_test);

  let l1 = List.create () in
  let l2 = List.create () in
  let nb_thread = 16 in
  let nb_init = 4000 in
  let nb_end = 1000 in
  let m = 100000000 in
  let wait_time = 2 in
  let elem_init = gen_elem nb_init m in
  let elem_end  = gen_elem nb_end m in

  let elem_insert1 = gen_queue elem_init in
  let elem_delete1 = gen_queue elem_init in
  let elem_end1    = gen_queue elem_end in
  let elem_insert2 = gen_queue elem_init in
  let elem_delete2 = gen_queue elem_init in
  let elem_end2    = gen_queue elem_end in

  let t_par_ins1 = ref 0.0 in
  let t_par_del = ref 0.0 in
  let t_par_ins2 = ref 0.0 in
  if verbose then print_endline "Parallel Insertion Phase";
  benchmark (fun () -> insert_list l1 elem_insert1) nb_thread "Insertion TH%d END" wait_time t_par_ins1 verbose;

  if verbose then print_endline "Parallel Insertion/Deletion Phase";
  benchmark (fun () -> Unix.sleep 1; remove_list l1 elem_delete1) nb_thread "Deletion TH%d END" 0 t_par_del verbose;
  benchmark (fun () -> Unix.sleep 1; insert_list l1 elem_end1) nb_thread "Insertion_bis TH%d END" wait_time t_par_ins2 verbose;

  if verbose then print_endline "Sequential Insertion Phase";
  let t_seq_ins1 = benchmark_seq (fun () -> insert_list l2 elem_insert2) in

  if verbose then print_endline "Sequential Insertion/Deletion Phase";
  let t_seq_del = benchmark_seq (fun () -> remove_list l2 elem_delete2) in
  let t_seq_ins2 = benchmark_seq (fun () -> insert_list l2 elem_end2) in

  let list_equal = List.equal l1 l2 in
  let elem_l1 = List.elem_of l1 in
  let elem_l2 = List.elem_of l2 in
  let elem_dif = dif_list elem_l1 elem_end in
  let len1 = STD_List.length elem_l1 in
  let len2 = STD_List.length elem_l2 in

  if verbose then begin
    printf "["; STD_List.iter (printf "%d, ") (STD_List.sort split_compare elem_l1); print_endline "]";
    printf "["; STD_List.iter (printf "%d, ") (STD_List.sort split_compare elem_l2); print_endline "]";
    printf "["; STD_List.iter (printf "%d, ") (STD_List.sort split_compare elem_end); print_endline "]";
    printf "["; STD_List.iter (printf "%d, ") (STD_List.sort split_compare elem_dif); print_endline "]";
    print_endline (sprintf "Insertions/Deletions : %d    Remain : %d" (STD_List.length elem_init) (STD_List.length elem_end));
    print_endline (sprintf "L1 equal L2 : %b" list_equal);
    print_endline (sprintf "Length L1 : %d    Length L2 : %d" len1 len2);
    print_endline (sprintf "Parallel Time : %f (Insertion : %f) (Deletion : %f) (Remain : %f)" (!t_par_ins1 +. !t_par_del +. !t_par_ins2) !t_par_ins1 !t_par_del !t_par_ins2);
    print_endline (sprintf "Sequential Time : %f (Insertion : %f) (Deletion : %f) (Remain : %f)" (t_seq_ins1 +. t_seq_del +. t_seq_ins2) t_seq_ins1 t_seq_del t_seq_ins2)
  end;

  elem_dif
;;

let rec loop nb_test verbose =
  let rec bosse nb out =
    if nb <= nb_test then
      let dif = run nb verbose in
      if dif = [] then
        bosse (nb+1) (out+1)
      else
        bosse (nb+1) out
    else
      print_endline (sprintf "TEST SUCCESS %d/%d" out nb_test)
  in bosse 1 0
;;

let () = loop 3 true;;
