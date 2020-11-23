(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module STD_List = List;;
module Hash = Lockfree.Hash_Custom(struct
  let load = 3;;
  let nb_bucket = 526;;
  let hash_function i = i;;
end);;
module Queue = Lockfree.MSQueue;;

let print t = print_endline (Hash.to_string t (sprintf "%d"));;

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

let gen_queue l =
  let out = Queue.create () in
  let rec loop l =
    match l with
    |h::t -> Queue.push out h; loop t
    |[] -> out
  in loop l |> ignore; out
;;

let insert_hash t q =
  let rec loop () =
    match Queue.pop q with
    |Some(v) ->
    Hash.add t v v; loop ()
    |None -> ()
  in loop ()
;;

let insert_stdhash t l =
  let rec loop l =
    match l with
    |v::tl -> Hashtbl.add t v v; loop tl
    |[] -> ()
  in loop l
;;

let remove_hash t q =
  let rec loop () =
    match Queue.pop q with
    |Some(v) ->
      if Hash.remove t v then
        loop ()
      else
        loop ()
    |None -> ()
  in loop ()
;;

let benchmark f nb_thread message wait_time out verbose =
  let t1 = Unix.gettimeofday () in
  out := t1 *. 1000000.0;
  for _ = 1 to nb_thread do
    Domain.spawn (fun () ->
      f (); out := min !out (Unix.gettimeofday () -. t1); if verbose then print_endline (sprintf message (Domain.self () :> int))) |> ignore
  done;
  Unix.sleep wait_time;
  ()
;;

let benchmark_seq f =
  let t1 = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. t1
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

let run num_test verbose =
  print_endline (sprintf "TEST N°%d" num_test);

  let t1 = Hash.create () in
  let t2 = Hash.create () in
  let nb_thread = 2 in
  let nb_init = 100000 in
  let nb_end = 10 in
  let m = 1000000000 in
  let wait_time = 2 in
  let elem_init = gen_elem nb_init m in
  let elem_end  = gen_elem nb_end m in

  let q_init1 = gen_queue elem_init in
  let q_init2 = gen_queue elem_init in
  let q_end1 = gen_queue elem_end in
  let q_end2 = gen_queue elem_end in
  let q_remove1 = gen_queue elem_init in
  let q_remove2 = gen_queue elem_init in

  let t_par_ins = ref 0.0 in
  let t_par_ins2 = ref 0.0 in
  let t_par_rem = ref 0.0 in

  if verbose then print t1;
  if verbose then print t2;

  if verbose then print_endline (sprintf "NB Thread : %d" nb_thread);
  if verbose then print_endline (sprintf "Parallele insertion of %d elements" (List.length elem_init));
  benchmark (fun () -> insert_hash t1 q_init1) nb_thread "Insertion TH%d END" wait_time t_par_ins verbose;

  if verbose then print_endline (sprintf "Parallele insertion of %d elements and deletion of %d elements" (List.length elem_end) (List.length elem_init));
  benchmark (fun () -> insert_hash t1 q_end1) nb_thread "Insertion2 TH%d END" 0 t_par_ins2 verbose;
  benchmark (fun () -> remove_hash t1 q_remove1) nb_thread "Deletion TH%d END" wait_time t_par_rem verbose;


  if verbose then print_endline (sprintf "Beginning sequential insertion/deletion");
  if verbose then print_endline (sprintf "Sequential insertion of %d elements" (List.length elem_init));
  let t_seq_ins = benchmark_seq (fun () -> insert_hash t2 q_init2) in

  if verbose then print_endline (sprintf "Sequential insertion of %d elements" (List.length elem_end));
  let t_seq_ins2 = benchmark_seq (fun () -> insert_hash t2 q_end2) in

  if verbose then print_endline (sprintf "Sequential deletion of %d elements" (List.length elem_init));
  let t_seq_rem = benchmark_seq (fun () -> remove_hash t2 q_remove2) in

  if verbose then print_endline (sprintf "FINISHED");

  let elem_t1 = Hash.elem_of t1 in
  let elem_t2 = Hash.elem_of t2 in
  let elem_dif = dif_list elem_t1 elem_t2 in

  if verbose then begin
    print t1;
    print t2;
    printf "[";
    List.iter (fun i -> printf "%d, " i) (STD_List.sort compare elem_end); print_endline "]";
    printf "[";
    List.iter (fun i -> printf "%d, " i) (STD_List.sort compare elem_t1); print_endline "]";
    printf "[";
    List.iter (fun i -> printf "%d, " i) (STD_List.sort compare elem_t2); print_endline "]";
    printf "[";
    List.iter (fun i -> printf "%d, " i) (STD_List.sort compare elem_dif); print_endline "]";
    print_endline (sprintf "T1    Queue IN Empty : %b    Queue OUT Empty : %b    Queue REMAIN Empty : %b" (Queue.is_empty q_init1) (Queue.is_empty q_remove1) (Queue.is_empty q_end1));
    print_endline (sprintf "T2    Queue IN Empty : %b    Queue OUT Empty : %b    Queue REMAIN Empty : %b" (Queue.is_empty q_init2) (Queue.is_empty q_remove2) (Queue.is_empty q_end2));
    print_endline (sprintf "Parallel Time :   %f (insertion : %f) (insertion2 : %f) (deletion : %f)" (!t_par_ins +. !t_par_ins2 +. !t_par_rem) !t_par_ins !t_par_ins2 !t_par_rem);
    print_endline (sprintf "Sequential Time : %f (insertion : %f) (insertion2 : %f) (deletion : %f)" (t_seq_ins +. t_seq_ins2 +. t_seq_rem) t_seq_ins t_seq_ins2 t_seq_rem)
  end;

  elem_dif
;;

let benchmark_insertion nb n _ =
  let rec loop i out =
    if i < nb then
      let _ = false in
      let t = Hashtbl.create 512 in
      let m = n * 1000 in
      let elem_l = gen_elem n m in
      loop (i+1) (out +. benchmark_seq (fun () -> insert_stdhash t elem_l))
    else
      out /. (float_of_int nb)
  in loop 0 0.0
;;

let test nb_test verbose =
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

(* Test *)
let () = test 1 true;;

(* Insertion Benchmark *)
(*let () =
  let print_usage_and_exit () =
    print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <num_items>";
    exit(0)
  in
  let num_items =
    if Array.length Sys.argv < 2 then
      print_usage_and_exit ()
    else try
      let a = int_of_string (Sys.argv.(1)) in
      a
    with Failure _ -> print_usage_and_exit ()
  in
  let nb = 5 in
  let n = num_items in
  let t = benchmark_insertion nb n false in
  print_endline (sprintf "%f" t)
;;*)
