(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module Queue = Lf_msqueue.M;;

module Bst = Lf_bst.Make(struct
  type t = int;;
  let compare = compare;;
  let hash_t x = (x * 104743) mod 1299721;;
  (*let hash_t x = x;;*)
  let str k = sprintf "%d" k;;
end);;

module MySet = Set.Make(struct
  type t = int;;
  let compare = compare;;
end);;

let rand_bool m n =
  let r = Random.int n in r < m
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
  in loop l; out
;;

let gen_elem_out l m n =
  let rec loop l out =
    match l with
    |h::t ->
      if rand_bool m n then
        loop t (h::out)
      else
        loop t out
    |[] -> out
  in loop l []
;;

let print_l l f =
  let rec loop l =
    match l with
    |h::t -> print_string (f h); loop t
    |[] -> print_endline "]"
  in
  print_string "[";
  loop l
;;

let insert_bst t q =
  let rec loop () =
    match Queue.pop q with
    |Some(v) -> Bst.insert t v v; loop ()
    |None -> ()
  in loop ()
;;

let delete_bst t q =
  let rec loop () =
    match Queue.pop q with
    |Some(v) -> Bst.delete t v; loop ()
    |None -> ()
  in loop ()
;;

let check_elem tree_elems elems_in_s elems_out_s =
  let rec loop l =
    match l with
    |h::t ->
      if not (MySet.mem h elems_out_s) then
        loop t
      else
        (print_endline (sprintf "Echec %d haven't been deleted !" h); false)
    |[] -> true
  in loop tree_elems
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

let run () =
  let t = Bst.create () in
  let n = 1000000 in
  let m = 1000000000 in
  let prob_m = 4 in
  let prob_n = 5 in
  let thread_insert = 5 in
  let thread_delete = 5 in
  let wait_time = 15 in

  let elems = gen_elem n m in
  let elems_out = gen_elem_out elems prob_m prob_n in
  let elems_s = MySet.of_list elems in
  let elems_out_s = MySet.of_list elems_out in
  let queue_in = gen_queue elems in
  let queue_out = gen_queue elems_out in
  let nb_elem_in = List.length elems in
  let nb_elem_out = List.length elems_out in

  print_endline "Insertion Phase";
  let t_par_ins = benchmark (fun () -> insert_bst t queue_in) thread_insert "Insertion TH%d END" wait_time in

  print_endline "Deletion Phase";
  let t_par_del = benchmark (fun () -> delete_bst t queue_out) thread_delete "Deletion TH%d END" wait_time in


  print_endline "Checking Phase";
  let h = Bst.heigh t in
  let l = Bst.to_list t in
  let nb_elem_final = List.length l in
  let tree_elems = List.rev_map (fun (hk, k, v) -> k) l in
  let nature = Bst.still_bst t in
  let test_success = check_elem tree_elems elems_s elems_out_s in

  print_l l (fun (hk, k, v) -> sprintf "%d ; " hk);
  print_endline (sprintf "Heigh: %d" h);
  print_endline (sprintf "Nb elems list: %d (begin: %d) (removed: %d) (expected: %d)" nb_elem_final nb_elem_in nb_elem_out (nb_elem_in - nb_elem_out));
  print_endline (sprintf "Still a BST : %b    Queue IN Empty : %b    Queue OUT Empty : %b" nature (Queue.is_empty queue_in) (Queue.is_empty queue_out));
  print_endline (sprintf "Test succeed : %b" test_success);
  print_endline (sprintf "Insertion time : %f" t_par_ins);
  print_endline (sprintf "Deletion time  : %f" t_par_del);
  let gc_stat = Gc.stat () in
  print_endline (sprintf "Number of Minor words %f" gc_stat.minor_words);
  ()
;;

let () = run ();;
