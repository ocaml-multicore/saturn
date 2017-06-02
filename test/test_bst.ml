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

let run () =
  let t = Bst.create () in
  let n = 100000 in
  let m = 1000000 in
  let prob_m = 4 in
  let prob_n = 5 in
  let thread_insert = 5 in
  let thread_delete = 5 in
  let wait_time = 5 in

  let elems = gen_elem n m in
  let elems_out = gen_elem_out elems prob_m prob_n in
  let elems_s = MySet.of_list elems in
  let elems_out_s = MySet.of_list elems_out in
  let queue_in = gen_queue elems in
  let queue_out = gen_queue elems_out in

  print_endline "Insertion Phase";
  for i = 1 to thread_insert do
    Domain.spawn (fun () -> insert_bst t queue_in)
  done;

  Unix.sleep wait_time;

  print_endline "Deletion Phase";
  for i = 1 to thread_delete do
    Domain.spawn (fun () -> delete_bst t queue_out)
  done;

  Unix.sleep wait_time;

  print_endline "Checking Phase";
  let h = Bst.heigh t in
  let l = Bst.to_list t in
  let tree_elems = List.rev_map (fun (hk, k, v) -> k) l in
  let nature = Bst.still_bst t in
  let test_success = check_elem tree_elems elems_s elems_out_s in

  print_l l (fun (hk, k, v) -> sprintf "%d ; " hk);
  print_endline (sprintf "Heigh: %d" h);
  print_endline (sprintf "Still a BST : %b    Queue IN Empty : %b    Queue OUT Empty : %b" nature (Queue.is_empty queue_in) (Queue.is_empty queue_out));
  print_endline (sprintf "Test succeed : %b" test_success);
  ()
;;

let () = run ();;
