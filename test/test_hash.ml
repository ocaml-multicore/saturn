(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module Hash = Lf_hash.M;;
module Queue = Lf_msqueue.M;;
module Cas = Kcas.W1;;

let print t = print_endline (Hash.to_string t);;

let cprint t = print_endline (Hash.to_string_clean t);;

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
  |hd::tl -> Hash.add t hd hd; insert_hash_l t tl
;;

let remove_hash t q =
  let rec loop () =
    match Queue.pop q with
    |Some(v) -> Hash.remove t v; loop ()
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
  let nb_init = 100000 in
  let nb_end = 1000 in
  let m = 100000000 in
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
  let sleep_time = 4 in

  print_endline (sprintf "NB Thread : %d" nb_thread);
  print_endline (sprintf "Parallele insertion of %d elements" (List.length elem_init));
  Domain.spawn (fun () -> insert_hash_l t1 odd; print_endline (sprintf "Insertion TH%d ODD END" (Domain.self ())));
  Domain.spawn (fun () -> insert_hash_l t1 even; print_endline (sprintf "Insertion TH%d EVEN END" (Domain.self ())));
(*
  let rec loop l =
    match l with
    |[] -> ()
    |hd::tl ->
      Domain.spawn (fun () -> insert_hash_l t1 hd; print_endline (sprintf "Insertion TH%d END" (Domain.self ())));
      loop tl
  in loop l_elem_init;*)
(*
  for i = 1 to nb_thread do
    Domain.spawn (fun () -> insert_hash t1 q_init1; print_endline (sprintf "Insertion TH%d END" (Domain.self ())))
  done;
*)
  Unix.sleep sleep_time;
(*
  print_endline (sprintf "Parallele insertion of %d elements and deletion of %d elements" (List.length elem_end) (List.length elem_init));

  for i = 1 to nb_thread do
    if i mod 2 = 0 then
      Domain.spawn (fun () -> insert_hash t1 q_end1; print_endline (sprintf "Final insertion TH%d END" (Domain.self ())))
    else
      Domain.spawn (fun () -> remove_hash t1 q_remove1; print_endline (sprintf "Deletion TH%d END" (Domain.self ())))
  done;

  Unix.sleep sleep_time;*)
  print_endline (sprintf "Insertion of %d elements" (List.length elem_init));

  insert_hash_l t2 elem_init;
  print_endline (sprintf "Insertion of %d elements END" (List.length elem_init));
  Unix.sleep 2;
(*
  print_endline (sprintf "Insertion of %d elements" (List.length elem_end));
  insert_hash t2 q_end2;

  print_endline (sprintf "Deletion of %d elements" (List.length elem_init));
  remove_hash t2 q_remove2;
*)
  print t1;
  print t2;
  (*print_endline "Elem Init :";
  List.iter (fun i -> printf "%d, " i) elem_init; print_endline "";
  print_endline "Elem End :";
  List.iter (fun i -> printf "%d, " i) elem_end; print_endline "";*)
  cprint t1;
  cprint t2;
  print_endline (sprintf "Are they equal ? %b" (Hash.equal t1 t2));
  print_endline (sprintf "Still valid split order ? %b" (Hash.still_split_order t1));
  print_endline (sprintf "T1 Elem valid ? %b" (check_valid t1 elem_end elem_init));
  print_endline (sprintf "T2 Elem valid ? %b" (check_valid t2 elem_end elem_init));
  (*List.iter (fun l -> List.iter (fun i -> printf "%d, " i) l) l_elem_init; print_endline "";*)
  ()
;;

let () = run ();;
