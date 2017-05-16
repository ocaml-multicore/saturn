(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

open Printf;;

module Bag_test = struct
  module Bag = Lf_bag.Make(struct let nb_domains = 3 end);;
  module Cas = Kcas.W1;;

  let content = Cas.ref 0;;

  let push_test b n =
    let rec loop i =
      if i < n then begin
        Bag.push b i; Cas.incr content; loop (i+1)
      end
    in loop 0
  ;;

  let rec pop_test b n =
    let back = Kcas.Backoff.create () in
    let rec loop n =
      if n > 0 then
        match Bag.pop b with
        |Some(out) -> Cas.decr content; pop_test b (n-1)
        |None -> Kcas.Backoff.once back; pop_test b n
    in loop n
  ;;

  let rec push_and_pop b ins del =
    push_test b ins;
    pop_test b del
  ;;

  let run () =
    let b = Bag.create () in
    let ins1 = 2000 in
    let del1 = 3000 in
    let ins2 = 8000 in
    let del2 = 7000 in
    let ended = Cas.ref 0 in

    print_endline (sprintf "THREAD1 Insertion/Deletion (%d/%d)" ins1 del1);
    print_endline (sprintf "THREAD2 Insertion/Deletion (%d/%d)" ins2 del2);

    Random.self_init ();

    Domain.spawn (fun () -> push_and_pop b ins1 del1; Cas.incr ended);
    Domain.spawn (fun () -> push_and_pop b ins2 del2; Cas.incr ended);

    Unix.sleep 1;
    let c = Cas.get content in
    print_endline (sprintf "Content %d elements (completed : %b) (success : %b)" c (Cas.get ended = 2) (c = 0))
end;;


module BST_test = struct
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

  let gen_queue l =
    let out = Queue.create () in
    let rec loop l =
      match l with
      |h::t -> Queue.push out h; loop t
      |[] -> out
    in loop l; out
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

    Unix.sleep 5;

    print_endline "Deletion Phase";
    for i = 1 to thread_delete do
      Domain.spawn (fun () -> delete_bst t queue_out)
    done;

    Unix.sleep 5;

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
end;;


module Wsqueue_test = struct
  module Wsqueue = Lf_wsqueue.M;;

  let print q v =
    match v with
    |Some(i) -> print_endline (sprintf "Th%d : %d (size %d)" (Domain.self ()) i (Wsqueue.size q))
    |None -> print_endline (sprintf "Th%d : None (size %d)" (Domain.self ()) (Wsqueue.size q))
  ;;

  let push_and_pop q n prob_m prob_n =
    let rec loop i =
      if i <= n then
        if Random.int prob_n < prob_m then begin
          Wsqueue.push q i; loop (i+1)
        end else begin
          print q (Wsqueue.pop q); loop i
        end
    in loop 1
  ;;

  let push_n q n =
    let rec loop i =
      if i <= n then begin
        Wsqueue.push q i;
        loop (i+1)
      end
    in loop 1
  ;;

  let pop_n q n =
    let rec loop i =
      if i <= n then begin
        print q (Wsqueue.pop q);
        loop (i+1)
      end
    in loop 1
  ;;

  let steal_n q n =
    let rec loop i =
      if i <= n then begin
        print q (Wsqueue.steal q);
        loop (i+1)
      end
    in loop 1
  ;;

  let easy_test () =
    let q = Wsqueue.create () in
    let n = 40 in
    push_n q n;
    pop_n q (n/2);
    steal_n q (n/2)
  ;;

  let run () =
    let q = Wsqueue.create () in
    let n = 100000 in
    let prob_m = 1 in
    let prob_n = 2 in
    let nb_thread = 2 in

    Domain.spawn (fun () -> push_and_pop q n prob_m prob_n);
    for i = 1 to nb_thread do
      Domain.spawn (fun () -> steal_n q n)
    done;

    Unix.sleep 10;

    ()
  ;;
end;;



module Hash_test = struct
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

  let gen_queue l =
    let out = Queue.create () in
    let rec loop l =
      match l with
      |h::t -> Queue.push out h; loop t
      |[] -> out
    in loop l; out
  ;;

  let insert_hash t q =
    let rec loop () =
      match Queue.pop q with
      |Some(v) -> Hash.add t v v; loop ()
      |None -> ()
    in loop ()
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
    let nb_thread = 32 in
    let nb_init = 1000000 in
    let nb_end = 1000 in
    let m = 10000000 in
    let elem_init = gen_elem nb_init m in
    let elem_end  = gen_elem nb_end m in
    let q_init1 = gen_queue elem_init in
    let q_init2 = gen_queue elem_init in
    let q_end1 = gen_queue elem_end in
    let q_end2 = gen_queue elem_end in
    let q_remove1 = gen_queue elem_init in
    let q_remove2 = gen_queue elem_init in
    let sleep_time = 20 in

    print_endline (sprintf "NB Thread : %d" nb_thread);
    print_endline (sprintf "Parallele insertion of %d elements" (List.length elem_init));

    for i = 1 to nb_thread do
      Domain.spawn (fun () -> insert_hash t1 q_init1; print_endline (sprintf "Insertion TH%d END" (Domain.self ())))
    done;

    Unix.sleep sleep_time;

    print_endline (sprintf "Parallele insertion of %d elements and deletion of %d elements" (List.length elem_end) (List.length elem_init));

    for i = 1 to nb_thread do
      if i mod 2 = 0 then
        Domain.spawn (fun () -> insert_hash t1 q_end1; print_endline (sprintf "Final insertion TH%d END" (Domain.self ())))
      else
        Domain.spawn (fun () -> remove_hash t1 q_remove1; print_endline (sprintf "Deletion TH%d END" (Domain.self ())))
    done;

    Unix.sleep sleep_time;

    print_endline (sprintf "Insertion of %d elements" (List.length elem_init));

    insert_hash t2 q_init2;

    print_endline (sprintf "Insertion of %d elements" (List.length elem_end));
    insert_hash t2 q_end2;

    print_endline (sprintf "Deletion of %d elements" (List.length elem_init));
    remove_hash t2 q_remove2;

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
    ()
  ;;
end;;

let () =
  (* Bag_test.run () *)
  Hash_test.run ()
;;
