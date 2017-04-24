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
  module Bag = Lf_bag.M;;

  let push_test b n init =
    let rec loop i =
      if i < n then begin
        Bag.push b i; loop (i+2)
      end
    in loop init
  ;;

  let rec pop_test b n =
    if n > 0 then
      match Bag.pop b with
      |Some(out) -> print_endline (sprintf "TH%d : %d" (Domain.self ()) out); pop_test b (n-1)
      |None -> print_endline (sprintf "TH%d : Vide" (Domain.self ())); pop_test b (n-1)
  ;;

  let rec push_and_pop b n init =
    let rand1 = Random.int n in
    let rand2 = Random.int n in
    push_test b (n + rand1) init;
    pop_test b (n + rand2);
    push_and_pop b n init
  ;;

  let run () =
    let b = Bag.create () in
    let n1 = 10 in
    let n2 = 20 in

    Random.self_init ();

    Domain.spawn (fun () -> push_and_pop b n1 0);
    Domain.spawn (fun () -> push_and_pop b n2 1);

    Unix.sleep 10
  ;;
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



let () =
  (* Bag_test.run () *)
  Wsqueue_test.run ()
;;
