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

module Bag = Lf_bag.M;;

module Bst = Lf_mybst.Make(struct
  type t = int;;
  let compare = compare;;
  let hash_t x = (x * 104743) mod 1299721;;
  let str k = sprintf "%d" k;;
end);;

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

let test_bag () =
  let b = Bag.create () in
  let n1 = 10 in
  let n2 = 20 in

  Random.self_init ();

  Domain.spawn (fun () -> push_and_pop b n1 0);
  Domain.spawn (fun () -> push_and_pop b n2 1);

  Unix.sleep 10
;;

let print_l l str =
  let rec loop l =
    match l with
    |(hk, k, v)::t -> print_string (sprintf str hk k v); loop t
    |[] -> print_endline "]"
  in
  print_string "[";
  loop l
;;

let insert_bst t n =
  let rec loop i =
    if i < n then begin
      Bst.insert t i i;
      loop (i+1)
    end
  in loop 0
;;

let delete_bst t n =
  let rec loop i =
    if i < n then begin
      Bst.delete t i;
      loop (i+2)
    end
  in loop 0
;;

let test_bst () =
  let t = Bst.create () in
  let n = 100000 in

  Domain.spawn (fun () -> insert_bst t n);
  Unix.sleep 1;
  Domain.spawn (fun () -> delete_bst t n);
  Unix.sleep 10;
  let h = Bst.heigh t in

  (*let h_init = Bst.heigh t in
  insert_bst t n;
  let h_insert = Bst.heigh t in
  delete_bst t n;
  let h_delete = Bst.heigh t in
*)

  let l = Bst.to_list t in
  let nature = Bst.still_bst t in
  print_l l "(%d, %d, %d) ; ";
  (*print_endline (sprintf "Init : %d  Insert : %d  Delete : %d" h_init h_insert h_delete);*)
  print_endline (sprintf "Heigh: %d" h);
  print_endline (sprintf "Still a BST : %b" nature);
  ()
;;

let () =
  (* test_bag () *)
  test_bst ()
;;
