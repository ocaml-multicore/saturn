(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module Bag = Lockfree.Bag(struct let nb_domains = 3 end);;
module Cas = Kcas.W1;;

let content = Cas.ref 0;;

let insert b n =
  let rec loop i =
    if i <= n then begin
      Bag.push b i; loop (i+1)
    end
  in loop 1
;;

let remove_own b n =
  let rec loop i =
    if i <= n then begin
      match Bag.pop b with
      |Some(v) -> loop (i+1)
      |None -> false
    end else
      true
  in loop 1
;;

let remove b n =
  let rec loop i =
    if i <= n then begin
      match Bag.pop b with
      |Some(v) -> loop (i+1)
      |None -> false
    end else
      true
  in loop 1
;;

let run () =
  let b = Bag.create () in
  let n_insert = 100000 in
  let n_remove = 10000 in
  let nb_thread = n_insert / n_remove in
  let wait_time = 10 in

  let rec loop f =
    f (); loop f
  in

  Domain.spawn (fun () -> loop (fun () -> insert b n_insert; remove_own b n_remove; print_endline (sprintf "TH%d INSERT/REMOVE empty : %b" (Domain.self ()) (Bag.is_empty b))));
  for i = 1 to nb_thread do
    Domain.spawn (fun () -> loop (fun () -> remove b n_remove; print_endline (sprintf "TH%d REMOVE size %b" (Domain.self ()) (Bag.is_empty b))))
  done;

  Unix.sleep wait_time;
;;

let () = run ();;
