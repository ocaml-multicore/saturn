(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module Bag = Lockfree.Bag(struct let nb_domains = 3 end);;
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
;;

let () = run ();;
