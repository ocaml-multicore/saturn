(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

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
  let wait_time = 10 in

  Domain.spawn (fun () -> push_and_pop q n prob_m prob_n);
  for i = 1 to nb_thread do
    Domain.spawn (fun () -> steal_n q n)
  done;

  Unix.sleep wait_time;

  ()
;;

let () = run ();;
