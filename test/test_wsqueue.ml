(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module Wsqueue = Lockfree.WSQueue;;

let print q v =
  match v with
  |Some(i) -> print_endline (sprintf "Th%d : %d (size %d)" (Domain.self () :> int) i (Wsqueue.size q))
  |None -> print_endline (sprintf "Th%d : None (size %d)" (Domain.self () :> int) (Wsqueue.size q))
;;

let insert q n =
  let rec loop i =
    if i <= n then begin
      Wsqueue.push q i;
      loop (i+1)
    end
  in loop 1
;;

let remove_own q n =
  let rec loop i =
    if i <= n then begin
      match Wsqueue.pop q with
      |Some(_) -> loop (i+1)
      |None -> false
    end else
      true
  in loop 1
;;

let remove_other q n =
  let rec loop i =
    if i <= n then begin
      match Wsqueue.steal q with
      |Some(_) -> loop (i+1)
      |None -> false
    end else
      true
  in loop 1
;;

let run () =
  let q = Wsqueue.create () in
  let n_insert = 1000000 in
  let n_remove = 50000 in
  let nb_thread = (n_insert / n_remove) in
  let wait_time = 10 in

  let rec loop f =
    f (); loop f
  in

  Domain.spawn (fun () -> loop (fun () -> insert q n_insert; ignore(remove_own q n_remove); print_endline (sprintf "TH%d OWNER size %d" (Domain.self () :> int) (Wsqueue.size q)))) |> ignore;
  for _ = 1 to nb_thread do
    Domain.spawn (fun () -> loop (fun () -> ignore(remove_other q n_remove); print_endline (sprintf "TH%d OTHER size %d" (Domain.self () :> int) (Wsqueue.size q)))) |> ignore
  done;

  Unix.sleep wait_time;

  ()
;;

let () = run ();;
