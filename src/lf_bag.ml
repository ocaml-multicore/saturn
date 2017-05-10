(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module type CoreDesc = sig
  val nb_domains : int;;
end;;

module type S = sig
  type 'a t;;
  exception To_Many_Domain of string;;
  val create : unit -> 'a t;;
  val is_empty : 'a t -> bool;;
  val push : 'a t -> 'a -> unit;;
  val pop : 'a t -> 'a option;;
end;;

module Make(Desc : CoreDesc) : S = struct
  module Queue = Lf_wsqueue.M;;

  exception To_Many_Domain of string;;

  type 'a t = 'a Queue.t array;;

  let nb_domains = Desc.nb_domains;;

  let create () = Random.self_init (); Array.init nb_domains (fun i -> Queue.create ());;

  let get_thread_id b =
    let out = Domain.self () in
    if out < Array.length b then
      out
    else
      raise (To_Many_Domain(sprintf "Domain n°%d try to use a %d domains bag" out (Array.length b)))
  ;;

  let is_empty b =
    let rec loop i =
      if i >= Array.length b then
        true
      else
        (Queue.is_empty b.(i)) && (loop (i+1))
    in loop 0
  ;;

  let push b v =
    let th_id = get_thread_id b in
    Queue.push (b.(th_id)) v
  ;;

  let make_steal_list id =
    let out = Array.init (nb_domains - 1) (fun i -> if i < id then i else i+1) in
    for i = 0 to 2*nb_domains do
      let i1 = Random.int (nb_domains - 1) in
      let i2 = Random.int (nb_domains - 1) in
      let tmp = out.(i1) in
      out.(i1) <- out.(i2);
      out.(i2) <- tmp
    done;
    Array.to_list out
  ;;

  let rec pop b =
    let th_id = get_thread_id b in
    match Queue.pop (b.(th_id)) with
    |Some(_) as out -> out
    |None -> steal th_id b
  and steal th_id b =
    let rec loop l =
      match l with
      |ind::tl -> begin
        match Queue.steal (b.(ind)) with
        |Some(_) as out -> out
        |None -> loop tl
      end
      |[] -> None
    in loop (make_steal_list th_id)
  ;;
end;;
