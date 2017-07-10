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
  val create : unit -> 'a t;;
  val is_empty : 'a t -> bool;;
  val push : 'a t -> 'a -> unit;;
  val pop : 'a t -> 'a option;;
end;;

module Make(Desc : CoreDesc) : S = struct
  module STD_List = List;;
  module Queue = Lf_wsqueue.M;;
  module Hash = Lf_hash.Make(struct
    let load = 3;;
    let nb_bucket = max 8 Desc.nb_domains;;
    let hash_function x = x;;
  end);;

  type 'a t = ('a Queue.t) Hash.t;;

  let nb_domains = 1 + Desc.nb_domains;;

  let create () = Random.self_init (); Hash.create ();;

  let is_empty b =
    let rec loop l =
      match l with
      |q::t -> Queue.is_empty q && loop t
      |[] -> true
    in loop (Hash.elem_of b)
  ;;

  let push b v =
    match Hash.find b (Domain.self ()) with
    |Some(q) -> Queue.push q v
    |None ->
      let q = Queue.create () in
      Queue.push q v;
      Hash.add b (Domain.self ()) q
  ;;

  let shuffle l =
    let rand_l = List.map (fun i -> (Random.bits (), i)) l in
    let shuffle_l = List.sort (fun x y -> compare (fst x) (fst y)) rand_l in
    List.map snd shuffle_l
  ;;

  let rec pop b =
    match Hash.find b (Domain.self ()) with
    |Some(q) -> begin
      match Queue.pop q with
      |Some(_) as out -> out
      |None -> steal b
    end
    |None -> steal b
  and steal b =
    let rec loop l =
      match l with
      |q::t -> begin
        match Queue.steal q with
        |Some(_) as out -> out
        |None -> loop t
      end
      |[] -> None
    in loop (shuffle (Hash.elem_of b))
  ;;
end;;
