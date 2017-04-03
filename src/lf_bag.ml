(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  type 'a t;;
  val create : unit -> 'a t;;
  val is_empty : 'a t -> bool;;
  val push : 'a t -> 'a -> unit;;
  val pop : 'a t -> 'a option;;
end;;

module M : S = struct

  module Queue = Lf_msqueue.M;;

  type 'a t = 'a Queue.t array;;

  let create_bis len = Array.init len (fun i -> Queue.create ());;

  let create () = create_bis 10;;

  let get_thread_id b = (Domain.self ()) mod (Array.length b);;

  let push b v =
    let th_id = get_thread_id b in
    Queue.push (b.(th_id)) v
  ;;

  let make_ind_list_of a id =
    let out = ref [] in
    for i = 0 to Array.length a - 1 do
      if i <> id then
        out := i::(!out)
    done;
    !out
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
        match Queue.pop (b.(ind)) with
        |Some(_) as out -> out
        |None -> loop tl
      end
      |[] -> None
    in
    loop (make_ind_list_of b th_id)
  ;;

  let is_empty b =
    let rec loop i =
      if i >= Array.length b then
        true
      else
        (Queue.is_empty b.(i)) && (loop (i+1))
    in loop 0
  ;;
end;;
