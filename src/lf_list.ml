(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module type S = sig
  type 'a t;;
  val equal    : 'a t -> 'a t -> bool;;
  val print    : 'a t -> ('a -> unit) -> unit;;
  val create   : unit -> 'a t;;
  val cons     : 'a -> 'a t -> unit;;
  val push     : 'a -> 'a t -> unit;;
  val pop      : 'a t -> 'a option;;
  val is_empty : 'a t -> bool;;
  val mem      : 'a t -> 'a -> bool;;
  val sinsert  : 'a t -> 'a -> ('a -> 'a -> int) -> 'a t;;
  val sdelete  : 'a t -> 'a -> ('a -> 'a -> int) -> bool;;
end;;

module M : S = struct
  module Cas = Kcas.W1;;

  exception Head_Of_List;;
  exception End_Of_List;;
  exception Not_Found;;

  type 'a t = (bool * 'a node) Cas.ref
  and 'a node =
    |Node of 'a comparable * 'a t
    |Nil
  and 'a comparable =
    |Min
    |Max
    |Val of 'a
  ;;

  let mk_compare f a b =
    match a, b with
    |Min, Min |Max, Max -> 0
    |Min, Max |Min, Val(_) |Val(_), Max -> -1
    |Max, Min |Max, Val(_) |Val(_), Min -> 1
    |Val(x), Val(y) -> f x y
  ;;

  let get_status vn = fst vn;;
  let get_v vn =
    match snd vn with
    |Node(v, _) -> v
    |Nil -> failwith "Lock_Free_List.get_v: impossible"
  ;;
  let get_next vn =
    match snd vn with
    |Node(_, next) -> next
    |Nil -> failwith "Lock_Free_List.get_next: impossible"
  ;;

  let print l f =
    let rec loop l =
      match Cas.get l with
      |_, Node(v, next) ->
        (match v with
         |Min -> printf "Min ; "
         |Max -> printf "Max ; "
         |Val(x) -> f x; printf " ; ");
        loop next
      |_, Nil -> ()
    in
    printf "[";
    loop l;
    printf "]";
    print_endline ""
  ;;

  let equal_val v1 v2 =
    match v1, v2 with
    |Max, Max |Min, Min -> true
    |Val(x), Val(y) -> x = y
    |_ -> false
  ;;

  let rec equal l1 l2 =
    match Cas.get l1, Cas.get l2 with
    |(s1, Node(v1, next1)), (s2, Node(v2, next2)) -> s1 = s2 && equal_val v1 v2 && equal next1 next2
    |(s1, Nil), (s2, Nil) -> s1 = s2
    |_ -> false
  ;;

  let create () = Cas.ref (false, Node(Min, Cas.ref (false, Node(Max, Cas.ref (false, Nil)))));;

  let cons v l =
    match Cas.get l with
    |_, Nil -> failwith "Lock_Free_List.cons: impossible"
    |_, Node(_, next) ->
      let vnext = Cas.get next in
      Cas.set next (false, Node(Val(v), Cas.ref vnext))
  ;;

  let rec push v l =
    let b = Kcas.Backoff.create () in
    let rec loop () =
      match Cas.get l with
      |_, Nil -> failwith "Lock_Free_List.push: impossible"
      |_, Node(_, next) ->
        let vnext = Cas.get next in
        if not (Cas.cas next vnext (false, Node(Val(v), Cas.ref vnext))) then begin
          Kcas.Backoff.once b; loop ()
        end
    in loop ()
  ;;

  let pop l =
    let b = Kcas.Backoff.create () in
    let rec loop () =
      match Cas.get l with
      |_, Nil -> failwith "Lock_Free_List.pop: impossible"
      |_, Node(_, next) -> begin
        match Cas.get next with
        |_, Nil -> failwith "Lock_Free_List.pop: impossible"
        |_, Node(out, next') as old -> begin
          match out with
          |Val(return) ->
            if Cas.cas next old (Cas.get next') then
              Some(return)
            else begin
              Kcas.Backoff.once b; loop ()
            end
          |Max -> None
          |Min -> failwith "Lock_Free_List.pop: impossible"
        end
      end
    in loop ()
  ;;

  let is_empty l =
    match Cas.get l with
    |_, Node(Min, next) -> begin
      match Cas.get next with
      |_, Nil -> failwith "Lock_Free_List.is_empty: impossible"
      |_, Node(Max, _) -> true
      |_ -> false
    end
    |_ -> failwith "Lock_Free_List.is_empty: impossible"
  ;;

  let mem l v =
    let v = Val(v) in
    let rec loop l =
      match Cas.get l with
      |_, Node(v', next) -> equal_val v v' || loop next
      |_ -> false
    in loop l
  ;;

  let sfind l v f =
    let compare = mk_compare f in
    let rec loop prev vprev n =
      match Cas.get n with
      |_, Nil -> failwith "Lock_Free_List.sfind: impossible"
      |status, Node(v', next') as vn ->
        if compare v v' <= 0 then begin
          (prev, vprev, n, vn)
        end else
          loop n vn next'
    in
    match Cas.get l with
    |_, Nil -> failwith "Lock_Free_List.sfind: impossible"
    |status, Node(v', next') as vl -> loop l vl next'
  ;;

  let sinsert l v f =
    let b = Kcas.Backoff.create () in
    let v = Val(v) in
    let compare = mk_compare f in
    let rec loop () =
      let (prev, vprev, n, vn) = sfind l v f in
      let (new_node, out) =
(*        if compare v (get_v vn) = 0 then
          ((get_status vprev, Node(get_v vprev, Cas.ref (false, Node(v, get_next vn)))), get_next vn)
        else
          ((get_status vprev, Node(get_v vprev, Cas.ref (false, Node(v, n)))), n)*)
        ((get_status vprev, Node(get_v vprev, Cas.ref (false, Node(v, n)))), n)
      in
      if get_status vprev || not (Cas.cas prev vprev new_node) then begin
        Kcas.Backoff.once b; loop ()
      end else
        out
    in loop ()
  ;;

  let sdelete l v f =
    let b = Kcas.Backoff.create () in
    let v = Val(v) in
    let compare = mk_compare f in
    let rec loop () =
      let (prev, vprev, n, vn) = sfind l v f in
      let marked_vn = (true, snd vn) in
      if compare v (get_v vn) = 0 then
        if Cas.cas n vn marked_vn then
          if not (Cas.cas n marked_vn (Cas.get (get_next vn))) then begin
            Cas.set n vn; Kcas.Backoff.once b; loop ()
          end else true
        else begin
          Kcas.Backoff.once b; loop ()
        end
      else false
    in loop ()
  ;;
end;;
