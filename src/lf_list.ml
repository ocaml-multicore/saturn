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
  val to_string: 'a t -> ('a -> string) -> string;;
  val create   : unit -> 'a t;;
  val cons     : 'a -> 'a t -> unit;;
  val push     : 'a t -> 'a -> unit;;
  val pop      : 'a t -> 'a option;;
  val is_empty : 'a t -> bool;;
  val mem      : 'a t -> 'a -> ('a -> 'a -> int) -> bool;;
  val find     : 'a t -> 'a -> ('a -> 'a -> int) ->'a option;;
  val sinsert  : 'a t -> 'a -> ('a -> 'a -> int) -> 'a t;;
  val sdelete  : 'a t -> 'a -> ('a -> 'a -> int) -> bool;;
  val elem_of  : 'a t -> 'a list;;
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

  let extract_comparable v =
    match v with
    |Min |Max -> None
    |Val(out) -> Some(out)
  ;;

  let get_mark vn = fst vn;;
(*  let get_v vn =
    match snd vn with
    |Node(v, _) -> v
    |Nil -> failwith "Lock_Free_List.get_v: impossible"
  ;;*)
(*  let get_next vn =
    match snd vn with
    |Node(_, next) -> next
    |Nil -> failwith "Lock_Free_List.get_next: impossible"
  ;;*)

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

  let string_of_mark m = if m then "x>" else "->";;

  let to_string l f =
    let buf = Buffer.create 17 in
    let rec loop l =
      match Cas.get l with
      |mark, Node(v, next) ->
        (match v with
         |Min -> Buffer.add_string buf ((string_of_mark mark) ^ "Min ; ")
         |Max -> Buffer.add_string buf ((string_of_mark mark) ^ "Max ; ")
         |Val(x) -> Buffer.add_string buf ((string_of_mark mark) ^ (sprintf "%s ; " (f x))));
        loop next
      |_, Nil -> ()
    in
    Buffer.add_string buf "[";
    loop l;
    Buffer.add_string buf "]\n";
    Buffer.contents buf
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

  let rec push l v =
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

  let mem l v f =
    let compare = mk_compare f in
    let v = Val(v) in
    let rec loop l =
      match Cas.get l with
      |_, Node(v', next) ->
        if compare v v' = 0 then
          true
        else if compare v v' > 0 then
          loop next
        else
          false
      |_ -> false
    in loop l
  ;;

  let find l v f =
    let compare = mk_compare f in
    let v = Val(v) in
    let rec loop l =
      match Cas.get l with
      |_, Node(v', next) ->
        if compare v v' = 0 then
          extract_comparable v'
        else if compare v v' > 0 then
          loop next
        else
          None
      |_ -> None
    in loop l
  ;;

  let sfind l v f =
    let compare = mk_compare f in
    let rec loop prev vprev n =
      match Cas.get n with
      |_, Nil -> print_endline "ERREUR MOYENNE"; failwith "Lock_Free_List.sfind: impossible"
      |status, Node(v', next') as vn ->
        if compare v v' <= 0 then begin
          (prev, vprev, n, vn)
        end else
          loop n vn next'
    in
    match Cas.get l with
    |_, Nil -> print_endline "Grosse ERREUR"; failwith "Lock_Free_List.sfind: impossible"
    |status, Node(v', next') as vl -> loop l vl next'
  ;;

  let sinsert l v f =
    let b = Kcas.Backoff.create () in
    let v = Val(v) in
    let compare = mk_compare f in
    let rec loop (prev, vprev, n, vn) =
      let new_node = (false, Node(v, Cas.ref vn)) in
      match snd vprev with
      |Node(vprev_v, vprev_next) ->
        if not (Cas.cas vprev_next vn new_node) then
          (Kcas.Backoff.once b; loop (sfind l v f))
        else
          vprev_next
      |Nil -> failwith "Lock_Free_List.sinsert: impossible"
    in loop (sfind l v f)
  ;;

(*  let sinsert l v f =
    let b = Kcas.Backoff.create () in
    let v = Val(v) in
    let compare = mk_compare f in
    let rec loop () =
      let (prev, vprev, n, vn) = sfind l v f in
      let new_node = Cas.ref (false, Node(v, n)) in
      let new_prev = (get_status vprev, Node(get_v vprev, new_node)) in
      if get_status vprev || not (Cas.cas prev vprev new_prev) then begin
        Kcas.Backoff.once b; loop ()
      end else
        new_node
    in loop ()
  ;;*)

  let marked node =
    match Cas.get node with
    |_, (Node(_, _) as vvnode) as vnode -> (vnode, (true, vvnode))
    |_ -> failwith "Lock_Free_List.marked: impossible"
  ;;

  let sdelete l v f =
(*    print_endline (sprintf "TH%d : SDELETE" (Domain.self ()));*)
    let b = Kcas.Backoff.create () in
    let v = Val(v) in
    let compare = mk_compare f in
    let rec loop (prev, vprev, n, vn) =
(*      print_endline (sprintf "TH%d : SDELETE LOOP" (Domain.self ()));*)
      match snd vn with
      |Node(vn_v, vn_next) ->
        if compare v vn_v = 0 then
          let (vn_next_v, marked_vn_next_v) = marked vn_next in
          if Cas.cas vn_next vn_next_v marked_vn_next_v then begin
            match snd vprev with
            |Node(vprev_v, vprev_next) ->
              if (*get_mark (Cas.get vprev_next) || *)not (Cas.cas vprev_next vn vn_next_v) then
                (
(*                print_endline (sprintf "TH%d : SDELETE (MARKED ALREADY ? %b)" (Domain.self ()) (get_mark (Cas.get vprev_next)));*)
                Cas.set vn_next vn_next_v; Kcas.Backoff.once b; loop (sfind l v f))
              else
                true
            |Nil -> failwith "Lock_Free_List.sdelete: impossible"
          end else
            (
(*            print_endline (sprintf "TH%d : SDELETE CAN'T MARK" (Domain.self ()));*)
            Kcas.Backoff.once b; loop (sfind l v f))
        else
          (
(*          print_endline (sprintf "TH%d : SDELETE NOT FOUND" (Domain.self ()));*)
          false)
      |Nil -> failwith "Lock_Free_List.sdelete: impossible"
    in loop (sfind l v f)
  ;;
(*
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
  ;;*)

  let elem_of l =
    let rec loop l out =
      match Cas.get l with
      |_, Node(v, next) -> begin
        match v with
        |Val(x) -> loop next (x::out)
        |_ -> loop next out
      end
      |_, Nil -> out
    in loop l []
  ;;
end;;
