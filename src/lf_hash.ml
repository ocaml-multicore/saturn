(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module type S = sig
  type 'a t;;

  val to_string : 'a t -> string;;

  val create : unit -> 'a t;;

  val find : 'a t -> int -> 'a option;;

  val mem : 'a t -> int -> bool;;

  val add : 'a t -> int -> 'a -> unit;;

(*  val remove : 'a t -> int -> unit;;*)
end;;

module M : S = struct
  module Cas = Kcas.W1;;

  type 'a elem_list = bool * int * 'a * 'a node Cas.ref
  and 'a node =
    |Node of 'a elem_list
    |Nil
  ;;

  type 'a bucket =
    |Initialized of 'a elem_list
    |Uninitialized
  ;;

  type 'a t = {
    access  : 'a bucket Cas.ref array;
    store   : 'a node Cas.ref;
    size    : int Cas.ref;
    content : int Cas.ref
  };;

  let to_string t =
    print_endline "TO_STRING";
    let buf = Buffer.create 20 in
    Buffer.add_string buf (sprintf "Size = %d and Content = %d\nAccess\n[" (Cas.get t.size) (Cas.get t.content));
    Array.iter
      (fun r ->
        match Cas.get r with
        |Uninitialized -> Buffer.add_string buf "x, "
        |Initialized(s, k, v, next) -> Buffer.add_string buf (sprintf "(%d), " k))
      t.access;
    Buffer.add_string buf (sprintf "]\nStore\n");
    let rec loop n =
      print_endline "TO_STRING_LOOP";
      match Cas.get n with
      |Node(s, k, v, next) ->
        print_endline (sprintf "%d, " k);
        if s then
          Buffer.add_string buf (sprintf "[%d]" k)
        else
          Buffer.add_string buf (sprintf "(%d)" k);
        loop next
      |Nil -> ()
    in loop t.store;
    Buffer.add_string buf "\n\n";
    Buffer.contents buf
  ;;

  let load = 3;;

  let rec split_compare a b =
    if a = 0 && b = 0 then
      0
    else
      let bit_a = a land 1 in
      let bit_b = b land 1 in
      if bit_a = bit_b then
        split_compare (a lsl 1) (b lsl 1)
      else if bit_a < bit_b then
        -1
      else
         1
  ;;

  let list_find sentinel new_k =
    print_endline "LIST_FIND";
    let (_, sk, sv, snext) = sentinel in
    let rec loop prev n =
      print_endline "LIST_FIND_LOOP";
      match Cas.get n with
      |Node(s, k, v, next) ->
        print_endline (sprintf "LIST_FIND_LOOP Cas 1    key: %d" k);
        if s || split_compare new_k k >= 0 then
          (prev, n)
        else
          loop n next
      |Nil -> print_endline "LIST_FIND_LOOP Cas 2"; (prev, n)
    in loop (Cas.ref (Node(sentinel))) snext
  ;;

  let create () =
    print_endline "CREATE";
    let nil = Cas.ref Nil in {
    access  = Array.init 2 (fun i -> if i=0 then Cas.ref (Initialized(true, 0, Obj.magic (), nil)) else Cas.ref Uninitialized);
    store   = Cas.ref (Node(true, 0, Obj.magic (), nil));
    size    = Cas.ref 2;
    content = Cas.ref 0
  };;

  let hash t k =
    print_endline "HASH";
    k mod (Cas.get t.size)
  ;;

  let rec get_bucket t hk =
    print_endline (sprintf "GET_BUCKET %d" hk);
    match Cas.get t.access.(hk) with
    |Uninitialized -> initialise_bucket t hk; get_bucket t hk
    |Initialized(s) -> s
  and initialise_bucket t hk =
    print_endline (sprintf "INITIALISE_BUCKET %d" hk);
    let prev_hk = hk mod ((Cas.get t.size) / 2) in
    let prev_s = get_bucket t prev_hk in
    let (prev, next) = list_find prev_s hk in
    match Cas.get prev, Cas.get next with
    |Node(ps, pk, pv, pnext), (Node(ns, nk, nv, nnext) as vnext) ->
      let rvnext = Cas.ref vnext in
      print_endline "Cas 1";
      assert false;
      if ns && split_compare nk hk = 0 then begin
        Cas.cas t.access.(hk) Uninitialized (Initialized(ns, nk, nv, nnext)); ()
      end else if Cas.cas pnext vnext (Node(true, hk, Obj.magic (), rvnext)) then begin
        Cas.cas t.access.(hk) Uninitialized (Initialized(true, hk, Obj.magic (), rvnext)); ()
      end
    |Node(ps, pk, pv, pnext), Nil ->
      let rnill = Cas.ref Nil in
      print_endline "Cas 2";
      if Cas.cas pnext Nil (Node(true, hk, Obj.magic (), rnill)) then begin
        Cas.cas t.access.(hk) Uninitialized (Initialized(true, hk, Obj.magic (), rnill)); ()
      end
    |_ -> assert false
  ;;

  let find t k =
    print_endline "FIND";
    let hk = hash t k in
    let s = get_bucket t hk in
    let (prev, next) = list_find s k in
    match Cas.get next with
    |Node(ns, nk, nv, _) when split_compare nk k = 0 -> Some(nv)
    |_ -> None
  ;;

  let mem t k =
    print_endline "MEM";
    let hk = hash t k in
    let s = get_bucket t hk in
    let (prev, next) = list_find s k in
    match Cas.get next with
    |Node(ns, nk, nv, _) when split_compare nk k = 0 -> true
    |_ -> false
  ;;

  let rec add t k v =
    print_endline "ADD";
    let hk = hash t k in
    let s = get_bucket t hk in
    let (prev, next) = list_find s k in
    print_endline "ICI";
    print_endline (to_string t);
    match Cas.get prev, Cas.get next with
    |Node(ps, pk, pv, pnext), vnext ->
      print_endline (sprintf "Prev : %d" pk);
      if vnext = Nil then print_endline "Nil";
      if not (Cas.cas pnext vnext (Node(false, k, v, Cas.ref vnext))) then
        add t k v
      else
        print_endline "SUCCESS"
    |_ -> assert false
  ;;

end;;
