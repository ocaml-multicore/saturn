(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

module type S = sig
  type 'a t;;

  val still_split_order : 'a t -> bool;;

  val equal : 'a t -> 'a t -> bool;;

  val to_string : 'a t -> string;;

  val create : unit -> 'a t;;

  val find : 'a t -> int -> 'a option;;

  val mem : 'a t -> int -> bool;;

  val add : 'a t -> int -> 'a -> unit;;

  val remove : 'a t -> int -> unit;;
end;;

module M : S = struct
  module Cas = Kcas.W1;;

  type 'a elem_list = bool * int * 'a * 'a node_ptr
  and 'a node_ptr = (bool * 'a node) Cas.ref
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
    store   : 'a node_ptr;
    size    : int Cas.ref;
    content : int Cas.ref
  };;

  let equal t1 t2 =
    let rec loop_l n1 n2 =
      match Cas.get n1, Cas.get n2 with
      |(m1, Node(s1, k1, v1, next1)), (m2, Node(s2, k2, v2, next2)) ->
        s1 = s2 && k1 = k2 && v1 = v2 && m1 = m2 && (loop_l next1 next2)
      |(_, Nil), (_, Nil) -> true
      |_ -> false
    in
    let loop_a a1 a2 =
      let len = min (Array.length a1) (Array.length a2) in
      let out = ref true in
      for i = 0 to len-1 do
        match Cas.get a1.(i), Cas.get a2.(i) with
        |Initialized(_), Initialized(_) |Uninitialized, Uninitialized -> ()
        |_ -> out := false
      done;
      !out
    in
    (Cas.get t1.size) = (Cas.get t2.size) &&
    (Cas.get t1.content) = (Cas.get t2.content) &&
    loop_l t1.store t2.store &&
    loop_a t1.access t2.access
  ;;

  let to_string t =
    (*print_endline "TO_STRING";*)
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
      (*print_endline "TO_STRING_LOOP";*)
      match Cas.get n with
      |m, Node(s, k, v, next) ->
        (*print_endline (sprintf "%d, " k);*)
        if s then
          Buffer.add_string buf (sprintf "[%d]" k)
        else
          Buffer.add_string buf (sprintf "(%d)" k);
        loop next
      |m, Nil -> ()
    in loop t.store;
    Buffer.add_string buf "\n\n";
    Buffer.contents buf
  ;;

  let load = 3;;

  let rec split_compare a b =
    (*print_endline (Printf.sprintf "%d %d" a b);*)
    if a = 0 && b = 0 then
      0
    else
      let bit_a = a land 1 in
      let bit_b = b land 1 in
      if bit_a = bit_b then
        split_compare (a lsr 1) (b lsr 1)
      else if bit_a < bit_b then
        -1
      else
         1
  ;;

  let still_split_order t =
    let rec loop k n =
      match Cas.get n with
      |_, Node(_, nk, _, next) -> split_compare k nk <= 0 && loop nk next
      |_ -> true
    in loop 0 t.store
  ;;

  let list_find sentinel new_k =
    print_endline "LIST_FIND";
    let (_, sk, sv, snext) = sentinel in
    let rec loop prev n =
      print_endline "LIST_FIND_LOOP";
      match Cas.get n with
      |mark, (Node(s, k, v, next) as nnode) as vn ->
        print_endline (sprintf "LIST_FIND_LOOP Cas 1    key: %d" k);
        if s || split_compare new_k k <= 0 then
          (prev, vn)
        else
          loop nnode next
      |mark, Nil as vn -> print_endline "LIST_FIND_LOOP Cas 2"; (prev, vn)
    in loop (Node(sentinel)) snext
  ;;

  let list_insert sentinel new_s new_k new_v =
    print_endline "LIST_INSERT";
    let b = Kcas.Backoff.create () in
    let rec loop () =
      print_endline "LIST_INSERT_LOOP";
      let (prev, next) = list_find sentinel new_k in
      let new_node = (new_s, new_k, new_v, Cas.ref (false, snd next)) in
      match prev, next with
      |Node(ps, pk, pv, pnext), (nm, Node(ns, nk, nv, nnext)) ->
        if not (new_s && ns && new_k = nk) then (* New sentinel not inserted yet *)
          if not nm && Cas.cas pnext next (false, Node(new_node)) then
    (print_endline "Branche 1";
            new_node)
          else
    (print_endline "Branche 2";
            (Kcas.Backoff.once b; loop ()))
        else
    (print_endline "Branche 3";
          (ns, nk, nv, nnext))
      |Node(ps, pk, pv, pnext), (nm, Nil) ->
        if not nm && Cas.cas pnext next (false, Node(new_node)) then
    (print_endline "Branche 4";
          new_node)
        else begin
          let (m, n) = Cas.get pnext in
          if (m, n) == (false, snd next) then print_endline "Nil------------------";
    (print_endline (sprintf "Branche 5 : (%b, %d, _, (%b, _))" ps pk m);
          (Kcas.Backoff.once b; loop ()))
        end
      |_ -> assert false
    in loop ()
  ;;

  let list_delete sentinel k =
    print_endline "LIST_DELETE";
    let b = Kcas.Backoff.create () in
    let rec loop () =
      print_endline "LIST_DELETE_LOOP";
      let (prev, next) = list_find sentinel k in
      match prev, next with
      |Node(ps, pk, pv, pnext), (nm, (Node(ns, nk, nv, nnext))) ->
        print_endline (sprintf "ns = %b && nk = %d ?= %d = k" ns nk k);
        if not ns && nk = k then begin
        print_endline (sprintf "Delete, noeud trouve prev : (%b, %d)    n : (%b, %d)" ps pk ns nk);
        let vnnext = Cas.get nnext in
        if Cas.cas nnext vnnext (true, snd vnnext) then begin
          print_endline "1er CAS OK";
          if not (Cas.cas pnext next (false, snd vnnext)) then
            (Cas.set nnext vnnext; Kcas.Backoff.once b; loop ())
        end else
          (Kcas.Backoff.once b; loop ())
        end else ()
      |_ ->
        print_endline "Non déso";
        if snd next = Nil then print_endline "next encore nil";
        if prev = Nil then print_endline "prev encore nil";
        ()
    in loop ()
  ;;

  let create () =
    print_endline "CREATE";
    let nil = Cas.ref (false, Nil) in {
    access  = Array.init 2 (fun i -> if i=0 then Cas.ref (Initialized(true, 0, Obj.magic (), nil)) else Cas.ref Uninitialized);
    store   = Cas.ref (false, Node(true, 0, Obj.magic (), nil));
    size    = Cas.ref 2;
    content = Cas.ref 0
  };;

  let hash t k =
    print_endline (sprintf "HASH : %d mod %d = %d" k (Cas.get t.size) (k mod (Cas.get t.size)));
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
    let (s, k, v, next) = list_insert prev_s true hk (Obj.magic ()) in
    Cas.cas t.access.(hk) Uninitialized (Initialized(s, k, v, next));
    ()
  ;;

  let find t k =
    print_endline "FIND";
    let hk = hash t k in
    let s = get_bucket t hk in
    let (prev, next) = list_find s k in
    match snd next with
    |Node(ns, nk, nv, _) when split_compare nk k = 0 -> Some(nv)
    |Nil -> None
  ;;

  let mem t k =
    print_endline "MEM";
    let hk = hash t k in
    let s = get_bucket t hk in
    let (prev, next) = list_find s k in
    match snd next with
    |Node(ns, nk, nv, _) when split_compare nk k = 0 -> true
    |Nil -> false
  ;;


  let rec add t k v =
    print_endline "ADD";
    let hk = hash t k in
    let s = get_bucket t hk in
    let _ = list_insert s false k v in
    ()
  ;;

  let remove t k =
    print_endline "REMOVE";
    let hk = hash t k in
    let s = get_bucket t hk in
    list_delete s k
  ;;

end;;
