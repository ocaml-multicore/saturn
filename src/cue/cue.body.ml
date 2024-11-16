(* Copyright (c) 2023-2024, Vesa Karvonen <vesa.a.j.k@gmail.com>
   Copyright (c) 2024, Carine Morel <carine.morel.pro@gmail.com>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

let[@inline] get_capacity (Node r : (_, [< `Node ]) node) = r.capacity

let[@inline] set_capacity (Node r : (_, [< `Node ]) node) value =
  r.capacity <- value

let[@inline] get_counter (Node r : (_, [< `Node ]) node) = r.counter

let[@inline] set_counter (Node r : (_, [< `Node ]) node) value =
  r.counter <- value

type 'a t = {
  head : ('a, [ `Node ]) node Atomic.t;
  capacity : int;
  tail : ('a, [ `Node ]) node Atomic.t;
}

exception Full
exception Empty

let create ?(capacity = Int.max_int) () =
  let value = Obj.magic () in
  let node = make_node ~value ~capacity ~counter:0 Null in
  let head = Atomic.make_contended node and tail = Atomic.make_contended node in
  { head; capacity; tail }

let of_list_exn ?(capacity = Int.max_int) list : 'a t =
  let len = List.length list in
  if len > capacity then raise Full
  else
    match list |> List.rev with
    | [] -> create ~capacity ()
    | hd :: tl ->
        let tail =
          make_node ~value:hd ~capacity:(capacity - len - 1) ~counter:len Null
        in
        let _, _, next =
          List.fold_left
            (fun (counter, capacity, next) value ->
              ( counter - 1,
                capacity + 1,
                make_node ~value ~capacity ~counter next ))
            (len - 1, capacity - len, tail)
            tl
        in
        let head =
          Atomic.make_contended
            (make_node ~value:(Obj.magic ()) ~capacity ~counter:0 next)
        in

        { head; capacity; tail = Atomic.make tail }

let capacity_of t = t.capacity

let is_empty t =
  let head = Atomic.get t.head in
  fenceless_get_next head == Link Null

let rec snapshot t =
  let head = Atomic.get t.head in
  let tail = fenceless_get t.tail in
  match fenceless_get_next tail with
  | Link (Node _ as node) ->
      Atomic.compare_and_set t.tail tail node |> ignore;
      snapshot t
  | Link Null -> if Atomic.get t.head != head then snapshot t else (head, tail)

let length t =
  let head, tail = snapshot t in
  get_counter tail - get_counter head

(* *)

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let rec peek_as : type a r. a t -> (a, r) poly -> r =
 fun t poly ->
  let old_head = Atomic.get t.head in
  match fenceless_get_next old_head with
  | Link Null -> ( match poly with Value -> raise Empty | Option -> None)
  | Link (Node r) -> (
      let value = r.value in
      if Atomic.get t.head != old_head then peek_as t poly
      else match poly with Value -> value | Option -> Some value)

(* *)

type ('a, _) poly2 =
  | Option : ('a, 'a option) poly2
  | Value : ('a, 'a) poly2
  | Unit : ('a, unit) poly2

let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly2 -> r =
 fun t backoff poly ->
  let old_head = Atomic.get t.head in
  match fenceless_get_next old_head with
  | Link Null -> (
      match poly with Option -> None | Value | Unit -> raise Empty)
  | Link (Node node as new_head) ->
      if Atomic.compare_and_set t.head old_head new_head then begin
        let value = node.value in
        node.value <- Obj.magic ();
        match poly with Option -> Some value | Value -> value | Unit -> ()
      end
      else pop_as t (Backoff.once backoff) poly

(* *)

let rec fix_tail tail new_tail =
  let old_tail = Atomic.get tail in
  if
    get_next new_tail == Link Null
    && not (Atomic.compare_and_set tail old_tail new_tail)
  then fix_tail tail new_tail

(* *)

type _ mono = Bool : bool mono | Unit : unit mono

let rec push_as :
    type r. 'a t -> ('a, [ `Node ]) node -> ('a, [ `Node ]) node -> r mono -> r
    =
 fun t new_node old_tail mono ->
  let capacity = get_capacity old_tail in
  if capacity = 0 then begin
    let old_head = Atomic.get t.head in
    let length = get_counter old_tail - get_counter old_head in
    let capacity = t.capacity - length in
    if 0 < capacity then begin
      set_capacity old_tail capacity;
      push_as t new_node old_tail mono
    end
    else match mono with Unit -> raise Full | Bool -> false
  end
  else begin
    set_capacity new_node (capacity - 1);
    set_counter new_node (get_counter old_tail + 1);
    if not (compare_and_set_next old_tail (Link Null) (Link new_node)) then
      push_as t new_node (link_as_node (get_next old_tail)) mono
    else begin
      if not (Atomic.compare_and_set t.tail old_tail new_node) then
        fix_tail t.tail new_node;
      match mono with Unit -> () | Bool -> true
    end
  end

(* *)

let[@inline] peek_opt t = peek_as t Option
let[@inline] peek_exn t = peek_as t Value
let[@inline] pop_opt t = pop_as t Backoff.default Option
let[@inline] pop_exn t = pop_as t Backoff.default Value
let[@inline] drop_exn t = pop_as t Backoff.default Unit

let[@inline] try_push t value =
  let new_node = make_node ~value ~capacity:0 ~counter:0 Null in
  push_as t new_node (Atomic.get t.tail) Bool

let[@inline] push_exn t value =
  let new_node = make_node ~value ~capacity:0 ~counter:0 Null in
  push_as t new_node (Atomic.get t.tail) Unit
