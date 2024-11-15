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
type ('a, _) node =
  | Null : ('a, [> `Null ]) node
  | Node : {
      next : 'a link Atomic.t;
      mutable value : 'a;
      mutable capacity : int;
      mutable counter : int;
    }
      -> ('a, [> `Node ]) node

and 'a link = Link : ('a, [< `Null | `Node ]) node -> 'a link [@@unboxed]

exception Full
exception Empty

let[@inline] get_capacity (Node r : (_, [< `Node ]) node) = r.capacity

let[@inline] set_capacity (Node r : (_, [< `Node ]) node) value =
  r.capacity <- value

let[@inline] get_counter (Node r : (_, [< `Node ]) node) = r.counter

let[@inline] set_counter (Node r : (_, [< `Node ]) node) value =
  r.counter <- value

let[@inline] get_next (Node node : (_, [< `Node ]) node) = Atomic.get node.next

let[@inline] compare_and_set_next (Node node : (_, [< `Node ]) node) before
    after =
  Atomic.compare_and_set node.next before after

let[@inline] link_as_node (Link n) : (_, [< `Node ]) node =
  match n with Null -> assert false | Node _ as node -> node

type 'a t = {
  head : ('a, [ `Node ]) node Atomic.t;
  capacity : int;
  tail : ('a, [ `Node ]) node Atomic.t;
}

let create ?(capacity = Int.max_int) () =
  let value = Obj.magic () in
  let node =
    Node { next = Atomic.make (Link Null); value; capacity; counter = 0 }
  in
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
          Node
            {
              value = hd;
              counter = len;
              capacity = capacity - len - 1;
              next = Atomic.make (Link Null);
            }
        in
        let _, _, next =
          List.fold_left
            (fun (counter, capacity, next) value ->
              ( counter - 1,
                capacity + 1,
                Node
                  { value; counter; capacity; next = Atomic.make (Link next) }
              ))
            (len - 1, capacity - len, tail)
            tl
        in
        let head =
          Atomic.make_contended
          @@ Node
               {
                 value = Obj.magic ();
                 capacity;
                 counter = 0;
                 next = Atomic.make (Link next);
               }
        in
        { head; capacity; tail = Atomic.make tail }

let capacity_of t = t.capacity

let is_empty t =
  let (Node head) = Atomic.get t.head in
  Atomic.get head.next == Link Null

let rec snapshot t =
  let old_head = Atomic.get t.head in
  let (Node tail as old_tail) = Atomic.get t.tail in
  match Atomic.get tail.next with
  | Link (Node _ as node) ->
      Atomic.compare_and_set t.tail old_tail node |> ignore;
      snapshot t
  | Link Null ->
      if Atomic.get t.head != old_head then snapshot t else (old_head, old_tail)

let length t =
  let head, tail = snapshot t in
  get_counter tail - get_counter head

(* *)

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let rec peek_as : type a r. a t -> (a, r) poly -> r =
 fun t poly ->
  let (Node head as old_head) = Atomic.get t.head in
  match Atomic.get head.next with
  | Link Null -> ( match poly with Value -> raise Empty | Option -> None)
  | Link (Node r) -> (
      let value = r.value in
      if Atomic.get t.head != old_head then peek_as t poly
      else match poly with Value -> value | Option -> Some value)

let[@inline] peek_opt t = peek_as t Option
let[@inline] peek_exn t = peek_as t Value
(* *)

type ('a, _) poly2 =
  | Option : ('a, 'a option) poly2
  | Value : ('a, 'a) poly2
  | Unit : ('a, unit) poly2

let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly2 -> r =
 fun t backoff poly ->
  let (Node head as old_head) = Atomic.get t.head in
  match Atomic.get head.next with
  | Link Null -> (
      match poly with Option -> None | Value | Unit -> raise Empty)
  | Link (Node node as new_head) ->
      if Atomic.compare_and_set t.head old_head new_head then begin
        let value = node.value in
        node.value <- Obj.magic ();
        match poly with Option -> Some value | Value -> value | Unit -> ()
      end
      else pop_as t (Backoff.once backoff) poly

let[@inline] pop_opt t = pop_as t Backoff.default Option
let[@inline] pop_exn t = pop_as t Backoff.default Value
let[@inline] drop_exn t = pop_as t Backoff.default Unit
(* *)

let rec fix_tail tail new_tail =
  let old_tail = Atomic.get tail in
  if
    get_next new_tail == Link Null
    && not (Atomic.compare_and_set tail old_tail new_tail)
  then fix_tail tail new_tail

(* *)

let rec try_push t new_node old_tail =
  let capacity = get_capacity old_tail in
  if capacity = 0 then
    let old_head = Atomic.get t.head in
    let length = get_counter old_tail - get_counter old_head in
    let capacity = t.capacity - length in
    0 < capacity
    && begin
         set_capacity old_tail capacity;
         try_push t new_node old_tail
       end
  else begin
    set_capacity new_node (capacity - 1);
    set_counter new_node (get_counter old_tail + 1);
    if not (compare_and_set_next old_tail (Link Null) (Link new_node)) then
      try_push t new_node (link_as_node (get_next old_tail))
    else begin
      if not (Atomic.compare_and_set t.tail old_tail new_node) then
        fix_tail t.tail new_node;
      true
    end
  end

(* *)

let[@inline] try_push t value =
  let new_node =
    Node { next = Atomic.make (Link Null); value; capacity = 0; counter = 0 }
  in
  try_push t new_node (Atomic.get t.tail)

let[@inline] push_exn t value = if not (try_push t value) then raise Full
