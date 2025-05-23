(* Copyright (c) 2024 Carine Morel

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

(* Based on the skiplist implementation written by Vesa Karvonen. *)

module Atomic = Multicore_magic.Transparent_atomic

type ('k, 'v, _) node =
  | Null : ('k, 'v, [> `Null ]) node
  | Node : {
      key : 'k;
      value : 'v;
      next : ('k, 'v) links;
      mutable incr : Size.once;
    }
      -> ('k, 'v, [> `Node ]) node
  | Mark : {
      node : ('k, 'v, [< `Null | `Node ]) node;
      decr : Size.once;
    }
      -> ('k, 'v, [> `Mark ]) node

and ('k, 'v) link =
  | Link : ('k, 'v, [< `Null | `Node | `Mark ]) node -> ('k, 'v) link
[@@unboxed]

and ('k, 'v) links = ('k, 'v) link Atomic.t array

type 'k compare = 'k -> 'k -> int
(* Encoding the [compare] function using an algebraic type would allow the
   overhead of calling a closure to be avoided for selected primitive types like
   [int]. *)

type ('k, 'v) t = { compare : 'k compare; root : ('k, 'v) links; size : Size.t }

exception Empty
(* *)

(** [get_random_height max_height] gives a random value [n] in the range from
    [1] to [max_height] with the desired distribution such that [n] is twice as
    likely as [n + 1]. *)
let rec get_random_height max_height =
  let m = (1 lsl max_height) - 1 in
  let x = Random.bits () land m in
  if x = 1 then
    (* We reject [1] to get the desired distribution. *)
    get_random_height max_height
  else
    (* We do a binary search for the highest 1 bit.  Techniques in

         Using de Bruijn Sequences to Index a 1 in a Computer Word
         by Leiserson, Prokop, and Randall

       could perhaps speed this up a bit, but this is likely not performance
       critical. *)
    let n = 0 in
    let n, x = if 0xFFFF < x then (n + 0x10, x lsr 0x10) else (n, x) in
    let n, x = if 0x00FF < x then (n + 0x08, x lsr 0x08) else (n, x) in
    let n, x = if 0x000F < x then (n + 0x04, x lsr 0x04) else (n, x) in
    let n, x = if 0x0003 < x then (n + 0x02, x lsr 0x02) else (n, x) in
    let n, _ = if 0x0001 < x then (n + 0x01, x lsr 0x01) else (n, x) in
    max_height - n

(* *)

let[@inline] is_marked = function
  | Link (Mark _) -> true
  | Link (Null | Node _) -> false

(* *)

(** [find_path t key preds succs lowest] search fo the position after all the
    nodes with key [key], updating [preds] and [succs] and removing nodes with
    marked references along the way, and always descending down to [lowest]
    level. The boolean return value is only meaningful when [lowest] is given as
    [0]. *)
let rec find_path t key preds succs lowest =
  let prev = t.root in
  let level = Array.length prev - 1 in
  let prev_at_level = Array.unsafe_get prev level in
  find_path_rec t key prev prev_at_level preds succs level lowest
    (Atomic.get prev_at_level)

and find_path_rec t key prev prev_at_level preds succs level lowest = function
  | Link Null ->
      if level < Array.length preds then begin
        Array.unsafe_set preds level prev_at_level;
        Array.unsafe_set succs level Null
      end;
      if lowest < level then
        let level = level - 1 in
        let prev_at_level = Array.unsafe_get prev level in
        find_path_rec t key prev prev_at_level preds succs level lowest
          (Atomic.get prev_at_level)
  | Link (Node r as curr) -> begin
      let next_at_level = Array.unsafe_get r.next level in
      match Atomic.get next_at_level with
      | Link (Null | Node _) as next ->
          let c = t.compare key r.key in

          if 0 <= c then
            (* key >= r.key *)
            find_path_rec t key r.next next_at_level preds succs level lowest
              next
          else begin
            if level < Array.length preds then begin
              Array.unsafe_set preds level (Array.unsafe_get prev level);
              Array.unsafe_set succs level curr
            end;

            if lowest < level then
              let level = level - 1 in
              let prev_at_level = Array.unsafe_get prev level in
              find_path_rec t key prev prev_at_level preds succs level lowest
                (Atomic.get prev_at_level)
            else begin
              if level = 0 then begin
                if r.incr != Size.used_once then begin
                  Size.update_once t.size r.incr;
                  r.incr <- Size.used_once
                end;
                () (* Return *)
              end
            end
          end
      | Link (Mark r) ->
          (* The [curr_node] is being removed from the skiplist and we help with
             that. *)
          if level = 0 then Size.update_once t.size r.decr;
          find_path_rec t key prev prev_at_level preds succs level lowest
            (let after = Link r.node in
             if Atomic.compare_and_set prev_at_level (Link curr) after then
               after
             else Atomic.get prev_at_level)
    end
  | Link (Mark _) ->
      (* The node corresponding to [prev] is being removed from the skiplist.
         This means we might no longer have an up-to-date view of the skiplist
         and so we must restart the search. *)
      find_path t key preds succs lowest

(* *)

(** [find_node t key] tries to find the first node with the specified [key],
    removing nodes with marked references along the way, and stopping as soon as
    the node is found. *)
let rec find_node t ?timestamp key =
  let prev = t.root in
  let level = Array.length prev - 1 in
  let prev_at_level = Array.unsafe_get prev level in
  find_node_rec t ~timestamp key prev prev_at_level level
    (Atomic.get prev_at_level)

and find_node_rec t ~timestamp key prev prev_at_level level :
    _ -> (_, _, [< `Null | `Node ]) node = function
  | Link Null ->
      if 0 < level then
        let level = level - 1 in
        let prev_at_level = Array.unsafe_get prev level in
        find_node_rec t ~timestamp key prev prev_at_level level
          (Atomic.get prev_at_level)
      else Null
  | Link (Node r as curr) -> begin
      let next_at_level = Array.unsafe_get r.next level in
      match Atomic.get next_at_level with
      | Link (Null | Node _) as next ->
          let c = t.compare key r.key in
          if 0 < c then
            find_node_rec t ~timestamp key r.next next_at_level level next
          else if 0 < level then
            let level = level - 1 in
            let prev_at_level = Array.unsafe_get prev level in
            find_node_rec t ~timestamp key prev prev_at_level level
              (Atomic.get prev_at_level)
          else if c = 0 then begin
            if r.incr != Size.used_once then begin
              Size.update_once t.size r.incr;
              r.incr <- Size.used_once
            end;
            curr
          end
          else Null
      | Link (Mark r) ->
          if level = 0 then Size.update_once t.size r.decr;
          find_node_rec t ~timestamp key prev prev_at_level level
            (let after = Link r.node in
             if Atomic.compare_and_set prev_at_level (Link curr) after then
               after
             else Atomic.get prev_at_level)
    end
  | Link (Mark _) -> find_node t key

(* *)

let create ?(max_height = 10) ~compare () =
  (* The upper limit of [30] comes from [Random.bits ()] as well as from
     limitations with 32-bit implementations.  It should not be a problem in
     practice. *)
  if max_height < 1 || 30 < max_height then
    invalid_arg "Skiplist: max_height must be in the range [1, 30]";
  let root = Array.init max_height @@ fun _ -> Atomic.make (Link Null) in
  let size = Size.create () in
  { compare; root; size }

let max_height_of t = Array.length t.root

(* *)

let rec add t key value preds succs =
  find_path t key preds succs 0;

  let (Node r as node : (_, _, [ `Node ]) node) =
    let next = Array.map (fun succ -> Atomic.make (Link succ)) succs in
    let incr = Size.new_once t.size Size.incr in
    Node { key; value; incr; next }
  in
  if
    let succ = Link (Array.unsafe_get succs 0) in
    Atomic.compare_and_set (Array.unsafe_get preds 0) succ (Link node)
  then begin
    if r.incr != Size.used_once then begin
      Size.update_once t.size r.incr;
      r.incr <- Size.used_once
    end;

    (* The node is now considered as added to the skiplist. *)
    let rec update_levels level : unit =
      if Array.length r.next = level then begin
        if is_marked (Atomic.get (Array.unsafe_get r.next (level - 1))) then begin
          (* The node we finished adding has been removed concurrently.  To
             ensure that no references we added to the node remain, we call
             [find_node] which will remove nodes with marked references along
             the way. *)
          find_node t key |> ignore
        end
      end
      else if
        let succ = Link (Array.unsafe_get succs level) in
        Atomic.compare_and_set (Array.unsafe_get preds level) succ (Link node)
      then update_levels (level + 1)
      else
        let _found = find_path t key preds succs level in
        let rec update_nexts level' =
          if level' < level then update_levels level
          else
            let next = Array.unsafe_get r.next level' in
            match Atomic.get next with
            | Link (Null | Node _) as before ->
                let succ = Link (Array.unsafe_get succs level') in
                if before != succ then
                  (* It is possible for a concurrent remove operation to have
                     marked the link. *)
                  if Atomic.compare_and_set next before succ then
                    update_nexts (level' - 1)
                  else update_levels level
                else update_nexts (level' - 1)
            | Link (Mark _) ->
                (* The node we were trying to add has been removed concurrently.
                   To ensure that no references we added to the node remain, we
                   call [find_node] which will remove nodes with marked
                   references along the way. *)
                find_path t key preds succs level
        in
        update_nexts (Array.length r.next - 1)
    in
    update_levels 1
  end
  else add t key value preds succs

let add t key value : unit =
  let height = get_random_height (Array.length t.root) in
  let preds =
    (* Init with [Obj.magic ()] is safe as the array is fully overwritten by
       [find_path] called at the start of the recursive [try_add]. *)
    Array.make height (Obj.magic ())
  in
  let succs = Array.make height Null in
  add t key value preds succs

(* *)

let length t = Size.get t.size

(* *)

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let rec find_min t : (_, _, [< `Node | `Null ]) node =
  let root = t.root in
  let root_at_level0 = Array.unsafe_get root 0 in
  match Atomic.get root_at_level0 with
  | Link (Mark _) -> assert false
  | Link Null -> Null
  | Link (Node r) as curr_link -> (
      let next_at_level_0 = Array.unsafe_get r.next 0 in
      match Atomic.get next_at_level_0 with
      | Link (Null | Node _) -> Node r
      | Link (Mark next_marked) ->
          Size.update_once t.size next_marked.decr;
          if
            Atomic.compare_and_set root_at_level0 curr_link
              (Link next_marked.node)
          then find_min t
          else find_min t)

let rec try_remove t key next level link = function
  | Link (Mark r) ->
      if level = 0 then begin
        Size.update_once t.size r.decr;
        false
      end
      else
        let level = level - 1 in
        let link = Array.unsafe_get next level in
        try_remove t key next level link (Atomic.get link)
  | Link ((Null | Node _) as succ) ->
      let decr =
        if level = 0 then Size.new_once t.size Size.decr else Size.used_once
      in
      let marked_succ = Mark { node = succ; decr } in
      if Atomic.compare_and_set link (Link succ) (Link marked_succ) then
        if level = 0 then
          let _node = find_node t key in
          true
        else
          let level = level - 1 in
          let link = Array.unsafe_get next level in
          try_remove t key next level link (Atomic.get link)
      else try_remove t key next level link (Atomic.get link)

let rec remove_min_as : type p v r. (p, v) t -> (p * v, r) poly -> r =
 fun t poly ->
  match find_min t with
  | Null -> ( match poly with Value -> raise Empty | Option -> None)
  | Node { next; key; value; _ } ->
      let level = Array.length next - 1 in
      let link = Array.unsafe_get next level in
      if try_remove t key next level link (Atomic.get link) then
        match poly with Value -> (key, value) | Option -> Some (key, value)
      else remove_min_as t poly

let remove_min_opt t = remove_min_as t Option
let remove_min_exn t = remove_min_as t Value
