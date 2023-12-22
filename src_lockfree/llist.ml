module Atomic = Transparent_atomic

type ('k, 'v, _) node =
  | Null : ('k, 'v, [> `Null ]) node
  | Node : {
      key : 'k;
      content : ('k, 'v) link Atomic.t;
    }
      -> ('k, 'v, [> `Node ]) node
  | Mark : ('k, 'v, [< `Null | `Node ]) node -> ('k, 'v, [> `Mark ]) node

and ('k, 'v, 'n) content = {
  mutable incr : Size.once;
  decr : Size.once;
  bindings : 'v list;
  next : ('k, 'v, 'n) node;
}

and ('k, 'v) link =
  | Link : ('k, 'v, [< `Null | `Node | `Mark ]) content -> ('k, 'v) link
[@@unboxed]

type 'k compare = 'k -> 'k -> int

type ('k, 'v) t = {
  compare : 'k compare;
  head : ('k, 'v) link Atomic.t;
  size : Size.t;
}

(* can we make it a value instead of a function *)
let dummy_content () =
  { incr = Size.used_once; decr = Size.used_once; bindings = []; next = Null }

let create ~compare () : ('k, 'v) t =
  let head =
    Multicore_magic.copy_as_padded @@ Atomic.make (Link (dummy_content ()))
  in
  let size = Size.create () in
  { size; compare; head }

let length t = Size.get t.size

let rec find_node t key =
  let head = t.head in
  find_node_rec t key head (Atomic.get head)

and find_node_rec t key prev curr :
    int
    * (_, _) link Atomic.t
    * (_, _, [< `Null | `Node ]) content
    * (_, _, [< `Null | `Node ]) content =
  match curr with
  | Link { next = Mark _; _ } -> find_node t key
  | Link ({ next = Null; _ } as r) -> (-1, prev, r, dummy_content ())
  | Link ({ next = Node node; _ } as curr_node) -> begin
      match Atomic.get node.content with
      | Link { next = Mark next_node; decr; _ } ->
          Size.update_once t.size decr;
          let after = Link { curr_node with next = next_node } in
          find_node_rec t key prev
            (if Atomic.compare_and_set prev curr after then after
             else Atomic.get prev)
      | Link ({ next = Null | Node _; incr; _ } as next_val) ->
          let comp = t.compare key node.key in
          if comp == 0 then begin
            if incr != Size.used_once then begin
              Size.update_once t.size incr;
              next_val.incr <- Size.used_once
            end;
            (comp, prev, curr_node, next_val)
          end
          else begin
            if comp > 0 then find_node_rec t key node.content (Link next_val)
            else (comp, prev, curr_node, next_val)
          end
    end

let rec add_replace_rec t key value fbinding prev curr =
  let found, prev, curr, next = find_node_rec t key prev curr in
  match curr.next with
  | Node node when found == 0 -> begin
      match next.bindings with
      | [] ->
          let incr = Size.new_once t.size Size.incr in
          let (Link r as after) =
            Link { next with bindings = fbinding value next.bindings; incr }
          in
          if Atomic.compare_and_set node.content (Link next) after then begin
            if r.incr != Size.used_once then begin
              Size.update_once t.size r.incr;
              r.incr <- Size.used_once
            end
          end
          else add_replace_rec t key value fbinding prev (Atomic.get prev)
      | _ ->
          let after =
            Link { next with bindings = fbinding value next.bindings }
          in
          if not @@ Atomic.compare_and_set node.content (Link next) after then
            add_replace_rec t key value fbinding prev (Atomic.get prev)
    end
  | _ -> begin
      let decr = Size.new_once t.size Size.decr in
      let incr = Size.new_once t.size Size.incr in
      let (Link r as new_content) =
        Link { bindings = [ value ]; incr; decr; next = curr.next }
      in
      let after =
        (Node { key; content = Atomic.make new_content }
          : (_, _, [ `Node ]) node)
      in
      if
        Atomic.compare_and_set prev (Link curr)
          (Link { curr with next = after })
      then begin
        if r.incr != Size.used_once then begin
          Size.update_once t.size r.incr;
          r.incr <- Size.used_once
        end
      end
      else add_replace_rec t key value fbinding prev (Atomic.get prev)
    end

let add t key value =
  add_replace_rec t key value List.cons t.head (Atomic.get t.head)

let replace_last new_binding = function
  | [] -> [ new_binding ]
  | _ :: bindings -> new_binding :: bindings

let replace t key value =
  add_replace_rec t key value replace_last t.head (Atomic.get t.head)

let rec add_empty t key = add_empty_rec t key t.head (Atomic.get t.head)

and add_empty_rec t key prev curr =
  let found, prev, curr, _ = find_node_rec t key prev curr in
  if found == 0 then ()
  else begin
    let incr = Size.used_once in
    let new_content = Link { curr with bindings = []; incr } in
    let after =
      (Node { key; content = Atomic.make new_content } : (_, _, [ `Node ]) node)
    in
    if Atomic.compare_and_set prev (Link curr) (Link { curr with next = after })
    then ()
    else add_empty_rec t key prev (Atomic.get prev)
  end

(* [try_remove ~empty=true t key] can removed empty nodes. Probably not the best
   API for using it in the hashtable, but it enables it to test it !  *)
let rec try_remove ?(empty = false) t key =
  try_remove_rec empty t key t.head (Atomic.get t.head)

and try_remove_rec empty t key prev curr =
  let cond, prev, curr, next = find_node_rec t key prev curr in
  match curr.next with
  | Null -> false
  | Node curr_node ->
      if cond != 0 then false
      else begin
        match next.bindings with
        | [] ->
            if empty then begin
              (* removing an empty node *)
              let decr = Size.used_once in
              (* we don't want to decrease the size *)
              let after =
                { next with bindings = []; decr; next = Mark next.next }
              in
              if
                Atomic.compare_and_set curr_node.content (Link next)
                  (Link after)
              then (
                find_node_rec t key prev (Atomic.get prev) |> ignore;
                true)
              else try_remove_rec empty t key prev (Atomic.get prev)
            end
            else false
        | _ :: [] ->
            if empty then
              let decr = Size.new_once t.size Size.decr in
              let after =
                { next with bindings = []; decr; next = Mark next.next }
              in
              if
                Atomic.compare_and_set curr_node.content (Link next)
                  (Link after)
              then (
                find_node_rec t key prev (Atomic.get prev) |> ignore;
                true)
              else try_remove_rec empty t key prev (Atomic.get prev)
            else
              let incr = Size.used_once in
              let decr = Size.new_once t.size Size.decr in
              let after =
                { bindings = []; decr; incr; next = Mark next.next }
              in
              if
                Atomic.compare_and_set curr_node.content (Link next)
                  (Link after)
              then (
                find_node_rec t key prev (Atomic.get prev) |> ignore;
                true)
              else try_remove_rec empty t key prev (Atomic.get prev)
        | _ :: xs ->
            let after = { next with bindings = xs } in
            if Atomic.compare_and_set curr_node.content (Link next) (Link after)
            then true
            else try_remove_rec empty t key prev (Atomic.get prev)
      end

let mem t key =
  let cond, _, _, next = find_node t key in
  match next.bindings with [] -> false | _ -> cond == 0

let find_opt t key =
  let found, _, curr, next = find_node t key in
  match curr.next with
  | Null -> None
  | Node _ -> (
      if found != 0 then None
      else match next.bindings with [] -> None | first :: _ -> Some first)

let find t key =
  let found, _, curr, next = find_node t key in
  match curr.next with
  | Null -> raise Not_found
  | Node _ -> (
      if found != 0 then raise Not_found
      else
        match next.bindings with [] -> raise Not_found | first :: _ -> first)

let find_all t key =
  let found, _, _, next = find_node t key in
  if found != 0 then [] else next.bindings
