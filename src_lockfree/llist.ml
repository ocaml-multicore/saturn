module Atomic = Transparent_atomic

(* not working for the same reason *)
type ('k, 'v, _) node =
  | Null : ('k, 'v, [> `Null ]) node
  | Node : {
      key : 'k;
      content : ('k, 'v) link Atomic.t;
      mutable incr : Size.once;
    }
      -> ('k, 'v, [> `Node ]) node
  | Mark : {
      node : ('k, 'v, [< `Null | `Node ]) node;
      decr : Size.once;
    }
      -> ('k, 'v, [> `Mark ]) node

and ('k, 'v, 'n) content = { bindings : 'v list; next : ('k, 'v, 'n) node }

and ('k, 'v) link =
  | Link : ('k, 'v, [< `Null | `Node | `Mark ]) content -> ('k, 'v) link
[@@unboxed]

type 'k compare = 'k -> 'k -> int

type ('k, 'v) t = {
  compare : 'k compare;
  head : ('k, 'v) link Atomic.t;
  size : Size.t;
}

let create ~compare () : ('k, 'v) t =
  let head =
    Multicore_magic.copy_as_padded
    @@ Atomic.make (Link { bindings = []; next = Null })
  in
  let size = Size.create () in
  { size; compare; head }

let length t = Size.get t.size
let dummy_next = { bindings = []; next = Null }

let rec find_node t key =
  let head = t.head in
  find_node_rec t key head (Atomic.get head)

and find_node_rec t key prev curr :
    int
    * (_, _) link Atomic.t
    * (_, _, [< `Node | `Null ]) content
    * (_, _, [< `Node | `Null ]) content =
  match curr with
  | Link { next = Mark _; _ } -> find_node t key
  | Link ({ next = Null; _ } as r) -> (-1, prev, r, dummy_next)
  | Link ({ next = Node node; _ } as curr_node) -> begin
      match Atomic.get node.content with
      | Link { next = Mark next; _ } ->
          Size.update_once t.size next.decr;
          let after = Link { curr_node with next = next.node } in
          find_node_rec t key prev
            (if Atomic.compare_and_set prev curr after then after
             else Atomic.get prev)
      | Link ({ next = Null | Node _; _ } as next_val) ->
          let comp = t.compare key node.key in
          if comp == 0 then begin
            if node.incr != Size.used_once then begin
              Size.update_once t.size node.incr;
              node.incr <- Size.used_once
            end;
            (comp, prev, curr_node, next_val)
          end
          else begin
            if comp > 0 then find_node_rec t key node.content (Link next_val)
            else (comp, prev, curr_node, next_val)
          end
    end

let rec add t key value = add_rec t key value t.head (Atomic.get t.head)

and add_rec t key value prev curr =
  let found, prev, curr, next = find_node_rec t key prev curr in
  match curr.next with
  | Node node when found == 0 -> begin
      let after = Link { next with bindings = value :: next.bindings } in
      if Atomic.compare_and_set node.content (Link next) after then ()
      else add_rec t key value prev (Atomic.get prev)
    end
  | _ -> begin
      let incr = Size.new_once t.size Size.incr in
      let new_content = Link { curr with bindings = [ value ] } in
      let (Node r as after) =
        (Node { key; content = Atomic.make new_content; incr }
          : (_, _, [ `Node ]) node)
      in
      if
        Atomic.compare_and_set prev (Link curr)
          (Link { curr with next = after })
      then begin
        if r.incr != Size.used_once then begin
          Size.update_once t.size r.incr;
          r.incr <- Size.used_once
        end;
        find_node_rec t key prev (Atomic.get prev) |> ignore
      end
      else add_rec t key value prev (Atomic.get prev)
    end

let[@inline] replace_head v = function [] -> assert false | _ :: xs -> v :: xs

let rec replace t key value = replace_rec t key value t.head (Atomic.get t.head)

and replace_rec t key value prev curr =
  let found, prev, curr, next = find_node_rec t key prev curr in
  match curr.next with
  | Node node when found == 0 -> begin
      let after =
        Link { next with bindings = replace_head value next.bindings }
      in
      if Atomic.compare_and_set node.content (Link next) after then ()
      else replace_rec t key value prev (Atomic.get prev)
    end
  | _ -> begin
      let incr = Size.new_once t.size Size.incr in
      let new_content = Link { curr with bindings = [ value ] } in
      let after : (_, _, [ `Node ]) node =
        Node { key; content = Atomic.make new_content; incr }
      in
      if
        Atomic.compare_and_set prev (Link curr)
          (Link { curr with next = after })
      then find_node_rec t key prev (Atomic.get prev) |> ignore
      else replace_rec t key value prev (Atomic.get prev)
    end

let rec try_remove t key = try_remove_rec t key t.head (Atomic.get t.head)

and try_remove_rec t key prev curr =
  let cond, prev, curr, next = find_node_rec t key prev curr in
  match curr.next with
  | Null -> false
  | Node curr_node ->
      if cond != 0 then false
      else begin
        match next.bindings with
        | [] -> assert false
        | _ :: [] ->
            let decr = Size.new_once t.size Size.decr in
            let after =
              { bindings = []; next = Mark { node = next.next; decr } }
            in
            if Atomic.compare_and_set curr_node.content (Link next) (Link after)
            then (
              find_node_rec t key prev (Atomic.get prev) |> ignore;
              true)
            else try_remove_rec t key prev (Atomic.get prev)
        | _ :: xs ->
            let after = { next with bindings = xs } in
            if Atomic.compare_and_set curr_node.content (Link next) (Link after)
            then true
            else try_remove_rec t key prev (Atomic.get prev)
      end

let mem t key =
  let cond, _, _, _ = find_node t key in
  cond == 0

let find_opt t key =
  let found, _, curr, next = find_node t key in
  match curr.next with
  | Null -> None
  | Node _ -> if found != 0 then None else Some (List.hd next.bindings)

let find t key =
  let found, _, curr, next = find_node t key in
  match curr.next with
  | Null -> raise Not_found
  | Node _ -> if found != 0 then raise Not_found else List.hd next.bindings

let find_all t key =
  let found, _, _, next = find_node t key in
  if found != 0 then [] else next.bindings
