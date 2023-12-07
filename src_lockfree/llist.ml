module Atomic = Transparent_atomic

type ('k, 'v, _) node =
  | Null : ('k, 'v, [> `Null ]) node
  | Node : {
      key : 'k;
      value : 'v;
      next : ('k, 'v) link Atomic.t;
      mutable incr : Size.once;
    }
      -> ('k, 'v, [> `Node ]) node
  | Mark : {
      node : ('k, 'v, [< `Null | `Node ]) node;
      decr : Size.once;
    }
      -> ('k, 'v, [ `Mark ]) node

and ('k, 'v) link = Link : ('k, 'v, _) node -> ('k, 'v) link [@@unboxed]

type 'k compare = 'k -> 'k -> int

type ('k, 'v) t = {
  compare : 'k compare;
  head : ('k, 'v) link Atomic.t;
  size : Size.t;
}

let create ~compare () : ('k, 'v) t =
  let head = Atomic.make @@ Link Null |> Multicore_magic.copy_as_padded in
  let size = Size.create () in
  { size; compare; head }

let length t = Size.get t.size

let rec find_node_rec t key prev curr =
  match curr with
  | Link (Mark _) -> find_node t key
  | Link Null -> (-1, prev, Null)
  | Link (Node node as curr_node) -> begin
      let next = node.next in
      match Atomic.get next with
      | Link (Null | Node _) as next_val ->
          let comp = t.compare key node.key in
          if comp == 0 then begin
            if node.incr != Size.used_once then begin
              Size.update_once t.size node.incr;
              node.incr <- Size.used_once
            end;
            (comp, prev, curr_node)
          end
          else begin
            if comp > 0 then find_node_rec t key next next_val
            else (comp, prev, curr_node)
          end
      | Link (Mark next) ->
          Size.update_once t.size next.decr;
          let after = Link next.node in
          find_node_rec t key prev
            (if Atomic.compare_and_set prev curr after then after
             else Atomic.get prev)
    end

and find_node t key =
  let head = t.head in
  find_node_rec t key head (Atomic.get head)

let rec try_add_rec t key value prev curr =
  let found, prev, before = find_node_rec t key prev curr in
  if found == 0 then false
  else
    let incr = Size.new_once t.size Size.incr in
    let (Node r as after) : (_, _, [ `Node ]) node =
      Node { key; value; next = Atomic.make (Link before); incr }
    in
    if Atomic.compare_and_set prev (Link before) (Link after) then begin
      if r.incr != Size.used_once then begin
        Size.update_once t.size r.incr;
        r.incr <- Size.used_once
      end;
      true
    end
    else
      (* What can make the CAS fail :

         A- prev value has been marked

         B- some nodes have been added after prev

         Case A, [find_node_rec] is going to restart from the beginning of the
         linked list, so no problem.

         Case B, [prev] is still before the node with want to add, so it's
         shorter to retry from it then from [t.head]. *)
      try_add_rec t key value prev (Atomic.get prev)

and try_add t key value = try_add_rec t key value t.head (Atomic.get t.head)

let rec try_remove t key = try_remove_rec t key t.head (Atomic.get t.head)

and try_remove_rec t key prev curr =
  let cond, prev, curr = find_node_rec t key prev curr in
  if cond != 0 then false
  else
    match curr with
    | Null -> false
    | Node curr_node -> (
        match Atomic.get curr_node.next with
        | Link (Mark _) -> false
        | Link ((Null | Node _) as next) as before ->
            let decr = Size.new_once t.size Size.decr in
            let after = Mark { node = next; decr } in
            if Atomic.compare_and_set curr_node.next before (Link after) then (
              find_node_rec t key prev (Atomic.get prev) |> ignore;
              true)
            else try_remove_rec t key prev (Atomic.get prev))

let mem t key =
  let cond, _, _ = find_node t key in
  cond == 0
