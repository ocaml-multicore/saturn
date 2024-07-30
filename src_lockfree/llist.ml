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

(* can we make it a value instead of a function *)
let dummy_content () =
  { incr = Size.used_once; decr = Size.used_once; bindings = []; next = Null }

let[@inline] rec find_node compare size head key =
  find_node_rec compare size head key head (Atomic.get head)

and find_node_rec compare size head key prev curr :
    int
    * (_, _) link Atomic.t
    * (_, _, [< `Null | `Node ]) content
    * (_, _, [< `Null | `Node ]) content =
  match curr with
  | Link { next = Mark _; _ } -> find_node compare size head key
  | Link ({ next = Null; _ } as r) -> (-1, prev, r, dummy_content ())
  | Link ({ next = Node node; _ } as curr_node) -> begin
      match Atomic.get node.content with
      | Link { next = Mark next_node; decr; _ } ->
          Size.update_once size decr;
          let after = Link { curr_node with next = next_node } in
          find_node_rec compare size head key prev
            (if Atomic.compare_and_set prev curr after then after
             else Atomic.get prev)
      | Link ({ next = Null | Node _; incr; _ } as next_val) ->
          if List.is_empty next_val.bindings then
            Size.update_once size next_val.decr;
          let comp = compare key node.key in
          if comp == 0 then begin
            if incr != Size.used_once then begin
              Size.update_once size incr;
              next_val.incr <- Size.used_once
            end;
            (comp, prev, curr_node, next_val)
          end
          else begin
            if comp > 0 then
              find_node_rec compare size head key node.content (Link next_val)
            else (comp, prev, curr_node, next_val)
          end
    end

let[@inline] rec add_replace_rec compare size head key value fbinding prev curr
    =
  let found, prev, curr, next = find_node_rec compare size head key prev curr in
  match (curr.next : (_, _, [ `Node | `Null ]) node) with
  | Node node when found == 0 -> begin
      match next.bindings with
      | [] ->
          let incr = Size.new_once size Size.incr in
          let (Link r as after) =
            Link { next with bindings = fbinding value next.bindings; incr }
          in
          if Atomic.compare_and_set node.content (Link next) after then begin
            if r.incr != Size.used_once then begin
              Size.update_once size r.incr;
              r.incr <- Size.used_once
            end
          end
          else
            add_replace_rec compare size head key value fbinding prev
              (Atomic.get prev)
      | _ ->
          let after =
            Link { next with bindings = fbinding value next.bindings }
          in
          if not @@ Atomic.compare_and_set node.content (Link next) after then
            add_replace_rec compare size head key value fbinding prev
              (Atomic.get prev)
    end
  | _ -> begin
      let decr = Size.used_once in
      let incr = Size.new_once size Size.incr in
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
          Size.update_once size r.incr;
          r.incr <- Size.used_once
        end
      end
      else
        add_replace_rec compare size head key value fbinding prev
          (Atomic.get prev)
    end

let[@inline] add compare size head key value =
  add_replace_rec compare size head key value List.cons head (Atomic.get head)

let replace_last new_binding = function
  | [] -> [ new_binding ]
  | _ :: bindings -> new_binding :: bindings

let[@inline] replace compare size head key value =
  add_replace_rec compare size head key value replace_last head
    (Atomic.get head)

let[@inline] rec add_empty compare size head key =
  add_empty_rec compare size head key head (Atomic.get head)

and add_empty_rec compare size head key prev curr =
  let found, prev, curr, _ = find_node_rec compare size head key prev curr in
  if found == 0 then ()
  else begin
    let incr = Size.used_once in
    let new_content = Link { curr with bindings = []; incr } in
    let after =
      (Node { key; content = Atomic.make new_content } : (_, _, [ `Node ]) node)
    in
    if Atomic.compare_and_set prev (Link curr) (Link { curr with next = after })
    then ()
    else add_empty_rec compare size head key prev (Atomic.get prev)
  end

(* [try_remove ~empty=true t key] can removed empty nodes. Probably not the best
   API for using it in the hashtable, but it enables it to test it ! *)
let[@inline] rec try_remove ?(empty = false) compare size head key =
  try_remove_rec empty compare size head key head (Atomic.get head)

and try_remove_rec empty compare size head key prev curr =
  let found, prev, curr, next = find_node_rec compare size head key prev curr in
  match curr.next with
  | Null -> false
  | Node curr_node ->
      if found != 0 then false
      else begin
        match next.bindings with
        | [] ->
            if empty then begin
              (* removing an empty node *)
              let decr = Size.used_once in
              (* we don't want to decrease the size *)
              let after = { next with decr; next = Mark next.next } in
              if
                Atomic.compare_and_set curr_node.content (Link next)
                  (Link after)
              then (
                find_node_rec compare size head key prev (Atomic.get prev)
                |> ignore;
                true)
              else
                try_remove_rec empty compare size head key prev
                  (Atomic.get prev)
            end
            else false
        | _ :: [] when empty ->
            let decr = Size.new_once size Size.decr in
            let after =
              { next with bindings = []; decr; next = Mark next.next }
            in
            if Atomic.compare_and_set curr_node.content (Link next) (Link after)
            then (
              find_node_rec compare size head key prev (Atomic.get prev)
              |> ignore;
              true)
            else
              try_remove_rec empty compare size head key prev (Atomic.get prev)
        | _ :: [] ->
            let incr = Size.used_once in
            let decr = Size.new_once size Size.decr in
            let after = { next with bindings = []; decr; incr } in
            if Atomic.compare_and_set curr_node.content (Link next) (Link after)
            then (
              find_node_rec compare size head key prev (Atomic.get prev)
              |> ignore;
              true)
            else
              try_remove_rec empty compare size head key prev (Atomic.get prev)
        | _ :: xs ->
            let after = { next with bindings = xs } in
            if Atomic.compare_and_set curr_node.content (Link next) (Link after)
            then true
            else
              try_remove_rec empty compare size head key prev (Atomic.get prev)
      end

let[@inline] mem compare size head key =
  let found, _, _, next = find_node compare size head key in
  match next.bindings with [] -> false | _ -> found == 0

let[@inline] find_opt compare size head key =
  let found, _, curr, next = find_node compare size head key in
  match curr.next with
  | Null -> None
  | Node _ -> (
      if found != 0 then None
      else match next.bindings with [] -> None | first :: _ -> Some first)

let[@inline] find compare size head key =
  let found, _, curr, next = find_node compare size head key in
  match curr.next with
  | Null -> raise Not_found
  | Node _ -> (
      if found != 0 then raise Not_found
      else
        match next.bindings with [] -> raise Not_found | first :: _ -> first)

let[@inline] find_all compare size head key =
  let found, _, _, next = find_node compare size head key in
  if found != 0 then [] else next.bindings

module type LLIST = sig
  type (!'k, !'v) t

  val create : compare:('k -> 'k -> int) -> unit -> ('k, 'v) t
  val length : ('k, 'v) t -> int
  val add : ('k, 'v) t -> 'k -> 'v -> unit
  val add_empty : ('k, _) t -> 'k -> unit
  val replace : ('k, 'v) t -> 'k -> 'v -> unit
  val try_remove : ?empty:bool -> ('k, 'v) t -> 'k -> bool
  val mem : ('k, 'v) t -> 'k -> bool
  val find : ('k, 'v) t -> 'k -> 'v
  val find_opt : ('k, 'v) t -> 'k -> 'v option
  val find_all : ('k, 'v) t -> 'k -> 'v list
end

module Linked_list : LLIST = struct
  type 'k compare = 'k -> 'k -> int

  type ('k, 'v) t = {
    compare : 'k compare;
    head : ('k, 'v) link Atomic.t;
    size : Size.t;
  }

  let length t = Size.get t.size

  let create ~compare () : ('k, 'v) t =
    let head =
      Multicore_magic.copy_as_padded @@ Atomic.make (Link (dummy_content ()))
    in
    let size = Size.create () in
    { size; compare; head }

  let add_empty t = add_empty t.compare t.size t.head
  let add t = add t.compare t.size t.head
  let replace t = replace t.compare t.size t.head
  let try_remove ?(empty = false) t = try_remove ~empty t.compare t.size t.head
  let mem t = mem t.compare t.size t.head
  let find_opt t = find_opt t.compare t.size t.head
  let find t = find t.compare t.size t.head
  let find_all t = find_all t.compare t.size t.head
end
