(** Functorized lock-free linked set with [length] for testing and as an example
    of using [Size].  The functorization is to allow the use of traced atomics
    with DSCheck. *)
module Make (Atomic : sig
  type !'a t

  val make : 'a -> 'a t
  val get : 'a t -> 'a
  val compare_and_set : 'a t -> 'a -> 'a -> bool
end) (Size : sig
  type t

  val create : unit -> t
  val get : t -> int

  type once

  val used_once : once

  type update

  val decr : update
  val incr : update
  val new_once : t -> update -> once
  val update_once : t -> once -> unit
end) : sig
  type 'a t

  val create : unit -> 'a t
  val length : 'a t -> int
  val mem : 'a t -> 'a -> bool
  val try_add : 'a t -> 'a -> bool
  val try_remove : 'a t -> 'a -> bool
end = struct
  type ('a, _) node =
    | Null : ('a, [> `Null ]) node
    | Node : {
        next : 'a link Atomic.t;
        value : 'a;
        mutable incr : Size.once;
      }
        -> ('a, [> `Node ]) node
    | Mark : {
        node : ('a, [< `Null | `Node ]) node;
        decr : Size.once;
      }
        -> ('a, [> `Mark ]) node

  and 'a link = Link : ('a, [< `Null | `Node | `Mark ]) node -> 'a link
  [@@unboxed]

  type 'a t = { size : Size.t; head : 'a link Atomic.t }

  let create () = { size = Size.create (); head = Atomic.make (Link Null) }
  let length t = Size.get t.size

  let rec find_node t prev value : _ -> (_, [< `Null | `Node ]) node = function
    | Link (Mark _) -> find_node t t.head value (Atomic.get t.head)
    | Link Null -> Null
    | Link (Node r as node) as before -> begin
        match Atomic.get r.next with
        | Link (Mark r) ->
            Size.update_once t.size r.decr;
            if Atomic.compare_and_set prev before (Link r.node) then
              find_node t prev value (Link r.node)
            else find_node t prev value (Atomic.get prev)
        | Link (Null | Node _) as next ->
            if r.value == value then begin
              if r.incr != Size.used_once then begin
                Size.update_once t.size r.incr;
                r.incr <- Size.used_once
              end;
              node
            end
            else find_node t r.next value next
      end

  let mem t value = find_node t t.head value (Atomic.get t.head) != Null

  let rec try_add t value =
    let before = Atomic.get t.head in
    match find_node t t.head value before with
    | Node _ -> false
    | Null ->
        let incr = Size.new_once t.size Size.incr in
        let (Node r as after) : (_, [ `Node ]) node =
          Node { next = Atomic.make before; value; incr }
        in
        if Atomic.compare_and_set t.head before (Link after) then begin
          if r.incr != Size.used_once then begin
            Size.update_once t.size r.incr;
            r.incr <- Size.used_once
          end;
          true
        end
        else try_add t value

  let rec try_remove t value =
    match find_node t t.head value (Atomic.get t.head) with
    | Null -> false
    | Node r -> begin
        match Atomic.get r.next with
        | Link (Mark r) ->
            Size.update_once t.size r.decr;
            false
        | Link ((Null | Node _) as node) as before ->
            let decr = Size.new_once t.size Size.decr in
            let after = Mark { node; decr } in
            if Atomic.compare_and_set r.next before (Link after) then begin
              find_node t t.head value (Atomic.get t.head) |> ignore;
              true
            end
            else try_remove t value
      end
end
