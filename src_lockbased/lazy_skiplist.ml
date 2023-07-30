(*
 * RMutex - Reentrant mutexes
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
 *               2011 Edgar Friendly <thelema314@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module RMutex = struct
  type owner =
    {
      thread : int;       (**Identity of the latest owner (possibly the current owner)*)
      mutable depth : int (**Number of times the current owner owns the lock.*)
    }

  type t =
    {
      primitive : Mutex.t; (**A low-level mutex, used to protect access to [ownership]*)
      wait      : Condition.t; (** a condition to wait on when the lock is locked *)
      mutable ownership : owner option;
    }


  let create () =
    {
      primitive = Mutex.create ();
      wait      = Condition.create ();
      ownership = None
    }

  (**
     Attempt to acquire the mutex, waiting indefinitely
   *)
  let lock m =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock m.primitive; (******Critical section begins*)
    (
      match m.ownership with
      | None -> (*Lock belongs to nobody, I can take it. *)
         m.ownership <- Some {thread = id; depth = 1}
      | Some s when s.thread = id -> (*Lock already belongs to me, I can keep it. *)
         s.depth <- s.depth + 1
      | _ -> (*Lock belongs to someone else. *)
         while not (m.ownership = None) do
           Condition.wait m.wait m.primitive
         done;
         m.ownership <- Some {thread = id; depth = 1}
    );
    Mutex.unlock m.primitive (******Critical section ends*)

  (** Attempt to acquire the mutex, returning true if successful.  If
      waiting would be required, return false instead.
   *)
  let try_lock m =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock m.primitive;     (******Critical section begins*)
    let r =
      match m.ownership with
      | None -> (*Lock belongs to nobody, I can take it. *)
         m.ownership <- Some {thread = id; depth = 1};
         true
      | Some s when s.thread = id -> (*Lock already belongs to me, I can keep it. *)
         s.depth <- s.depth + 1;
         true
      | _ -> (*Lock belongs to someone else. *)
         false (* give up *)
    in
    Mutex.unlock m.primitive; (******Critical section ends*)
    r


  (** Unlock the mutex; this function checks that the thread calling
      unlock is the owner and raises an assertion failure if this is not
      the case. It will also raise an assertion failure if the mutex is
      not locked. *)
  let unlock m =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock m.primitive; (******Critical section begins*)
    (match m.ownership with
     | Some s ->
        assert (s.thread = id); (*If I'm not the owner, we have a consistency issue.*)
        if s.depth > 1 then
          s.depth <- s.depth - 1 (*release one depth but we're still the owner*)
        else
          begin
            m.ownership <- None;  (*release once and for all*)
            Condition.signal m.wait   (*wake up waiting threads *)
          end
     | _ -> assert false
    );
    Mutex.unlock m.primitive (******Critical section ends  *)

end

module type Compare = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
  val hash : t -> int
end

module Node (V : Compare) = struct
  type t = Null | Node of data

  and data = {
      mutable key : int;
      item : V.t option;
      next : t array;
      lock : RMutex.t;
      marked : bool Atomic.t;
      fully_linked : bool Atomic.t;
      toplevel : int;
    }

  let node_to_string = function
    | Null -> "Null"
    | Node { item; key; _ } ->
       if Option.is_none item then
         Printf.sprintf "Node(key:%d; item:None)" key
       else
         Printf.sprintf "Node(key:%d; item:%s)" key
           (item |> Option.get |> V.to_string)

  let next_to_string { next; _ } =
    Array.fold_left (fun acc node -> acc ^ "; " ^ node_to_string node) "" next

  let to_string = function
    | Null -> "Null"
    | Node d as n ->
       Printf.sprintf "%s [%s]" (node_to_string n) (next_to_string d)

  let make ?item height =
    let key = match item with Some item -> V.hash item | None -> 0 in
    Node
      {
        key;
        item;
        next = Array.make (height + 1) Null;
        lock = RMutex.create ();
        marked = Atomic.make false;
        fully_linked = Atomic.make false;
        toplevel = height;
      }

  let ( !^ ) = function
    | Null -> failwith "Tried to dereference Null node"
    | Node data -> data

  let ( !!^ ) node = !^(!node)

  let overide_key _key = function
    | Null -> failwith "Tried to overide Null node"
    | Node data -> data.key <- _key

  let lock = function
    | Null -> failwith "Tried to lock Null node"
    | Node { lock; _ } -> RMutex.lock lock

  let unlock = function
    | Null -> failwith "Tried to unlock Null node"
    | Node { lock; _ } -> RMutex.unlock lock
end

(* Caveats: If the hash algorithm encounters a collision, the element
   will fail to be added to the list *)
module Make (V : Compare) = struct
  module Node = Node (V)

  type t = {
      head : Node.t;
      tail : Node.t;
    }

  let maxlevel = 32

  let random_level () =
    let lvl = ref 0 in
    while Random.float 1. < 0.5 && !lvl < maxlevel do
      incr lvl
    done;
    !lvl

  let mk_sentinel key =
    let node = Node.make maxlevel in
    Node.overide_key key node;
    node

  (* Initial structure between the sentinels *)
  let create () =
    let open Node in
    let head = mk_sentinel min_int in
    let tail = mk_sentinel max_int in
    Array.iteri (fun i _ -> !^head.next.(i) <- tail) !^head.next;
    {head; tail}

  let find {head; _} (item : V.t) (preds : Node.t array) (succs : Node.t array) : int =
    let open Node in
    let v = V.hash item in
    let lfound = ref (-1) in
    let pred = ref head in
    for level = maxlevel downto 0 do
      let curr = ref !!^pred.next.(level) in
      while v > !!^curr.key do
        pred := !curr;
        curr := !!^pred.next.(level)
      done;
      if !lfound = -1 && v = !!^curr.key then lfound := level;
      preds.(level) <- !pred;
      succs.(level) <- !curr
    done;
    !lfound

  let contains t (item : V.t) : bool =
    let open Node in
    let preds = Array.make (maxlevel + 1) Node.Null in
    let succs = Array.make (maxlevel + 1) Node.Null in
    let lfound = find t item preds succs in
    lfound <> -1
    && Atomic.get !^(succs.(lfound)).fully_linked
    && not (Atomic.get !^(succs.(lfound)).marked)

  let add t (item : V.t) : bool =
    let open Node in
    let exception False in
    let exception True in
    let toplevel = random_level () in
    let preds = Array.make (maxlevel + 1) Node.Null in
    let succs = Array.make (maxlevel + 1) Node.Null in

    let aux_add () =
      while true do
        let skip = ref false in
        let lfound = find t item preds succs in
        if lfound <> -1 then (
          let node_found = succs.(lfound) in
          if not (Atomic.get !^node_found.marked) then (
            while not (Atomic.get !^node_found.fully_linked) do () done;
            raise False);
          skip := true);
        if not !skip then (
          let highestlocked = ref (-1) in
          Fun.protect (fun () ->
              let pred, succ = (ref Null, ref Null) in
              let valid = ref true in
              let level = ref 0 in
              while !valid && !level <= toplevel do
                pred := preds.(!level);
                succ := succs.(!level);
                lock !pred;
                highestlocked := !level;
                valid :=
                  (not (Atomic.get !!^pred.marked))
                  && (not (Atomic.get !!^succ.marked))
                  && !!^pred.next.(!level) == !succ;
                level := !level + 1
              done;
              if not !valid then skip := true;
              if not !skip then (
                let new_node = make ~item toplevel in
                (* first link succs *)
                for lvl = 0 to toplevel do
                  !^new_node.next.(lvl) <- succs.(lvl)
                done;
                (* then link next fields of preds *)
                for lvl = 0 to toplevel do
                  !^(preds.(lvl)).next.(lvl) <- new_node
                done;
                Atomic.set !^new_node.fully_linked true;
                raise True))
            ~finally:(fun () ->
              for lvl = 0 to !highestlocked do
                unlock preds.(lvl)
              done));
      done;
      failwith "[add] This is unreachable"
    in
    try aux_add () with
    | False -> false
    | True -> true

  let remove t item : bool =
    let open Node in
    let exception False in
    let exception True in
    let victim = ref Node.Null in
    let is_marked = ref false in
    let toplevel = ref (-1) in
    let preds = Array.make (maxlevel + 1) Node.Null in
    let succs = Array.make (maxlevel + 1) Node.Null in

    let aux_remove () =
      while true do
      let lfound = find t item preds succs in
      if lfound <> -1 then victim := succs.(lfound);
      if !is_marked ||
           (lfound <> -1 &&
              (Atomic.get !!^victim.fully_linked
               && !!^victim.toplevel = lfound
               && (not (Atomic.get !!^victim.marked))))
      then
        begin
          if not !is_marked then
            (
              toplevel := !!^victim.toplevel;
              lock !victim;
              if Atomic.get !!^victim.marked then
                (lock !victim; raise False);
              Atomic.set !!^victim.marked true;
              is_marked := true
            );
          let highest_locked = ref (-1) in
          Fun.protect (fun () ->
              let pred = ref Node.Null in
              let valid = ref true in
              let level = ref 0 in
              while (!valid && !level <= !toplevel) do
                pred := preds.(!level);
                lock !pred;
                highest_locked := !level;
                valid := (not (Atomic.get !!^pred.marked))
                         && (!!^pred.next.(!level) == !victim);
                incr level
              done;
              if not !valid then ()
              else (
                for level = !toplevel downto 0 do
                  !^(preds.(level)).next.(level) <- !!^victim.next.(level)
                done;
                unlock !victim;
                raise True)
            )
            ~finally:(fun () ->
              for i = 0 to !highest_locked do
                unlock preds.(i)
              done)
        end
      else  raise False
      done;
      failwith "[remove] This is unreachable"
    in

    try aux_remove () with
    | True -> true
    | False -> false

end
