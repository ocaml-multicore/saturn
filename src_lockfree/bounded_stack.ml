open Picos

type 'a node = Nil | Cons of { value : 'a; tail : 'a node; capacity : int }

type 'a t = {
  head : 'a node Atomic.t;
  cons_waiters : Trigger.t list Atomic.t;
  prod_waiters : Trigger.t list Atomic.t;
  capacity : int;
}

let create ?(capacity = Int.max_int) () =
  let head = Atomic.make_contended Nil in
  let cons_waiters = Atomic.make_contended [] in
  let prod_waiters = Atomic.make_contended [] in
  { head; cons_waiters; prod_waiters; capacity = max capacity 1 }
  |> Multicore_magic.copy_as_padded

let is_empty t = Atomic.get t.head == Nil

let rec signal_all waiters =
  let triggers = Atomic.get waiters in
  if triggers != [] then
    if Atomic.compare_and_set waiters triggers [] then
      List.iter Trigger.signal triggers
    else signal_all waiters

let rec peek t =
  let old_head = Atomic.get t.head in
  match old_head with
  | Nil ->
      let trigger = Trigger.create () in
      let triggers = Atomic.get t.cons_waiters in
      if Atomic.compare_and_set t.cons_waiters triggers (trigger :: triggers)
      then begin
        if Atomic.get t.head != Nil then signal_all t.cons_waiters
        else
          match Trigger.await trigger with
          | None -> ()
          | Some (exn, bt) ->
              signal_all t.cons_waiters;
              Printexc.raise_with_backtrace exn bt
      end;
      peek t
  | Cons cons -> cons.value

let peek_opt t =
  let head = Atomic.get t.head in
  match head with Nil -> None | Cons cons -> Some cons.value

let rec pop t backoff =
  match Atomic.get t.head with
  | Nil ->
      let trigger = Trigger.create () in
      let triggers = Atomic.get t.cons_waiters in
      if Atomic.compare_and_set t.cons_waiters triggers (trigger :: triggers)
      then begin
        if Atomic.get t.head != Nil then signal_all t.cons_waiters
        else
          match Trigger.await trigger with
          | None -> ()
          | Some (exn, bt) ->
              signal_all t.cons_waiters;
              Printexc.raise_with_backtrace exn bt
      end;
      pop t backoff
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t.head old_head cons_r.tail then (
        signal_all t.prod_waiters;
        cons_r.value)
      else pop t (Backoff.once backoff)

let pop t = pop t Backoff.default

let rec pop_opt t backoff =
  match Atomic.get t.head with
  | Nil -> None
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t.head old_head cons_r.tail then (
        signal_all t.prod_waiters;
        Some cons_r.value)
      else pop_opt t (Backoff.once backoff)

let pop_opt t = pop_opt t Backoff.default

let rec push t backoff value =
  match Atomic.get t.head with
  | Nil ->
      let new_head = Cons { value; tail = Nil; capacity = 1 } in
      if Atomic.compare_and_set t.head Nil new_head then
        signal_all t.cons_waiters
      else push t (Backoff.once backoff) value
  | Cons cons_r as old_head ->
      if cons_r.capacity >= t.capacity then begin
        let trigger = Trigger.create () in
        let triggers = Atomic.get t.prod_waiters in
        if Atomic.compare_and_set t.prod_waiters triggers (trigger :: triggers)
        then begin
          if Atomic.get t.head != old_head then signal_all t.prod_waiters
          else
            match Trigger.await trigger with
            | None -> ()
            | Some (exn, bt) ->
                signal_all t.prod_waiters;
                Printexc.raise_with_backtrace exn bt
        end;
        push t backoff value
      end
      else
        let new_head =
          Cons { value; tail = old_head; capacity = cons_r.capacity + 1 }
        in
        if Atomic.compare_and_set t.head old_head new_head then
          signal_all t.cons_waiters
        else push t (Backoff.once backoff) value

let push t value = push t Backoff.default value

let rec try_push t backoff value =
  match Atomic.get t.head with
  | Nil ->
      let new_head = Cons { value; tail = Nil; capacity = 1 } in
      if Atomic.compare_and_set t.head Nil new_head then (
        signal_all t.cons_waiters;
        true)
      else try_push t (Backoff.once backoff) value
  | Cons cons_r as old_head ->
      if cons_r.capacity >= t.capacity then false
      else
        let new_head =
          Cons { value; tail = old_head; capacity = cons_r.capacity + 1 }
        in
        if Atomic.compare_and_set t.head old_head new_head then (
          signal_all t.cons_waiters;
          true)
        else try_push t (Backoff.once backoff) value

let try_push t value = try_push t Backoff.default value

let length t =
  match Atomic.get t.head with Nil -> 0 | Cons cons -> cons.capacity

let rec pop_all t backoff =
  match Atomic.get t.head with
  | Nil -> []
  | old_head ->
      if Atomic.compare_and_set t.head old_head Nil then (
        signal_all t.prod_waiters;
        let rec aux acc = function
          | Nil -> List.rev acc
          | Cons cons -> aux (cons.value :: acc) cons.tail
        in
        aux [] old_head)
      else pop_all t (Backoff.once backoff)

let pop_all t = pop_all t Backoff.default
