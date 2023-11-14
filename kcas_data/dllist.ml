open Kcas

type 'a t = { prev : 'a t Loc.t; next : 'a t Loc.t }
type 'a node = { node_prev : 'a t Loc.t; node_next : 'a t Loc.t; value : 'a }

external as_list : 'a node -> 'a t = "%identity"
external as_node : 'a t -> 'a node = "%identity"

let get { value; _ } = value [@@inline]

let create () =
  let prev = Loc.make (Obj.magic ()) and next = Loc.make (Obj.magic ()) in
  let list = { prev; next } in
  Loc.set prev list;
  Loc.set next list;
  list

let create_node ~prev ~next value =
  { node_prev = Loc.make prev; node_next = Loc.make next; value }

module Xt = struct
  let remove ~xt node =
    let list = as_list node in
    let next = Xt.exchange ~xt list.next list in
    if next != list then begin
      let prev = Xt.exchange ~xt list.prev list in
      Xt.set ~xt next.prev prev;
      Xt.set ~xt prev.next next
    end

  let is_empty ~xt list = Xt.get ~xt list.prev == list

  let add_node_l ~xt node list =
    let next = Xt.get ~xt list.next in
    assert (Loc.fenceless_get node.node_prev == list);
    Loc.set node.node_next next;
    Xt.set ~xt list.next (as_list node);
    Xt.set ~xt next.prev (as_list node);
    node

  let add_l ~xt value list =
    let next = Xt.get ~xt list.next in
    let node = create_node ~prev:list ~next value in
    Xt.set ~xt list.next (as_list node);
    Xt.set ~xt next.prev (as_list node);
    node

  let add_node_r ~xt node list =
    let prev = Xt.get ~xt list.prev in
    Loc.set node.node_prev prev;
    assert (Loc.fenceless_get node.node_next == list);
    Xt.set ~xt list.prev (as_list node);
    Xt.set ~xt prev.next (as_list node);
    node

  let add_r ~xt value list =
    let prev = Xt.get ~xt list.prev in
    let node = create_node ~prev ~next:list value in
    Xt.set ~xt list.prev (as_list node);
    Xt.set ~xt prev.next (as_list node);
    node

  let move_l ~xt node list =
    let node = as_list node in
    let list_next = Xt.exchange ~xt list.next node in
    if list_next != node then begin
      let node_prev = Xt.exchange ~xt node.prev list in
      let node_next = Xt.exchange ~xt node.next list_next in
      if node_prev != node then begin
        Xt.set ~xt node_prev.next node_next;
        Xt.set ~xt node_next.prev node_prev
      end;
      Xt.set ~xt list_next.prev node
    end

  let move_r ~xt node list =
    let node = as_list node in
    let list_prev = Xt.exchange ~xt list.prev node in
    if list_prev != node then begin
      let node_next = Xt.exchange ~xt node.next list in
      let node_prev = Xt.exchange ~xt node.prev list_prev in
      if node_next != node then begin
        Xt.set ~xt node_prev.next node_next;
        Xt.set ~xt node_next.prev node_prev
      end;
      Xt.set ~xt list_prev.next node
    end

  let take_opt_l ~xt list =
    let next = Xt.get ~xt list.next in
    if next == list then None
    else
      let node = as_node next in
      remove ~xt node;
      Some node.value

  let take_opt_r ~xt list =
    let prev = Xt.get ~xt list.prev in
    if prev == list then None
    else
      let node = as_node prev in
      remove ~xt node;
      Some node.value

  let take_blocking_l ~xt list = Xt.to_blocking ~xt (take_opt_l list)
  let take_blocking_r ~xt list = Xt.to_blocking ~xt (take_opt_r list)

  let transfer_l ~xt t1 t2 =
    let t1_next = Xt.exchange ~xt t1.next t1 in
    if t1_next != t1 then begin
      let t1_prev = Xt.exchange ~xt t1.prev t1 in
      let t2_next = Xt.exchange ~xt t2.next t1_next in
      Xt.set ~xt t2_next.prev t1_prev;
      Xt.set ~xt t1_next.prev t2;
      Xt.set ~xt t1_prev.next t2_next
    end

  let transfer_r ~xt t1 t2 =
    let t1_next = Xt.exchange ~xt t1.next t1 in
    if t1_next != t1 then begin
      let t1_prev = Xt.exchange ~xt t1.prev t1 in
      let t2_prev = Xt.exchange ~xt t2.prev t1_prev in
      Xt.set ~xt t2_prev.next t1_next;
      Xt.set ~xt t1_prev.next t2;
      Xt.set ~xt t1_next.prev t2_prev
    end

  let swap ~xt t1 t2 =
    let t1_next = Xt.get ~xt t1.next in
    if t1_next == t1 then transfer_l ~xt t2 t1
    else
      let t2_prev = Xt.get ~xt t2.prev in
      if t2_prev == t2 then transfer_l ~xt t1 t2
      else
        let t1_prev = Xt.exchange ~xt t1.prev t2_prev
        and t2_next = Xt.exchange ~xt t2.next t1_next in
        Xt.set ~xt t2.prev t1_prev;
        Xt.set ~xt t1.next t2_next;
        Xt.set ~xt t2_next.prev t1;
        Xt.set ~xt t2_prev.next t1;
        Xt.set ~xt t1_next.prev t2;
        Xt.set ~xt t1_prev.next t2

  let[@tail_mod_cons] rec to_list_as_l ~xt f list node =
    if node == list then []
    else f (as_node node) :: to_list_as_l ~xt f list (Xt.get ~xt node.next)

  let to_list_as_l ~xt f list = to_list_as_l ~xt f list (Xt.get ~xt list.next)
  let to_list_l ~xt list = to_list_as_l ~xt get list
  let to_nodes_l ~xt list = to_list_as_l ~xt Fun.id list

  let[@tail_mod_cons] rec to_list_as_r ~xt f list node =
    if node == list then []
    else f (as_node node) :: to_list_as_r ~xt f list (Xt.get ~xt node.prev)

  let to_list_as_r ~xt f list = to_list_as_r ~xt f list (Xt.get ~xt list.prev)
  let to_list_r ~xt list = to_list_as_r ~xt get list
  let to_nodes_r ~xt list = to_list_as_r ~xt Fun.id list
end

let remove node = Kcas.Xt.commit { tx = Xt.remove node }
let is_empty list = Loc.get list.prev == list

let add_l value list =
  let node = create_node ~prev:list ~next:list value in
  Kcas.Xt.commit { tx = Xt.add_node_l node list }

let add_r value list =
  let node = create_node ~prev:list ~next:list value in
  Kcas.Xt.commit { tx = Xt.add_node_r node list }

let move_l node list = Kcas.Xt.commit { tx = Xt.move_l node list }
let move_r node list = Kcas.Xt.commit { tx = Xt.move_r node list }
let take_opt_l list = Kcas.Xt.commit { tx = Xt.take_opt_l list }
let take_opt_r list = Kcas.Xt.commit { tx = Xt.take_opt_r list }
let take_blocking_l list = Kcas.Xt.commit { tx = Xt.take_blocking_l list }
let take_blocking_r list = Kcas.Xt.commit { tx = Xt.take_blocking_r list }
let swap t1 t2 = Kcas.Xt.commit { tx = Xt.swap t1 t2 }
let transfer_l t1 t2 = Kcas.Xt.commit { tx = Xt.transfer_l t1 t2 }
let transfer_r t1 t2 = Kcas.Xt.commit { tx = Xt.transfer_r t1 t2 }
let to_list_l list = Kcas.Xt.commit { tx = Xt.to_list_l list }
let to_list_r list = Kcas.Xt.commit { tx = Xt.to_list_r list }
let to_nodes_l list = Kcas.Xt.commit { tx = Xt.to_nodes_l list }
let to_nodes_r list = Kcas.Xt.commit { tx = Xt.to_nodes_r list }

exception Empty

let take_l list = match take_opt_l list with None -> raise Empty | Some v -> v
let take_r list = match take_opt_r list with None -> raise Empty | Some v -> v

let take_all list =
  let copy = { prev = Loc.make list; next = Loc.make list } in
  let open Kcas in
  let tx ~xt =
    let prev = Xt.exchange ~xt list.prev list in
    if prev == list then begin
      Loc.set copy.prev copy;
      Loc.set copy.next copy
    end
    else
      let next = Xt.exchange ~xt list.next list in
      Xt.set ~xt prev.next copy;
      Xt.set ~xt next.prev copy;
      Loc.set copy.prev prev;
      Loc.set copy.next next
  in
  Xt.commit { tx };
  copy
