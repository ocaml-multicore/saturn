type markable_reference = { node : node; marked : bool }
(** markable reference: stores a reference to a node and has a field to specify if the original node is marked *)

and node = {
  key : int;
  height : int;
  logical_mark : bool Atomic.t;
  next : markable_reference Atomic.t array;
}

exception Failed_snip

type t = { head : node; max_height : int }

let null_node =
  {
    key = Int.max_int;
    height = 0;
    logical_mark = Atomic.make true;
    next = [||];
  }

(** create_new_node: creates a new node with some value and height *)
let create_new_node value height =
  let next =
    Array.init (height + 1) (fun _ ->
        Atomic.make { node = null_node; marked = false })
  in
  { key = value; height; logical_mark = Atomic.make true; next }

(** create_dummy_node_array: Creates a new array with the different node for each index *)
let create_dummy_node_array sl =
  let arr = Array.make (sl.max_height + 1) null_node in
  arr

(** Get a random level from 0 till max_height (both included), the node will be assigned this height *)
let get_random_level sl =
  let rec count_level cur_level =
    if cur_level == sl.max_height || Random.bool () then cur_level
    else count_level (cur_level + 1)
  in
  count_level 0

(** Create a new skiplist *)
let create ?(max_height = 10) () =
  let tail = create_new_node Int.max_int max_height in
  let next =
    Array.init (max_height + 1) (fun _ ->
        Atomic.make { node = tail; marked = false })
  in
  let head =
    {
      key = Int.min_int;
      height = max_height;
      logical_mark = Atomic.make false;
      next;
    }
  in
  { head; max_height }

(** Compares old_node and old_mark with the atomic reference and if they are the same then
    Replaces the value in the atomic with node and mark *)
let compare_and_set_mark_ref (atomic, old_node, old_mark, node, mark) =
  let current = Atomic.get atomic in
  let set_mark_ref () =
    Atomic.compare_and_set atomic current { node; marked = mark }
  in
  let current_node = current.node in
  current_node == old_node && current.marked = old_mark
  && ((current_node == node && current.marked = mark) || set_mark_ref ())

(** Returns true if key is found within the skiplist else false;
    Irrespective of return value, fills the preds and succs array with
    the predecessors nodes with smaller key and successors nodes with greater than
    or equal to key
  *)
let find_in (key, preds, succs, sl, is_del) =
  let head = sl.head in
  let rec iterate (prev, curr, succ, mark, level) =
    if mark then
      (* need to delete curr if marked, so update prev next ptr to succ *)
      let snip =
        compare_and_set_mark_ref (prev.next.(level), curr, false, succ, false)
      in
      if not snip then raise Failed_snip
      else
        let { node = curr; marked = _ } = Atomic.get prev.next.(level) in
        let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
        iterate (prev, curr, succ, mark, level)
    else if (not is_del) && curr.key <= key then
      (* keep traversing to get key greater than or equal *)
      let { node = new_succ; marked = mark } = Atomic.get succ.next.(level) in
      iterate (curr, succ, new_succ, mark, level)
    else if is_del && curr.key < key then
      (* keep traversing to get key greater than or equal *)
      let { node = new_succ; marked = mark } = Atomic.get succ.next.(level) in
      iterate (curr, succ, new_succ, mark, level)
    else (prev, curr)
  in
  (* find pred and succ at that level *)
  let rec update_arrays prev level =
    let { node = curr; marked = _ } = Atomic.get prev.next.(level) in
    let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
    try
      let prev, curr = iterate (prev, curr, succ, mark, level) in
      (* prev <= key < curr *)
      preds.(level) <- prev;
      succs.(level) <- curr;
      if level > 0 then update_arrays prev (level - 1) else curr.key == key
    with Failed_snip -> update_arrays head sl.max_height
  in
  update_arrays head sl.max_height

(** Adds a new key to the skiplist sl. *)
let push sl key =
  let top_level = get_random_level sl in
  let preds = create_dummy_node_array sl in
  let succs = create_dummy_node_array sl in
  let rec repeat () =
    (* check if key already exists and fill preds and succs *)
    find_in (key, preds, succs, sl, false) |> ignore;
    let new_node_next =
      (* build next array based on succs *)
      Array.map
        (fun element ->
          let mark_ref = { node = element; marked = false } in
          Atomic.make mark_ref)
        succs
    in
    let new_node =
      {
        key;
        height = top_level;
        logical_mark = Atomic.make false;
        next = new_node_next;
      }
    in
    let pred = preds.(0) in
    let succ = succs.(0) in
    (* insert at level 0 *)
    if
      not
        (compare_and_set_mark_ref (pred.next.(0), succ, false, new_node, false))
    then repeat ()
    else
      let rec update_levels level =
        let rec set_next () =
          let pred = preds.(level) in
          let succ = succs.(level) in
          if
            compare_and_set_mark_ref
              (pred.next.(level), succ, false, new_node, false)
          then ()
          else (
            find_in (key, preds, succs, sl, false) |> ignore;
            set_next ())
        in
        set_next ();
        if level < top_level then update_levels (level + 1)
      in
      if top_level > 0 then update_levels 1;
      (* start updating from level 1 and then move upwards *)
      ()
  in
  repeat ()

(** Returns true if the key is within the skiplist, else returns false *)
let contains sl key =
  let rec search (pred, curr, succ, mark, level) =
    if mark then
      (* to be deleted *)
      let curr = succ in
      let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
      search (pred, curr, succ, mark, level)
    else if curr.key < key then
      (* keep iterating to find correct position *)
      let pred = curr in
      let curr = succ in
      let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
      search (pred, curr, succ, mark, level)
    else if level > 0 then
      (* found correct position, find exact level *)
      let level = level - 1 in
      let { node = curr; marked = _ } = Atomic.get pred.next.(level) in
      let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
      search (pred, curr, succ, mark, level)
    else
      curr.key == key (* at the most accurate position, check if key exists *)
  in
  let pred = sl.head in
  let { node = curr; marked = _ } = Atomic.get pred.next.(sl.max_height) in
  let { node = succ; marked = mark } = Atomic.get curr.next.(sl.max_height) in
  search (pred, curr, succ, mark, sl.max_height)

(* find the minimum node on the bottom level and mark it as deleted,
   important to refetch successor node because something could have changed in between *)
let find_mark_min sl =
  let rec find_unmarked curr =
    let { node = not_tail; marked = _ } = Atomic.get curr.next.(0) in
    if not_tail != null_node then
      if
        (not (Atomic.get curr.logical_mark))
        && Atomic.compare_and_set curr.logical_mark false true
      then curr
      else
        let { node = succ; marked = _ } = Atomic.get curr.next.(0) in
        find_unmarked succ
    else null_node
  in
  let { node = curr; marked = _ } = Atomic.get sl.head.next.(0) in
  find_unmarked curr

(** Removes given key from skiplist, unlinking the next pointers *)
let remove sl key =
  let preds = create_dummy_node_array sl in
  let succs = create_dummy_node_array sl in
  let rec repeat () =
    find_in (key, preds, succs, sl, true) |> ignore;
    let nodeToRemove = succs.(0) in
    (* expected node to remove based on given key *)
    let nodeHeight = nodeToRemove.height in
    let rec mark_levels succ level =
      (* set node to marked *)
      let _ =
        compare_and_set_mark_ref
          (nodeToRemove.next.(level), succ, false, succ, true)
      in
      let { node = succ; marked = mark } =
        Atomic.get nodeToRemove.next.(level)
      in
      if not mark then
        mark_levels succ level (* some update happened to next so retry *)
    in
    let rec update_upper_levels level =
      (* from node height to 1 *)
      let { node = succ; marked = mark } =
        Atomic.get nodeToRemove.next.(level)
      in
      if not mark then mark_levels succ level;
      if level > 1 then update_upper_levels (level - 1)
    in
    let rec update_bottom_level succ =
      (* for bottom level only *)
      let iMarkedIt =
        compare_and_set_mark_ref (nodeToRemove.next.(0), succ, false, succ, true)
      in
      let { node = succ; marked = mark } = Atomic.get succs.(0).next.(0) in
      if iMarkedIt then (
        (* update next links to remove marked node in all levels *)
        find_in (key, preds, succs, sl, true) |> ignore;)
      else if mark then repeat () (* some other thread deleted same key *)
      else
        update_bottom_level
          succ (* retry because some update happened in between *)
    in
    if nodeHeight > 0 then update_upper_levels nodeHeight;
    let { node = succ; marked = _ } = Atomic.get nodeToRemove.next.(0) in
    update_bottom_level succ
  in
  repeat ()

(** remove smallest key from priority queue, first mark logically and then physical removal *)
let pop sl =
  let num = find_mark_min sl in
  if num != null_node then (
    remove sl num.key;
    num.key)
  else null_node.key
  