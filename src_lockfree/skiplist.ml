type 'a markable_reference = { node : 'a node; marked : bool }

and 'a node = {
  key : 'a;
  height : int;
  next : 'a markable_reference Atomic.t array;
}

exception Failed_snip

type 'a t = { head : 'a node; max_height : int }

let min = Obj.new_block 5 5 |> Obj.obj
let max = Obj.new_block 5 5 |> Obj.obj
let null_node = { key = max; height = 0; next = [||] }

let[@inline] create_dummy_node_array sl =
  Array.make (sl.max_height + 1) null_node

let[@inline] create_new_node value height =
  let next =
    Array.init (height + 1) (fun _ ->
        Atomic.make { node = null_node; marked = false })
  in
  { key = value; height; next }

let[@inline] get_random_level sl =
  let rec count_level cur_level =
    if Random.bool () then cur_level
    else if cur_level == sl.max_height then count_level 0
    else count_level (cur_level + 1)
  in
  if sl.max_height = 0 then 0 else count_level 0

let create ?(max_height = 10) () =
  let max_height = Int.max max_height 1 in
  let tail = create_new_node max (max_height - 1) in
  let next =
    Array.init max_height (fun _ -> Atomic.make { node = tail; marked = false })
  in
  let head = { key = min; height = max_height - 1; next } in
  { head; max_height = max_height - 1 }

(** Compares old_node and old_mark with the atomic reference and if they are the same then
    Replaces the value in the atomic with node and mark *)
let compare_and_set_mark_ref (atomic, old_node, old_mark, node, mark) =
  let ({ node = current_node; marked = current_marked } as current) =
    Atomic.get atomic
  in
  current_node == old_node && current_marked = old_mark
  && ((current_node == node && current_marked = mark)
     || Atomic.compare_and_set atomic current { node; marked = mark })

(** Returns true if key is found within the skiplist else false;
    Irrespective of return value, fills the preds and succs array with
    the predecessors nodes with smaller key and successors nodes with greater than
    or equal to key
  *)
let find_in (key, preds, succs, sl) =
  let head = sl.head in
  let rec iterate (prev, curr, succ, mark, level) =
    if mark then
      let snip =
        compare_and_set_mark_ref (prev.next.(level), curr, false, succ, false)
      in
      if not snip then raise Failed_snip
      else
        let { node = curr; marked = _ } = Atomic.get prev.next.(level) in
        let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
        iterate (prev, curr, succ, mark, level)
    else if curr.key != max && curr.key < key then
      let { node = new_succ; marked = mark } = Atomic.get succ.next.(level) in
      iterate (curr, succ, new_succ, mark, level)
    else (prev, curr)
  in
  let rec update_arrays prev level =
    let { node = curr; marked = _ } = Atomic.get prev.next.(level) in
    let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
    try
      let prev, curr = iterate (prev, curr, succ, mark, level) in
      preds.(level) <- prev;
      succs.(level) <- curr;
      if level > 0 then update_arrays prev (level - 1)
      else if curr.key == max then false
      else curr.key = key
    with Failed_snip -> update_arrays head sl.max_height
  in
  update_arrays head sl.max_height

(** Adds a new key to the skiplist sl. *)
let add sl key =
  let top_level = get_random_level sl in
  let preds = create_dummy_node_array sl in
  let succs = create_dummy_node_array sl in
  let rec repeat () =
    let found = find_in (key, preds, succs, sl) in
    if found then false
    else
      let new_node_next =
        Array.map
          (fun element ->
            let mark_ref = { node = element; marked = false } in
            Atomic.make mark_ref)
          succs
      in
      let new_node = { key; height = top_level; next = new_node_next } in
      let pred = preds.(0) in
      let succ = succs.(0) in
      if
        not
          (compare_and_set_mark_ref
             (pred.next.(0), succ, false, new_node, false))
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
              find_in (key, preds, succs, sl) |> ignore;
              set_next ())
          in
          set_next ();
          if level < top_level then update_levels (level + 1)
        in
        if top_level > 0 then update_levels 1;
        true
  in
  repeat ()

let mem sl key =
  let rec search (pred, curr, succ, mark, level) =
    if mark then
      let curr = succ in
      let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
      search (pred, curr, succ, mark, level)
    else if curr.key != max && curr.key < key then
      let pred = curr in
      let curr = succ in
      let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
      search (pred, curr, succ, mark, level)
    else if level > 0 then
      let level = level - 1 in
      let { node = curr; marked = _ } = Atomic.get pred.next.(level) in
      let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
      search (pred, curr, succ, mark, level)
    else if curr.key == max then false
    else curr.key = key
  in
  let pred = sl.head in
  let { node = curr; marked = _ } = Atomic.get pred.next.(sl.max_height) in
  let { node = succ; marked = mark } = Atomic.get curr.next.(sl.max_height) in
  search (pred, curr, succ, mark, sl.max_height)

let remove sl key =
  let preds = create_dummy_node_array sl in
  let succs = create_dummy_node_array sl in
  let found = find_in (key, preds, succs, sl) in
  if not found then false
  else
    let nodeToRemove = succs.(0) in
    let nodeHeight = nodeToRemove.height in
    let rec mark_levels succ level =
      let _ =
        compare_and_set_mark_ref
          (nodeToRemove.next.(level), succ, false, succ, true)
      in
      let { node = succ; marked = mark } =
        Atomic.get nodeToRemove.next.(level)
      in
      if not mark then mark_levels succ level
    in
    let rec update_upper_levels level =
      let { node = succ; marked = mark } =
        Atomic.get nodeToRemove.next.(level)
      in
      if not mark then mark_levels succ level;
      if level > 1 then update_upper_levels (level - 1)
    in
    let rec update_bottom_level succ =
      let iMarkedIt =
        compare_and_set_mark_ref (nodeToRemove.next.(0), succ, false, succ, true)
      in
      let { node = succ; marked = mark } = Atomic.get succs.(0).next.(0) in
      if iMarkedIt then (
        find_in (key, preds, succs, sl) |> ignore;
        true)
      else if mark then false
      else update_bottom_level succ
    in
    if nodeHeight > 0 then update_upper_levels nodeHeight;
    let { node = succ; marked = _ } = Atomic.get nodeToRemove.next.(0) in
    update_bottom_level succ
