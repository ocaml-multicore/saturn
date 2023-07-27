type 'a node = { value : 'a; mutable next : 'a node option; lock : Mutex.t }
type 'a t = { head : 'a node }

let create_node value nextptr =
  { value; next = nextptr; lock = Mutex.create () }

let create key = { head = create_node key None }

(* find the previous node <= key *)
let find_previous_add t key =
  let rec aux prev next =
    match next with
    | Some node when node.value > key -> prev
    | Some node -> aux node node.next
    | None -> prev
  in
  aux t.head t.head.next

(* find the previous node < key *)
let find_previous_remove t key =
  let rec aux prev next =
    match next with
    | Some node when node.value >= key -> prev
    | Some node -> aux node node.next
    | None -> prev
  in
  aux t.head t.head.next

(* add new node in correct position *)
let add t value =
  (* x will point to the new node after insertion *)
  let insert x =
    if x.value = value && not (x == t.head) then (
      Mutex.unlock x.lock;
      false)
    else
      let new_node = create_node value x.next in
      x.next <- Some new_node;
      Mutex.unlock x.lock;
      true
  in
  (* check if prev and cur are still in same position after locking *)
  let rec validate prev cur =
    Mutex.lock prev.lock;
    let verify = find_previous_add t value in
    let temp = verify.next in
    match temp with
    | Some _ when temp == cur && prev == verify -> insert prev
    | None when cur = None && prev == verify -> insert prev
    | _ ->
        Mutex.unlock prev.lock;
        (*Domain.cpu_relax ();*)
        let again = find_previous_add t value in
        validate again again.next
  in
  let start = find_previous_add t value in
  validate start start.next

(* remove node from correct position *)
let remove t value =
  let erase x y =
    if y.value <> value then (
      Mutex.unlock x.lock;
      Mutex.unlock y.lock;
      false)
    else (
      x.next <- y.next;
      Mutex.unlock x.lock;
      Mutex.unlock y.lock;
      true)
  in
  (* check if prev and cur are still in same position after locking *)
  let rec validate prev cur =
    Mutex.lock prev.lock;
    let is_tail = ref false in
    (* get the node to remove from cur pointer *)
    let to_remove =
      match cur with
      | Some node -> node
      | _ ->
          is_tail := true;
          create_node value None
    in
    if !is_tail then (
      Mutex.unlock prev.lock;
      false)
    else (
      Mutex.lock to_remove.lock;
      let verify = find_previous_remove t value in
      let temp = verify.next in
      match temp with
      | Some _ when temp == cur && prev == verify -> erase prev to_remove
      | None when cur = None && prev == verify -> erase prev to_remove
      | _ ->
          Mutex.unlock prev.lock;
          Mutex.unlock to_remove.lock;
          (* Domain.cpu_relax (); *)
          let again = find_previous_remove t value in
          validate again again.next)
  in
  let start = find_previous_remove t value in
  validate start start.next

(* check if key exists in list *)
let contains t value =
  let rec validate prev cur =
    Mutex.lock prev.lock;
    let is_tail = ref false in
    let to_find =
      match cur with
      | Some node -> node
      | _ ->
          is_tail := true;
          create_node value None
    in
    if !is_tail then (
      Mutex.unlock prev.lock;
      false)
    else (
      Mutex.lock to_find.lock;
      let verify = find_previous_remove t value in
      let temp = verify.next in
      match temp with
      | Some _ when temp == cur && prev == verify ->
          let res = to_find.value = value in
          Mutex.unlock prev.lock;
          Mutex.unlock to_find.lock;
          res
      | None ->
          Mutex.unlock prev.lock;
          Mutex.unlock to_find.lock;
          false
      | _ ->
          Mutex.unlock prev.lock;
          Mutex.unlock to_find.lock;
          (* Domain.cpu_relax (); *)
          let again = find_previous_remove t value in
          validate again again.next)
  in
  let start = find_previous_remove t value in
  validate start start.next

(* check if list is empty *)
let is_empty t =
  Mutex.lock t.head.lock;
  let empty = t.head.next = None in
  Mutex.unlock t.head.lock;
  empty
