type 'a node = Nil | Next of 'a * 'a node Atomic.t * bool * int
type 'a t = { head : 'a node Atomic.t }
type 'a find_prev_result_t = { found : bool; prev : 'a node Atomic.t }

let make_node v = Next (v, Atomic.make Nil, false, 0)

let create () =
  let dummy = make_node 0 in
  { head = Atomic.make dummy }

let find_prev s v =
  (* get node just before the first node with value >= v *)
  let rec aux prev =
    let prev_snapshot = Atomic.get prev in
    match prev_snapshot with
    | Nil -> failwith "impossible: prev is Nil"
    | Next (pval, curr, pmark, ptag) -> (
        match Atomic.get curr with
        | Nil -> { found = false; prev }
        | Next (_, next, true, _) ->
            let new_prev = Next (pval, next, false, ptag + 1) in
            ignore (Atomic.compare_and_set prev prev_snapshot new_prev);
            aux s.head
        | Next (cval, next, false, ctag) when cval >= v ->
            { found = cval = v; prev }
        | _ -> aux curr)
  in
  aux s.head

let contains s v = (find_prev s v).found

let rec insert s v =
  match find_prev s v with
  | { found = true } -> false
  | { found = false; prev } -> (
      let prev_snapshot = Atomic.get prev in
      match prev_snapshot with
      | Nil -> failwith "impossible case: prev is Nil"
      | Next (_, _, true, _) -> insert s v
      | Next (pval, curr, false, ptag) -> (
          let new_node = Next (v, curr, false, 0) in
          let new_prev = Next (pval, Atomic.make new_node, false, ptag + 1) in
          match Atomic.compare_and_set prev prev_snapshot new_prev with
          | false -> insert s v
          | true -> true))

let rec delete s v =
  match find_prev s v with
  | { found = false } -> false
  | { found = true; prev } -> (
      let prev_snapshot = Atomic.get prev in
      match prev_snapshot with
      | Nil -> failwith "impossible case: prev is Nil"
      | Next (_, _, true, _) -> delete s v
      | Next (pval, curr, false, ptag) -> (
          let curr_snapshot = Atomic.get curr in
          match curr_snapshot with
          | Nil ->
              delete s v (* curr changed by some other concurrent procedure *)
          | Next (cval, _, _, _) when cval != v ->
              delete s v (* some other concurrent procedure changed curr *)
          | Next (cval, _, true, _) ->
              false (* some other delete operation marked it *)
          | Next (cval, next, false, ctag) -> (
              let new_curr = Next (cval, next, true, ctag + 1) in
              match Atomic.compare_and_set curr curr_snapshot new_curr with
              | false -> delete s v
              | true -> (
                  let new_prev = Next (pval, next, false, ptag + 1) in
                  match Atomic.compare_and_set prev prev_snapshot new_prev with
                  | false ->
                      ignore (find_prev s v);
                      true
                  | true -> true))))
