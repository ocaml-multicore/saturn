module Array = struct
  include Array

  let get = unsafe_get
  let set = unsafe_set
end

let default_capacity = 512

type 'a s = {
  status : int Atomic.t array;
  buffer : 'a array;
  head : int Atomic.t;
  tail : int Atomic.t;
  rest : 'a s option Atomic.t;
}

type 'a t = { first : 'a s Atomic.t; last : 'a s Atomic.t; dummy : 'a }

let pack_size = Sys.int_size / 2

let make_s ~capacity ~dummy =
  {
    head = Atomic.make 0;
    tail = Atomic.make (-1);
    buffer = Array.make capacity dummy;
    status = Array.init (1 + (capacity / pack_size)) (fun _ -> Atomic.make 0);
    rest = Atomic.make None;
  }

let make ?(capacity = default_capacity) ~dummy () =
  let s = make_s ~capacity ~dummy in
  { first = Atomic.make s; last = Atomic.make s; dummy }

let rec gift_rest t some_s =
  if not (Atomic.compare_and_set t.rest None some_s) then follow_rest t some_s

and follow_rest t some_s =
  match Atomic.get t.rest with
  | None -> gift_rest t some_s
  | Some t -> follow_rest t some_s

let force_rest ~dummy t =
  match Atomic.get t.rest with
  | Some s -> s
  | None -> (
      let s = make_s ~capacity:(Array.length t.buffer) ~dummy in
      let some_s = Some s in
      if Atomic.compare_and_set t.rest None some_s then s
      else
        match Atomic.get t.rest with
        | None -> assert false
        | Some rest ->
            gift_rest rest some_s;
            rest)

let mark t i =
  let status = t.status.(i / pack_size) in
  let shift = 2 * (i mod pack_size) in
  let status = Atomic.fetch_and_add status (1 lsl shift) in
  (status lsr shift) land 1 = 0

let rec push_s ~dummy t x =
  let i = Atomic.fetch_and_add t.tail 1 in
  if i < 0 then
    let _ = force_rest ~dummy t in
    push_s ~dummy t x
  else if i >= Array.length t.buffer then false
  else (
    t.buffer.(i) <- x;
    if mark t i then true
    else
      let hd = Atomic.get t.head in
      let (_ : bool) = Atomic.compare_and_set t.tail (i + 1) hd in
      t.buffer.(i) <- dummy;
      push_s ~dummy t x)

let rec push ({ last; dummy; _ } as t) x =
  let last_s = Atomic.get last in
  if not (push_s ~dummy last_s x) then
    let rest = force_rest ~dummy last_s in
    let (_ : bool) = Atomic.compare_and_set last last_s rest in
    push t x

type 'a pop_result = Is_empty | Wait_for_it | Pop of 'a

let rec pop_s ~dummy t =
  let current_head = Atomic.get t.head in
  if current_head >= Array.length t.buffer then Is_empty
  else if current_head >= Atomic.get t.tail then Wait_for_it
  else
    let i = Atomic.fetch_and_add t.head 1 in
    if i >= Array.length t.buffer then Is_empty
    else if mark t i then pop_s ~dummy t
    else
      let v = t.buffer.(i) in
      t.buffer.(i) <- dummy;
      Pop v

let rec pop t =
  let first = Atomic.get t.first in
  match pop_s ~dummy:t.dummy first with
  | Pop v -> Some v
  | Wait_for_it -> None
  | Is_empty -> (
      match Atomic.get first.rest with
      | None -> None
      | Some rest ->
          let (_ : bool) = Atomic.compare_and_set t.first first rest in
          pop t)
