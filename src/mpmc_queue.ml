let default_capacity = 4096
let spinlock_iterations = 16

type 'a cell =
  | Empty
  | Tombstone
  | Value of 'a

type 'a s =
  { buffer : 'a cell Atomic.t array
  ; head : int Atomic.t
  ; tail : int Atomic.t
  ; rest : 'a s option Atomic.t
  }

type 'a t =
  { first : 'a s Atomic.t
  ; last : 'a s Atomic.t
  }

let make_s ~capacity () =
  { head = Atomic.make 0
  ; tail = Atomic.make (-1)
  ; buffer = Array.init capacity (fun _ -> Atomic.make Empty)
  ; rest = Atomic.make None
  }

let make ?(capacity = default_capacity) () =
  let s = make_s ~capacity () in
  { first = Atomic.make s
  ; last = Atomic.make s
  }

let rec gift_rest t some_s =
  if Atomic.compare_and_set t.rest None some_s
  then ()
  else follow_rest t some_s

and follow_rest t some_s =
  match Atomic.get t.rest with
  | None -> gift_rest t some_s
  | Some t -> follow_rest t some_s

let force_rest t =
  match Atomic.get t.rest with
  | Some s -> s
  | None ->
      let s = make_s ~capacity:(Array.length t.buffer) () in
      let some_s = Some s in
      if Atomic.compare_and_set t.rest None some_s
      then s
      else match Atomic.get t.rest with
           | None -> assert false
           | Some rest ->
              gift_rest rest some_s ;
              rest

let rec push_s t x =
  let i = Atomic.fetch_and_add t.tail 1 in
  if i < 0
  then (let _ = force_rest t in push_s t x)
  else if i >= Array.length t.buffer
  then false
  else begin
    let cell = Array.unsafe_get t.buffer i in
    match Atomic.get cell with
    | Empty ->
        if Atomic.compare_and_set cell Empty (Value x)
        then true
        else begin
          assert (Atomic.get cell = Tombstone) ;
          push_s t x
        end
    | Tombstone ->
        push_s t x
    | Value _ -> assert false
  end

let rec push t x =
  let last = Atomic.get t.last in
  if push_s last x
  then ()
  else begin
    let rest = force_rest last in
    let _ : bool = Atomic.compare_and_set t.last last rest in
    push t x
  end


type 'a pop_result =
  | Is_empty
  | Wait_for_it
  | Pop of 'a

let rec pop_s t =
  let current_head = Atomic.get t.head in
  if current_head >= Array.length t.buffer
  then Is_empty
  else if current_head >= Atomic.get t.tail
  then Wait_for_it
  else
    let i = Atomic.fetch_and_add t.head 1 in
    if i >= Array.length t.buffer
    then Is_empty
    else
      let cell = Array.unsafe_get t.buffer i in
      if i >= Atomic.get t.tail
      then tombstone t cell
      else spinlock ~retry:spinlock_iterations t cell

and tombstone t cell =
  if Atomic.compare_and_set cell Empty Tombstone
  then pop_s t
  else begin match Atomic.get cell with
       | (Value v) as old ->
           assert (Atomic.compare_and_set cell old Tombstone) ;
           Pop v
       | _ -> assert false
       end

and spinlock ~retry t cell =
  match Atomic.get cell with
  | (Value v) as old ->
      assert (Atomic.compare_and_set cell old Tombstone) ;
      Pop v
  | Empty when retry <= 0 ->
      tombstone t cell
  | Empty ->
      Domain.cpu_relax () ;
      spinlock ~retry:(retry - 1) t cell
  | Tombstone ->
      assert false

let rec pop t =
  let first = Atomic.get t.first in
  match pop_s first with
  | Pop v -> Some v
  | Wait_for_it -> None
  | Is_empty ->
      begin match Atomic.get first.rest with
      | None -> None
      | Some rest ->
          let _ : bool = Atomic.compare_and_set t.first first rest in
          pop t
      end
