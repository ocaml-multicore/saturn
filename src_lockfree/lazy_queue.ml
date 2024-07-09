exception Empty

type ('a, _) result =
  | Option : ('a, 'a option) result
  | Value : ('a, 'a) result

type ('a, _) tdt =
  | Nil : ('a, [> `Nil ]) tdt
  | Cons : { value : 'a; mutable next : 'a spine } -> ('a, [> `Cons ]) tdt

and 'a spine = S : ('a, [< `Nil | `Cons ]) tdt -> 'a spine [@@unboxed]

type 'a cons = ('a, [ `Cons ]) tdt

type 'a state =
  | Zero
  | Queue of { head : 'a cons; tail : 'a cons }
  | Snoc of { head : 'a cons; tail : 'a cons; cons : 'a cons }

type 'a t = 'a state Atomic.t

let create () = Atomic.make Zero |> Multicore_magic.copy_as_padded

let rec push t cons backoff =
  let before = Atomic.get t in
  let after =
    match before with
    | Zero -> Queue { head = cons; tail = cons }
    | Queue q -> Snoc { head = q.head; tail = q.tail; cons }
    | Snoc s ->
        let (Cons tl) = s.tail in
        if tl.next == S Nil then tl.next <- S s.cons;
        Snoc { head = s.head; tail = s.cons; cons }
  in
  if not (Atomic.compare_and_set t before after) then
    push t cons (Backoff.once backoff)

let push t value = push t (Cons { value; next = S Nil }) Backoff.default

let rec pop_as : type a r. a t -> (a, r) result -> _ -> r =
 fun t result backoff ->
  match Atomic.get t with
  | Zero -> begin
      match result with Option -> None | Value -> raise_notrace Empty
    end
  | Queue q as before ->
      let (Cons hd) = q.head in
      let after =
        match hd.next with
        | S Nil -> Zero
        | S (Cons _ as head) -> Queue { head; tail = q.tail }
      in
      if Atomic.compare_and_set t before after then
        match result with Value -> hd.value | Option -> Some hd.value
      else pop_as t result (Backoff.once backoff)
  | Snoc s as before ->
      let (Cons tl) = s.tail in
      if tl.next == S Nil then tl.next <- S s.cons;
      let (Cons hd) = s.head in
      let open struct
        external as_cons : 'a spine -> 'a cons = "%identity"
      end in
      let after = Queue { head = as_cons hd.next; tail = s.cons } in
      if Atomic.compare_and_set t before after then
        match result with Value -> hd.value | Option -> Some hd.value
      else pop_as t result (Backoff.once backoff)

let peek_as : type a r. a t -> (a, r) result -> r =
 fun t result ->
  match Atomic.get t with
  | Zero -> begin
      match result with Option -> None | Value -> raise_notrace Empty
    end
  | Queue { head = Cons { value; _ }; _ } | Snoc { head = Cons { value; _ }; _ }
    -> begin
      match result with Value -> value | Option -> Some value
    end

let is_empty t = Zero == Atomic.get t
let[@inline] pop_exn t = pop_as t Value Backoff.default
let[@inline] pop_opt t = pop_as t Option Backoff.default
let[@inline] peek_exn t = peek_as t Value
let[@inline] peek_opt t = peek_as t Option
