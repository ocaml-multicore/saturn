let atomic_get x = Atomic.get (Sys.opaque_identity x)

module Once : sig
  type t

  val used : t
  val create : (t -> unit) -> t
  val use : t -> unit
  val is_alive : t -> bool
  val perform : t -> unit
end = struct
  type t = Used | Alive of { mutable alive : bool; action : t -> unit }

  let used = Used
  let create action = Alive { alive = true; action }

  let is_alive t =
    match Sys.opaque_identity t with Used -> false | Alive r -> r.alive

  let use t =
    match Sys.opaque_identity t with Used -> () | Alive r -> r.alive <- false

  let perform t =
    match Sys.opaque_identity t with
    | Used -> ()
    | Alive r as t ->
        if r.alive then begin
          r.action t;
          r.alive <- false
        end
end

module Size : sig
  type t

  val create : unit -> t
  val incr_as_once : t -> Once.t -> Once.t
  val decr_as_once : t -> Once.t -> Once.t
  val get : t -> int
end = struct
  type state = { self : Once.t; next : Once.t; value : int }
  type t = state Atomic.t

  let create () = Atomic.make { self = Once.used; next = Once.used; value = 0 }

  let add_as_once t delta next =
    let rec retry self =
      let before = atomic_get t in
      Once.perform before.next;
      Once.use before.self;
      if Once.is_alive self then begin
        let after = { self; next; value = before.value + delta } in
        if Atomic.compare_and_set t before after then Once.perform after.next
        else retry self
      end
    in
    Once.create retry

  let incr_as_once t next = add_as_once t 1 next
  let decr_as_once t next = add_as_once t (-1) next

  let get t =
    let r = atomic_get t in
    Once.perform r.next;
    Once.use r.self;
    r.value
end

module Stack : sig
  type !'a t

  val create : unit -> 'a t
  val push_as_once : 'a t -> 'a -> Once.t -> Once.t
  val pop_as_once : 'a t -> ('a option -> Once.t) -> Once.t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val pop_and_push : 'a t -> 'a t -> unit
end = struct
  type ('a, _) state =
    | Nil : ('a, [> `Nil ]) state
    | Cons : 'a * ('a, [ `Nil | `Cons ]) state -> ('a, [> `Cons ]) state
    | Mark : {
        self : Once.t;
        next : Once.t;
        state : ('a, [> `Nil | `Cons ]) state;
      }
        -> ('a, [ `Mark ]) state

  type 'a root = Root : ('a, _) state -> 'a root [@@unboxed]
  type 'a t = 'a root Atomic.t

  let create () = Atomic.make (Root Nil)

  let push_as_once t x next =
    let rec retry self =
      match atomic_get t with
      | Root (Mark before_r) as before ->
          Once.perform before_r.next;
          Once.use before_r.self;
          Atomic.compare_and_set t before (Root before_r.state) |> ignore;
          if Once.is_alive self then retry self
      | Root ((Nil | Cons _) as xs) as before ->
          if Once.is_alive self then
            let state = Cons (x, xs) in
            let (Mark r as after) = Mark { self; next; state } in
            if Atomic.compare_and_set t before (Root after) then
              Once.perform r.next
            else retry self
    in
    Once.create retry

  let pop_as_once t on =
    let rec retry self =
      match atomic_get t with
      | Root (Mark before_r) as before ->
          Once.perform before_r.next;
          Once.use before_r.self;
          Atomic.compare_and_set t before (Root before_r.state) |> ignore;
          if Once.is_alive self then retry self
      | Root Nil as before ->
          if Once.is_alive self then
            let next = on None in
            let (Mark r as after) = Mark { self; next; state = Nil } in
            if Atomic.compare_and_set t before (Root after) then
              Once.perform r.next
            else retry self
      | Root (Cons (x, state)) as before ->
          if Once.is_alive self then
            let next = on (Some x) in
            let (Mark r as after) = Mark { self; next; state } in
            if Atomic.compare_and_set t before (Root after) then
              Once.perform r.next
            else retry self
    in
    Once.create retry

  let pop s =
    let result = ref None in
    let action = function
      | None -> Once.used
      | some -> Once.create @@ fun _ -> result := some
    in
    Once.perform (pop_as_once s action);
    !result

  let push s x = Once.perform @@ push_as_once s x Once.used

  let pop_and_push s1 s2 =
    if s1 != s2 then
      Once.perform @@ pop_as_once s1
      @@ function None -> Once.used | Some x -> push_as_once s2 x Once.used
end

module Stack_size_spec = struct
  type cmd = Push of int | Pop_opt | Length

  let show_cmd = function
    | Push x -> "Push " ^ string_of_int x
    | Pop_opt -> "Pop_opt"
    | Length -> "Length"

  type state = int list
  type sut = int Stack.t * Size.t

  let arb_cmd _s =
    QCheck.(
      make ~print:show_cmd
        (Gen.oneof
           [
             Gen.int_range 1 50 |> Gen.map (fun x -> Push x);
             Gen.return Pop_opt;
             Gen.return Length;
           ]))

  let init_state = []
  let init_sut () = (Stack.create (), Size.create ())
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push x -> x :: s
    | Pop_opt -> begin match s with [] -> [] | _ :: s -> s end
    | Length -> s

  let precond _ _ = true

  let run c (s, l) =
    let open STM in
    match c with
    | Push x ->
        Res
          ( unit,
            Once.perform
              (Stack.push_as_once s x (Size.incr_as_once l Once.used)) )
    | Pop_opt ->
        Res
          ( option int,
            let result = ref None in
            let action = function
              | None -> Once.used
              | some ->
                  Size.decr_as_once l @@ Once.create @@ fun _ -> result := some
            in
            Once.perform (Stack.pop_as_once s action);
            !result )
    | Length -> Res (int, Size.get l)

  let postcond c (s : state) res =
    let open STM in
    match (c, res) with
    | Push _x, Res ((Unit, _), ()) -> true
    | Pop_opt, Res ((Option Int, _), res) ->
        res
        = begin
            match s with [] -> None | x :: _ -> Some x
          end
    | Length, Res ((Int, _), res) -> res = List.length s
    | _, _ -> false
end

module Stack_size_seq = STM_sequential.Make (Stack_size_spec)
module Stack_size_par = STM_domain.Make (Stack_size_spec)

module Stack_pair_spec = struct
  type cmd =
    | Push_l of int
    | Push_r of int
    | Pop_opt_l
    | Pop_opt_r
    | Pop_l_push_r

  let show_cmd = function
    | Push_l x -> "Push_l " ^ string_of_int x
    | Push_r x -> "Push_r " ^ string_of_int x
    | Pop_opt_l -> "Pop_opt_l"
    | Pop_opt_r -> "Pop_opt_r"
    | Pop_l_push_r -> "Pop_l_push_r"

  type state = int list * int list
  type sut = int Stack.t * int Stack.t

  let arb_cmd _s =
    QCheck.(
      make ~print:show_cmd
        (Gen.oneof
           [
             Gen.int_range 1 50 |> Gen.map (fun x -> Push_l x);
             Gen.int_range 1 50 |> Gen.map (fun x -> Push_r x);
             Gen.return Pop_opt_l;
             Gen.return Pop_opt_r;
             Gen.return Pop_l_push_r;
           ]))

  let init_state = ([], [])
  let init_sut () = (Stack.create (), Stack.create ())
  let cleanup _ = ()

  let next_state c (l, r) =
    match c with
    | Push_l x -> (x :: l, r)
    | Push_r x -> (l, x :: r)
    | Pop_opt_l ->
        ( begin
            match l with [] -> [] | _ :: l -> l
          end,
          r )
    | Pop_opt_r ->
        ( l,
          begin
            match r with [] -> [] | _ :: r -> r
          end )
    | Pop_l_push_r -> begin
        match l with [] -> ([], r) | x :: l -> (l, x :: r)
      end

  let precond _ _ = true

  let run c (l, r) =
    let open STM in
    match c with
    | Push_l x -> Res (unit, Stack.push l x)
    | Push_r x -> Res (unit, Stack.push r x)
    | Pop_opt_l -> Res (option int, Stack.pop l)
    | Pop_opt_r -> Res (option int, Stack.pop r)
    | Pop_l_push_r -> Res (unit, Stack.pop_and_push l r)

  let postcond c ((l, r) : state) res =
    let open STM in
    match (c, res) with
    | Push_l _x, Res ((Unit, _), ()) -> true
    | Push_r _x, Res ((Unit, _), ()) -> true
    | Pop_opt_l, Res ((Option Int, _), res) ->
        res
        = begin
            match l with [] -> None | x :: _ -> Some x
          end
    | Pop_opt_r, Res ((Option Int, _), res) ->
        res
        = begin
            match r with [] -> None | x :: _ -> Some x
          end
    | Pop_l_push_r, Res ((Unit, _), ()) -> true
    | _, _ -> false
end

module Stack_pair_seq = STM_sequential.Make (Stack_pair_spec)
module Stack_pair_par = STM_domain.Make (Stack_pair_spec)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      Stack_size_seq.agree_test ~count ~name:"STM Stack size test sequential";
      Stack_size_par.agree_test_par ~count ~name:"STM Stack size test parallel";
      Stack_pair_seq.agree_test ~count ~name:"STM Stack pair test sequential";
      Stack_pair_par.agree_test_par ~count ~name:"STM Stack pair test parallel";
    ]
