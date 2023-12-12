let atomic_get x = Atomic.get (Sys.opaque_identity x)

module Once : sig
  type t

  val used : t
  val create : (t -> unit) -> t
  val use : t -> unit
  val is_alive : t -> bool
  val perform : t -> unit
end = struct
  type t = Used | Alive of { mutable action : t -> unit }

  let used = Used
  let create action = Alive { action }

  let is_alive t =
    match Sys.opaque_identity t with
    | Used -> false
    | Alive r -> r.action != Obj.magic ()

  let use t =
    match Sys.opaque_identity t with
    | Used -> ()
    | Alive r -> if r.action != Obj.magic () then r.action <- Obj.magic ()

  let perform t =
    match Sys.opaque_identity t with
    | Used -> ()
    | Alive r as t ->
        let action = r.action in
        if action != Obj.magic () then action t
end

module Chainable : sig
  type !'a t

  val make : 'a -> 'a t
  val modify_as_once : 'a t -> ('a -> 'a * Once.t) -> Once.t
  val get : 'a t -> 'a
end = struct
  type 'a state = { mutable next : Once.t; self : Once.t; value : 'a }
  type 'a t = 'a state Atomic.t

  let make value = Atomic.make { next = Once.used; self = Once.used; value }

  let perform state =
    if state.next != Once.used then begin
      Once.perform state.next;
      if state.next != Once.used then state.next <- Once.used
    end;
    Once.use state.self

  let modify_as_once t fn =
    let rec retry self =
      let before = atomic_get t in
      perform before;
      if Once.is_alive self then begin
        let value, next = fn before.value in
        let after = { value; self; next } in
        if Atomic.compare_and_set t before after then perform after
        else retry self
      end
    in
    Once.create retry

  let get t =
    let state = atomic_get t in
    perform state;
    state.value
end

module Size : sig
  type t

  val create : unit -> t
  val incr_as_once : t -> Once.t -> Once.t
  val decr_as_once : t -> Once.t -> Once.t
  val get : t -> int
end = struct
  type t = int Chainable.t

  let create () = Chainable.make 0

  let incr_as_once t next =
    Chainable.modify_as_once t @@ fun value -> (value + 1, next)

  let decr_as_once t next =
    Chainable.modify_as_once t @@ fun value -> (value - 1, next)

  let get = Chainable.get
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
  type 'a t = 'a list Chainable.t

  let create () = Chainable.make []

  let push_as_once t x next =
    Chainable.modify_as_once t (fun xs -> (x :: xs, next))

  let pop_as_once t fn =
    Chainable.modify_as_once t (function
      | [] -> ([], fn None)
      | x :: xs -> (xs, fn (Some x)))

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
