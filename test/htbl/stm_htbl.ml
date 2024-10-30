open QCheck
open STM

module STM_htbl (Htbl : Htbls.Htbl_tests) = struct
  let () =
    (* Basics *)
    Random.self_init ();
    let t = Htbl.create () in
    assert (Htbl.try_add t "Basics" 101);
    assert (Htbl.try_add t "Answer" 42);
    assert (101 = Htbl.remove_exn t "Basics");
    assert (not (Htbl.try_remove t "Basics"));
    assert (Htbl.remove_all t |> List.of_seq = [ ("Answer", 42) ]);
    assert (Htbl.to_seq t |> List.of_seq = []);
    [ "One"; "Two"; "Three" ]
    |> List.iteri (fun v k -> assert (Htbl.try_add t k v));
    assert (
      Htbl.to_seq t |> List.of_seq
      |> List.sort (fun l r -> String.compare (fst l) (fst r))
      = [ ("One", 0); ("Three", 2); ("Two", 1) ])

  module Int = struct
    include Int

    let hash = Fun.id
  end

  module Spec = struct
    type cmd =
      | Try_add of int
      | Mem of int
      | Find_opt of int
      | Remove_opt of int
      (* | To_keys *)
      | Remove_all
      | Length
      | Set_exn of int
      | Try_compare_and_set of int * int
      | Try_compare_and_remove of int

    let show_cmd c =
      match c with
      | Try_add x -> "Try_add " ^ string_of_int x
      | Mem x -> "Mem " ^ string_of_int x
      | Remove_opt x -> "Remove_opt " ^ string_of_int x
      (* | To_keys -> "To_keys" *)
      | Remove_all -> "Remove_all"
      | Find_opt x -> "Find_opt " ^ string_of_int x
      | Length -> "Length"
      | Set_exn k -> "Set_exn " ^ string_of_int k
      | Try_compare_and_set (k, v) ->
          "Try_compare_and_set " ^ string_of_int k ^ " " ^ string_of_int v
      | Try_compare_and_remove x -> "Try_compare_and_remove " ^ string_of_int x

    module State = Map.Make (Int)

    type state = int State.t
    type sut = (int, int) Htbl.t

    let arb_cmd _s =
      [
        Gen.int_bound 10 |> Gen.map (fun x -> Try_add x);
        Gen.int_bound 10 |> Gen.map (fun x -> Mem x);
        Gen.int_bound 10 |> Gen.map (fun x -> Remove_opt x);
        (* Gen.return To_keys; *)
        Gen.return Remove_all;
        Gen.int_bound 10 |> Gen.map (fun x -> Find_opt x);
        Gen.return Length;
        Gen.(int_bound 10 >>= fun k -> return (Set_exn k));
        Gen.(
          int_bound 10 >>= fun k ->
          int_bound 10 >>= fun v -> return (Try_compare_and_set (k, v)));
        Gen.int_bound 10 |> Gen.map (fun x -> Try_compare_and_remove x);
      ]
      |> Gen.oneof |> make ~print:show_cmd

    let init_state = State.empty
    let init_sut () = Htbl.create ~hashed_type:(module Int) ()
    let cleanup _ = ()

    let next_state c s =
      match c with
      | Try_add x -> if State.mem x s then s else State.add x x s
      | Mem _ -> s
      | Remove_opt x -> State.remove x s
      (* | To_keys -> s *)
      | Remove_all -> State.empty
      | Find_opt _ -> s
      | Length -> s
      | Set_exn k -> if State.mem k s then State.add k (k + 1) s else s
      | Try_compare_and_set (k, v) -> (
          match State.find_opt k s with
          | Some v' -> if v' = k then State.add k v s else s
          | None -> s)
      | Try_compare_and_remove k -> (
          match State.find_opt k s with
          | Some v -> if k = v then State.remove k s else s
          | None -> s)

    let precond _ _ = true

    let run c d =
      match c with
      | Try_add x -> Res (bool, Htbl.try_add d x x)
      | Mem x -> Res (bool, Htbl.mem d x)
      | Remove_opt x ->
          Res
            ( option int,
              match Htbl.remove_exn d x with
              | x -> Some x
              | exception Not_found -> None )
      (* | To_keys ->
          Res
            ( list (list int),
              Htbl.to_seq d |> List.of_seq |> List.map (fun (a, b) -> [ a; b ])
            ) *)
      | Remove_all ->
          Res
            ( list (list int),
              Htbl.remove_all d |> List.of_seq
              |> List.map (fun (a, b) -> [ a; b ]) )
      | Length -> Res (int, Htbl.length d)
      | Find_opt k -> Res (option int, Htbl.find_opt d k)
      | Set_exn k ->
          Res (result int exn, protect (fun d -> Htbl.set_exn d k (k + 1)) d)
      | Try_compare_and_remove k -> Res (bool, Htbl.try_compare_and_remove d k k)
      | Try_compare_and_set (k, v) ->
          Res (bool, Htbl.try_compare_and_set d k k v)

    let postcond c (s : state) res =
      match (c, res) with
      | Try_add x, Res ((Bool, _), res) -> res <> State.mem x s
      | Mem x, Res ((Bool, _), res) -> res = State.mem x s
      | Remove_opt k, Res ((Option Int, _), res) -> res = State.find_opt k s
      | Remove_all, Res ((List (List Int), _), res) -> (
          try
            let res : (int * int) list =
              List.map (function [ k; v ] -> (k, v) | _ -> raise Exit) res
            in
            List.sort
              (fun (k, _) (k', _) -> Int.compare k k')
              (State.bindings s)
            = List.sort (fun (k, _) (k', _) -> Int.compare k k') res
          with _ -> false)
      | Length, Res ((Int, _), res) -> res = State.cardinal s
      | Find_opt k, Res ((Option Int, _), res) -> State.find_opt k s = res
      | Set_exn k, Res ((Result (Int, Exn), _), res) -> begin
          match State.find_opt k s with
          | Some v -> res = Ok v
          | None -> res = Error Not_found
        end
      | Try_compare_and_remove k, Res ((Bool, _), res) -> (
          match State.find_opt k s with
          | Some v' when v' = k -> res = true
          | _ -> res = false)
      | Try_compare_and_set (k, _), Res ((Bool, _), res) -> (
          match State.find_opt k s with
          | Some v' -> v' = k = res
          | None -> res = false)
      | _, _ -> false
  end

  let run () = Stm_run.run ~name:"Htbl" (module Spec) |> exit

  (* let test () =
       let open Htbl in
       let h = create ~hashed_type:(module Int) () in
       let t1 = try_add h 1 1 in
       let t2 = try Some (set_exn h 1 2) with Not_found -> None in
       let t3 = try_add h 1 1 in
       let t4 = try Some (set_exn h 1 3) with Not_found -> None in
       (t1, t2, t3, t4)

     let test_m () =
       let module State = Map.Make (Int) in
       let m = State.empty in
       let t1 = State.mem 1 m in
       let m = State.add 1 1 m in
       let t2 = State.find_opt 1 m in
       let m = State.add 1 2 m in
       let t3 = State.mem 1 m in
       let m = if t3 then m else State.add 1 1 m in
       let t4 = State.find_opt 1 m in
       (t1, t2, t3, t4)

     let run test n =
       let count = ref 0 in
       let res = ref [] in
       let expected = (true, Some 1, false, Some 2) in
       for _ = 1 to n do
         let r = test () in
         if r <> expected then (
           res := r :: !res;
           incr count)
       done;
       (!count, !res) *)
end

let () =
  (* Both safe and unsafe version have the same body code. We randomly pick one for testing. *)
  Random.self_init ();
  let safe = Random.bool () in
  if safe then
    let module Safe = STM_htbl (Htbls.Htbl) in
    Safe.run () |> exit
  else
    let module Unsafe = STM_htbl (Htbls.Htbl_unsafe) in
    Unsafe.run () |> exit
