module Llist = Lockfree__.Htbl.Llist

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let exclusion l l' =
  let s = IntSet.of_list l in
  let s' = IntSet.of_list l' in
  IntSet.diff s (IntSet.inter s s') |> IntSet.elements

(* making sur the generation int list are small enough (small_nat < 100) *)
let int_list = QCheck.(list_of_size Gen.small_nat small_nat)

let tests_one_domain =
  QCheck.
    [
      (* Add a list of unique elements in a linked list. All
         insertion should success. *)
      Test.make ~name:"seq_add" int_list (fun l ->
          let open Llist in
          let t = init () in

          let l = List.sort_uniq (fun a b -> -compare a b) l in
          List.for_all (fun elt -> add elt Dummy t) l);
      (* Add a list of elements in a linked list and checks :

         - the elements that have already been added, the [add]
         function returns [false] and [true] otherwise.

         - [mem] on all added elements returns [true].
      *)
      Test.make ~name:"seq_add2" int_list (fun l ->
          let open Llist in
          let t = init () in

          let has_been_added = List.map (fun elt -> add elt Dummy t) l in

          let rec loop prev l has_been_added =
            match (l, has_been_added) with
            | [], [] -> true
            | [], _ | _, [] -> false
            | x :: xs, added :: has_been_added ->
                if added = not (List.mem x prev) then
                  loop (x :: prev) xs has_been_added
                else false
          in

          loop [] l has_been_added
          &&
          let uniq = List.sort compare l in
          List.for_all (fun elt -> mem elt t) uniq);
    ]

let tests_two_domains =
  QCheck.
    [
      Test.make ~name:"add_add1" ~count:10000 (pair int_list int_list)
        (fun (l, l') ->
          let open Llist in
          let t = init () in
          let sema = Semaphore.Binary.make false in

          let l = List.sort_uniq compare l in
          let l' = List.sort_uniq compare l' in
          let l' = List.filter (fun elt -> not (List.mem elt l)) l' in

          let d1 =
            Domain.spawn (fun () ->
                while not (Semaphore.Binary.try_acquire sema) do
                  Domain.cpu_relax ()
                done;
                List.map (fun i -> add i Dummy t) l)
          in
          let d2 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.map (fun i -> add i Dummy t) l')
          in
          let res1 = Domain.join d1 in
          let res2 = Domain.join d2 in

          List.for_all (fun i -> i) res1 && List.for_all (fun i -> i) res2);
      Test.make ~name:"add_add" ~count:10000 (pair int_list int_list)
        (fun (l, l') ->
          let open Llist in
          let t = init () in
          let sema = Semaphore.Binary.make false in

          let d1 =
            Domain.spawn (fun () ->
                while not (Semaphore.Binary.try_acquire sema) do
                  Domain.cpu_relax ()
                done;
                List.iter (fun i -> ignore @@ add i Dummy t) l)
          in
          let d2 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.iter (fun i -> ignore @@ add i Dummy t) l')
          in
          let () = Domain.join d1 in
          let () = Domain.join d2 in

          List.for_all (fun elt -> mem elt t) (l @ l'));
      Test.make ~name:"remove_remove"
        (pair int_list (pair int_list int_list))
        (fun (l, (l', l'')) ->
          let open Llist in
          let t = init () in
          let sema = Semaphore.Binary.make false in
          List.iter (fun i -> ignore @@ add i Dummy t) (l @ l' @ l'');

          let d1 =
            Domain.spawn (fun () ->
                while not (Semaphore.Binary.try_acquire sema) do
                  Domain.cpu_relax ()
                done;
                List.iter (fun i -> ignore @@ remove i t) l)
          in
          let d2 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.iter (fun i -> ignore @@ remove i t) l')
          in
          let () = Domain.join d1 in
          let () = Domain.join d2 in

          List.for_all (fun elt -> not (mem elt t)) (l @ l')
          && List.for_all (fun elt -> mem elt t) (exclusion l'' (l @ l')));
      Test.make ~name:"add_remove" ~count:1000 small_nat (fun n ->
          let open Llist in
          let t = init () in
          let sema = Semaphore.Binary.make false in

          let l = List.init n (fun i -> i) in

          let d1 =
            Domain.spawn (fun () ->
                while not (Semaphore.Binary.try_acquire sema) do
                  Domain.cpu_relax ()
                done;
                List.map (fun i -> remove i t) l)
          in
          let d2 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.map (fun i -> add i Dummy t) l)
          in
          let remove = Domain.join d1 in
          let _add = Domain.join d2 in

          List.for_all2
            (fun has_been_removed k ->
              if has_been_removed then not (mem k t) else mem k t)
            remove l);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Llist"
    [
      ("one_domain", to_alcotest tests_one_domain);
      ("two_domains", to_alcotest tests_two_domains);
    ]
;;

main ()
