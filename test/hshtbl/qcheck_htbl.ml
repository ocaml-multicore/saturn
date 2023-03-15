module Htbl = Lockfree.Hshtbl_resizable

(* making sure the generation int list are small enough (small_nat < 100) *)
let nat_list = QCheck.(list_of_size Gen.small_nat small_nat)
let nat_int_list = QCheck.(list_of_size Gen.small_nat (pair small_nat int))
let count = 10_000

(* this function makes sure that all pairs in [l1] and [l2] have a
   different key without sorting the lists.

   We want to avoid sorting the list as the linked list below the hash
   table is sorted and adding sorted elements might avoid some bugs.

   The maximun key value of l1 is added to all keys of l2 to create
   new unique keys and avoid reducing the lists to much
*)
let avoid_dup_keys l1 l2 =
  let max_l1, l1_uniq =
    List.fold_left
      (fun (m, acc) (k, v) ->
        if List.mem_assoc k acc then (m, acc) else (max m k, (k, v) :: acc))
      (0, []) l1
  in
  let l2_uniq =
    List.fold_left
      (fun acc (k, v) -> if List.mem_assoc k acc then acc else (k, v) :: acc)
      [] l2
    |> List.map (fun (k, v) -> (k + max_l1 + 1, v))
  in
  (l1_uniq, l2_uniq)

let tests_one_domain =
  QCheck.
    [
      (* Add a list of unique elements in a linked list. All
         insertion should success. *)
      Test.make ~count ~name:"seq_add" nat_int_list (fun l ->
          let open Htbl in
          let t = init ~size_exponent:4 in
          let l = List.sort_uniq (fun (a, _) (b, _) -> compare a b) l in
          List.for_all (fun (k, v) -> add k v t) l);
      (* Add a list of elements in a linked list and checks :

         - the elements that have already been added, the [add]
         function returns [false] and [true] otherwise.

         - [mem] on all added elements returns [true].
      *)
      Test.make ~count ~name:"seq_add_mem" nat_int_list (fun l ->
          let open Htbl in
          let t = init ~size_exponent:4 in

          let has_been_added = List.map (fun (k, v) -> add k v t) l in

          let rec loop prev l has_been_added =
            match (l, has_been_added) with
            | [], [] -> true
            | [], _ | _, [] -> false
            | (k, _) :: xs, added :: has_been_added ->
                if added = not (List.mem k prev) then
                  loop (k :: prev) xs has_been_added
                else false
          in

          loop [] l has_been_added
          &&
          let uniq = List.sort (fun (a, _) (b, _) -> compare a b) l in
          List.for_all (fun (k, _) -> mem k t) uniq);
      (* Add a list of elements and then remove then. Tested properties are :
         - added and removed elements are the same
         - mem on all elements return false
      *)
      Test.make ~count ~name:"seq_remove" nat_int_list (fun l ->
          let open Htbl in
          let t = init ~size_exponent:4 in

          let has_been_added = List.map (fun (k, v) -> add k v t) l in
          let has_been_removed = List.map (fun (k, _) -> remove k t) l in

          (* Tested properties *)
          has_been_added = has_been_removed
          && List.for_all (fun (k, _) -> not (mem k t)) l);
      (* Add a list of elements and then search random keys.
         This test checks that
         forall k, l,  List.assoc_opt k l = Htbl.find k t
         where t = List.iter (fun (k, v) -> Htbl.add k v t |> ignore) l (Htbl.init ~size_exponent:n)
      *)
      Test.make ~count ~name:"seq_find" (pair nat_int_list nat_list)
        (fun (to_add, to_search_for) ->
          let open Htbl in
          let t = init ~size_exponent:4 in

          List.iter (fun (k, v) -> add k v t |> ignore) to_add;
          let found = List.map (fun k -> find k t) to_search_for in

          (* Tested properties *)
          List.for_all2
            (fun k found -> List.assoc_opt k to_add = found)
            to_search_for found);
      (* Add a list of elements and then search for then specifically.
      *)
      Test.make ~count ~name:"seq_find2" nat_int_list (fun l ->
          let open Htbl in
          let t = init ~size_exponent:4 in

          let has_been_added = List.map (fun (k, v) -> add k v t) l in
          let should_be_found = List.map (fun (k, _) -> find k t) l in
          let res = List.combine has_been_added should_be_found in

          (* Tested properties *)
          List.for_all2
            (fun (k, v) (added, found) ->
              match (added, found) with
              | true, Some r -> r = v
              | false, Some r ->
                  List.assoc_opt k l = Some r
                  (* if adding the element did not work, that means
                     the key is already used *)
              | _, _ -> false)
            l res
          && List.for_all Option.is_some should_be_found);
      (* Replace a list of elements with unique keys twice :
         - first time = add,
         - second time = replace with value +1
      *)
      Test.make ~count ~name:"seq_replace" nat_int_list (fun l ->
          let open Htbl in
          let t = init ~size_exponent:4 in
          let l = List.sort_uniq (fun (k, _) (k', _) -> compare k k') l in

          List.iter (fun (k, v) -> replace k v t) l;
          List.iter (fun (k, v) -> replace k (v + 1) t) l;

          (* Tested properties *)
          List.for_all
            (fun (k, v) ->
              match find k t with None -> false | Some v' -> v' = v + 1)
            l);
    ]

let tests_two_domain =
  QCheck.
    [
      (* Two domains add elements that all have a different key. The
         tested properties is that no element is missing at the
         end. *)
      Test.make ~count ~name:"par_add" (pair nat_int_list nat_int_list)
        (fun (l1, l2) ->
          let l1, l2 = avoid_dup_keys l1 l2 in
          let open Htbl in
          let t = init ~size_exponent:4 in
          let sema = Semaphore.Binary.make false in

          let domain2 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.map
                  (fun (k, v) ->
                    Domain.cpu_relax ();
                    add k v t)
                  l2)
          in

          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;

          let added_by_d1 =
            List.map
              (fun (k, v) ->
                Domain.cpu_relax ();
                add k v t)
              l1
          in
          let added_by_d2 = Domain.join domain2 in

          (* test properties : all elements have been added and can be
             sequentially found *)
          List.for_all (fun a -> a) added_by_d1
          && List.for_all (fun a -> a) added_by_d2
          && List.for_all (fun (k, v) -> find k t = Some v) l1
          && List.for_all (fun (k, v) -> find k t = Some v) l2);
      (* Parallel removal of elements in a sequentially built hash table. *)
      Test.make ~count ~name:"par_remove" nat_int_list (fun to_add ->
          let open Htbl in
          let t = init ~size_exponent:4 in
          let sema = Semaphore.Binary.make false in
          (* Sequential add *)
          let added = List.map (fun (k, v) -> add k v t) to_add in

          let domain2 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.map
                  (fun (k, _v) ->
                    Domain.cpu_relax ();
                    remove k t)
                  to_add)
          in

          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;

          let removed_by_d1 =
            List.map
              (fun (k, _v) ->
                Domain.cpu_relax ();
                remove k t)
              to_add
          in
          let removed_by_d2 = Domain.join domain2 in

          let rec test added removed1 removed2 =
            match (added, removed1, removed2) with
            | [], [], [] -> true
            | false :: xs, false :: xs', false :: xs''
            | true :: xs, true :: xs', false :: xs''
            | true :: xs, false :: xs', true :: xs'' ->
                test xs xs' xs''
            | _, _, _ -> false
          in

          test added removed_by_d1 removed_by_d2);
      (* Parallel addition and removal of elements in a sequentially built hash table. *)
      Test.make ~count ~name:"par_add_remove" (pair nat_int_list nat_int_list)
        (fun (lseq, lpar) ->
          let to_add_seq, to_add_par = avoid_dup_keys lseq lpar in
          let to_remove = List.rev lpar in
          let open Htbl in
          let t = init ~size_exponent:4 in
          let sema = Semaphore.Binary.make false in

          (* sequential adds *)
          let added_seq = List.map (fun (k, v) -> add k v t) to_add_seq in

          (* parallel removals *)
          let domain2 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                List.map
                  (fun (k, _v) ->
                    Domain.cpu_relax ();
                    remove k t)
                  to_remove)
          in

          while not (Semaphore.Binary.try_acquire sema) do
            Domain.cpu_relax ()
          done;

          (* parallel adds *)
          let added_par =
            List.map
              (fun (k, v) ->
                Domain.cpu_relax ();
                add k v t)
              to_add_par
          in
          let removed = Domain.join domain2 in

          let rec test to_remove removed prev =
            match (to_remove, removed) with
            | [], [] -> true
            | (k, _) :: to_remove, false :: removed ->
                if List.mem k prev then test to_remove removed prev
                else if List.mem_assoc k to_add_seq then false
                else test to_remove removed prev
            | (k, _) :: to_remove, true :: removed ->
                if List.mem k prev then false
                else test to_remove removed (k :: prev)
            | _, _ -> false
          in

          (* to_add_seq and to_add_par have no identical key or
             duplicated key so addition should always succeed. *)
          (* removals are done on keys added by parallel addition,
             meaning it can fail. *)
          List.for_all (fun a -> a) added_seq
          && List.for_all (fun a -> a) added_par
          && test to_remove removed []);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Hshtbl"
    [
      ("one_domain", to_alcotest tests_one_domain);
      ("two_domain", to_alcotest tests_two_domain);
    ]
;;

main ()
