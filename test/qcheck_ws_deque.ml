module Ws_deque = Lockfree.Ws_deque.M

(* Sequential building of a deque *)
let deque_of_list l =
  let deque = Ws_deque.create () in
  List.iter (Ws_deque.push deque) l;
  deque
  
(* [deque_to_list] makes no use of (list or array) iterators to avoid
   the order of the resulting list to depend on the order of evaluation of the iterator.
*)

let deque_to_list f q n =
  let rec loop acc = 
    function
    | 0 -> acc
    | n ->  
        let a = f q in
        loop (a :: acc) (n-1)
  in
  loop [] n

let keep_some l =
   List.filter Option.is_some l
   |> List.map Option.get 

let keep_n_first n = List.filteri (fun i _ -> i < n)

let tests_one_domain =
  [
    (* TEST 1 - single domain:
       forall l, l' and with q built by pushing in order (l@l') 
                pop q :: pop q :: pop q :: ... :: [] = List.rev l' *)
    QCheck.(Test.make
              ~name:"conservation_order"
              (pair (list int) (list int)) (fun (l, l') ->
                  assume (l' <> []);
                  let deque = deque_of_list (l@l') in
                  
                  let pop_list = deque_to_list Ws_deque.pop deque (List.length l') in
                  pop_list = l'));

    (* TEST 2 - single domain : 
       forall q of size n, poping m > n times raises Exit (m-n) times. *)
    QCheck.(Test.make
              ~name:"pop_empty_raises_exit" ~count:1
              (pair (list int) small_nat) (fun (l, m) ->
                  assume ( m > 0);
                  let n = List.length l in
                  let m = m + n in
                  let count = ref 0 in
                  let deque = deque_of_list l in
                  
                  for _i = 0 to m-1 do
                    try ignore (Ws_deque.pop deque)
                    with Exit -> incr count
                  done;

                  !count = m - n))]

let tests_two_domains =
  [
    (* TEST 1 with 2 domains and sequential execution. 
       Main domain does pushes THEN a stealer domain steals. 

       This checks : 
       - order is preserved (first push = first steal)
       - Exit is raised only when the deque is empty
    *)
    QCheck.(Test.make
              ~name:"seq_push_steal"
              (pair (list int) small_nat) (fun (l, n) ->
                 (* Main domain pushes in order all elements of l *)
                 let deque = deque_of_list l in

                 (* Then the stealer domain steals n times. If an
                    [Exit] is raised, it is register as a [None] value
                    in the returned list.*)
                 let stealer =
                   Domain.spawn (fun () ->
                       let steal' deque =
                           try Some (Ws_deque.steal deque)
                           with Exit -> None in
                       deque_to_list steal' deque n) in
                 let steal_list = Domain.join stealer |> List.rev in

                 (* The stolen values should be the [n]th first elements of [l]*)
                 (let expected_stolen = keep_n_first n l in
                  let nfirst = keep_n_first (List.length l) steal_list in
                 
                 List.for_all2 (fun found_opt expected ->
                     match found_opt with
                     |  Some found -> found = expected
                     | _ -> false)
                   nfirst expected_stolen)                
                 &&
                  (* The [n - (List.length l)] last values of [steal_list]
                    should be [None] (i.e. the [steal] function had raised [Exit]). *)
                 (let exits = List.filteri (fun i _ -> i >= (List.length l)) steal_list in
                  List.for_all (function None -> true | _ -> false) exits)   
                ));
      
    (* TEST 2 with 2 domains and parallel execution.

       Main domain does pushes at the same time that a stealer domain
       steals.

       This test checks : 
       - order is preserved (first push = first steal) 
       - Exit is raised only when the deque is empty 
    *)
      QCheck.(Test.make
                ~name:"par_push_steal"
                (pair (list small_int) (int_bound 200)) (fun (l, n) ->
                    (* Initialization *)
                    let deque = Ws_deque.create () in      
                    let sema = Semaphore.Binary.make false in
                    
                    (* The stealer domain steals n times. If a value [v]
                       is steal, it is registered as [Some v] in the
                       returned list whereas any [Exit] raised, it is
                       register as a [None].*)
                    let stealer =
                      Domain.spawn (fun () ->
                          Semaphore.Binary.release sema;
                          let steal' deque =
                            let res =
                              try Some (Ws_deque.steal deque)
                              with Exit -> None in
                            Domain.cpu_relax (); res
                          in
                          deque_to_list steal' deque n) in
                    (* Here to make sure the stealer domain has a chance
                       to begin before the main domain is done. *)
                    while not (Semaphore.Binary.try_acquire sema) do Domain.cpu_relax () done;
                    (* Main domain pushes.*)
                    List.iter (fun elt -> Ws_deque.push deque elt; Domain.cpu_relax ()) l;
                    
                    let steal_list = Domain.join stealer |> List.rev in
                    
                    (* We don't know how the pushes and the steals are
                       intertwined but we can check that if [m] values
                       have been stolen, they are the [m] first pushed values. *)
                    List.length steal_list = n
                    &&
                    (let stolen = keep_some steal_list 
                     in
                     let expected_stolen = keep_n_first (List.length stolen) l in
                     stolen = expected_stolen)));

    (* TEST 3 with 2 domains and parallel execution.

       Main domain does sequential pushes and then pops at the same time that a
       stealer domain steals.

       This test checks : 
       - order is preserved (first push = first steal, first push = last pop)
       - no value is both popped and stolen *)
      QCheck.(Test.make
                ~name:"par_push_pop_steal"
                (pair (list small_int) (pair small_nat small_nat)) (fun (l, (nsteal, npop)) ->
                    assume (nsteal+npop > List.length l);
                    (* Initialization - sequential pushes*)
                    let deque = deque_of_list l in      
                    let sema = Semaphore.Binary.make false in
                    let _ =  Random.self_init () in
                    let pop' deque =
                      let res = try Some (Ws_deque.pop deque)
                      with Exit -> None in
                      Domain.cpu_relax (); res in
                    
                    (* The stealer domain steals nsteal times. If a value [v]
                       is steal, it is registered as [Some v] in the
                       returned list whereas any [Exit] raised, it is
                       register as a [None].*)
                    let stealer =
                      Domain.spawn (fun () ->
                          Semaphore.Binary.release sema;
                          let steal' deque =
                            let res =
                              try Some (Ws_deque.steal deque)
                              with Exit -> None in
                            Domain.cpu_relax (); res
                          in
                          deque_to_list steal' deque nsteal) in
                    (* Here to make sure the stealer domain has a chance
                       to begin before the main domain is done. *)
                    while not (Semaphore.Binary.try_acquire sema) do Domain.cpu_relax () done;
                    (* Main domain pops and build a list of popped values. *)
                    let pop_list = deque_to_list pop' deque npop in
                                            
                    let steal_list = Domain.join stealer |> List.rev in
                    
                    (* We don't know how the pushes, pops and steals
                       are intertwined but we can check that if popped
                       and stolen values are ordered properly. *)
                    List.length steal_list = nsteal
                    &&
                    List.length pop_list = npop
                    &&
                    (let stolen = keep_some steal_list
                     in
                     let popped_val = keep_some pop_list
                     in
                     stolen @ popped_val = l)))
]

let tests_three_domains =
  [  
    (* TEST 1 with 2 stealers domains and parallel steals.
    *)
      QCheck.(Test.make
                ~name:"3_dom_par_steal"
                (pair (list small_int) (pair small_nat small_nat)) (fun (l, (ns1, ns2)) ->
                    (* Initialization *)
                    let deque = deque_of_list l in      
                    let sema = Semaphore.Counting.make 2 in
                    let steal nsteal =
                      Semaphore.Counting.acquire sema;
                      let res = Array.make nsteal None in
                      while Semaphore.Counting.get_value sema <> 0 do
                        Domain.cpu_relax ()
                      done;
                      for i = 0 to nsteal -1 do
                        (res.(i) <- try Some (Ws_deque.steal deque) with Exit -> None);
                        Domain.cpu_relax ()
                      done ;
                      res
                    in

                    let stealer1 = Domain.spawn (fun  () -> steal ns1) in
                    let stealer2 = Domain.spawn (fun  () -> steal ns2) in
                    
                    let steal_list1 = Domain.join stealer1  in
                    let steal_list2 = Domain.join stealer2 in

                    let stolen1 = keep_some (Array.to_list steal_list1) in
                    let stolen2 = keep_some (Array.to_list steal_list2) in

                    let expected_stolen = keep_n_first (ns1+ns2) l in

                    let rec compare l l1 l2 =
                      match l, l1, l2 with
                      | [], [] , [] -> true
                      | [], _, _ -> false
                      | _, [], _ -> l = l2
                      | _, _, [] -> l = l1
                      | x :: l', y :: l1', z :: l2' ->
                          begin
                            if x = y && x = z then
                              compare l' l1 l2' || compare l' l1' l2
                            else if x = y then
                              compare l' l1' l2
                            else if x = z then
                              compare l' l1 l2'
                            else false end
                     
                    in
                    
                    Array.length steal_list1 = ns1 &&
                    Array.length steal_list2 = ns2 &&
                    compare expected_stolen stolen1 stolen2
                  ))
    ]


let main () = 
  QCheck_runner.run_tests (tests_one_domain @ tests_two_domains @ tests_three_domains);;

main ()

