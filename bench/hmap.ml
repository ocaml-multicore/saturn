module type Interface = sig
  type 'a t
  type key = int

  val create : size_exponent:int -> 'a t
  val add : key -> 'a -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
  val remove : key -> 'a t -> bool
end

module Locked : Interface = struct
  type key = int
  type 'a t = (key, 'a) Hashtbl.t

  let mutex = Mutex.create ()

  let with_mutex f =
    Mutex.lock mutex;
    let v = f () in
    Mutex.unlock mutex;
    v

  let create ~size_exponent =
    let size = Int.shift_left 1 size_exponent in
    Hashtbl.create size

  let add (k : key) (v : 'a) (t : 'a t) =
    with_mutex (fun () ->
        Hashtbl.replace t k v;
        true)

  let find_opt k t = with_mutex (fun () -> Hashtbl.find_opt t k)

  let remove k t =
    with_mutex (fun () ->
        Hashtbl.remove t k;
        true)
end

let implementations =
  [
    ("locked\t\t\t", (module Locked : Interface));
    ("lockfree\t\t", (module Lockfree.Hshtbl : Interface));
    ("lockfree-resizable\t", (module Lockfree.Hshtbl_resizable : Interface));
  ]

(* module Hashtable = HashtblLocked *)
module Hashtable = Lockfree.Hshtbl

let run_bench item_count total_domains size_exponent preload_item_count workload
    (impl_name, implementation) =
  let module Hashtable = (val implementation : Interface) in
  Random.init 0;

  let hashtbl = Hashtable.create ~size_exponent in
  let orchestrator = Orchestrator.init ~total_domains ~rounds:10 in

  let key_range =
    let size = Int.shift_left 1 size_exponent in
    max size preload_item_count
  in
  for _ = 1 to preload_item_count do
    Hashtable.add (Random.int key_range) 0 hashtbl |> ignore
  done;

  (* start domains *)
  let domains =
    List.init total_domains (fun _ ->
        Domain.spawn (fun () ->
            Orchestrator.worker orchestrator (fun () ->
                for _ = 1 to item_count do
                  let key = Random.int key_range in
                  match workload with
                  | `Add_remove ->
                      Hashtable.add key 0 hashtbl |> ignore;
                      Hashtable.remove key hashtbl |> ignore
                  | `Get -> Hashtable.find_opt key hashtbl |> ignore
                done)))
  in
  (* run test *)
  let times = Orchestrator.run orchestrator in
  List.iter Domain.join domains;
  let time_median = Stats.median times in
  let time_stddev = Stats.stddev times in
  Printf.printf "[%s] time median: %f, stddev: %f\n" impl_name time_median
    time_stddev

let run item_count domains size_exponent preload_item_count workload =
  List.iter
    (run_bench item_count domains size_exponent preload_item_count workload)
    implementations

open Cmdliner

let preload_item_count =
  let default = 500 in
  let info =
    Arg.info [ "p"; "preload-items" ] ~docv:"INT"
      ~doc:"Number of items to place in the hashmap before starting benchmark."
  in
  Arg.value (Arg.opt Arg.int default info)

let size_exponent =
  let default = 10 in
  let info =
    Arg.info
      [ "s"; "init-size-exponent" ]
      ~docv:"INT" ~doc:"Initial size (size = 2^parameter)."
  in
  Arg.value (Arg.opt Arg.int default info)

let item_count =
  let default = 100_000 in
  let info =
    Arg.info [ "i"; "item-count" ] ~docv:"INT"
      ~doc:"Number of items to add and remove."
  in
  Arg.value (Arg.opt Arg.int default info)

let domains =
  let default = 4 in
  let info =
    Arg.info [ "d"; "domains" ] ~docv:"INT" ~doc:"Number of domains."
  in
  Arg.value (Arg.opt Arg.int default info)

let workload =
  let default = `Add_remove in
  let info =
    Arg.info [ "w"; "workload" ] ~docv:"[add-remove|get]"
      ~doc:
        "Workload to bench. The Get option might benefit from combination with \
         preloading items to create varying hit rates."
  in
  Arg.value
    (Arg.opt
       (Arg.enum [ ("add-remove", `Add_remove); ("get", `Get) ])
       default info)

let cmd =
  let open Term in
  const run $ item_count $ domains $ size_exponent $ preload_item_count
  $ workload

let () =
  exit @@ Cmd.eval
  @@ Cmd.v (Cmd.info ~doc:"Hashmap Benchmark" "hmap_benchmark") cmd
