let item_count = 2_000_000

module type QUEUE = sig
  type 'a t

  val make : unit -> int t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val name : string
end

module Bench (Q : QUEUE) = struct
  let run () =
    let queue = Q.make () in
    let pusher =
      Domain.spawn (fun () ->
          let start_time = Unix.gettimeofday () in
          for i = 1 to item_count do
            Q.push queue i
          done;
          start_time)
    in
    for _ = 1 to item_count do
      while Option.is_none (Q.pop queue) do
        ()
      done
    done;
    let end_time = Unix.gettimeofday () in
    let start_time = Domain.join pusher in
    let time_diff = end_time -. start_time in
    time_diff

  let bench () =
    let results = ref [] in
    for i = 1 to 10 do
      let time = run () in
      if i > 1 then results := time :: !results
    done;
    let results = List.sort Float.compare !results in
    let median_time = List.nth results 4 in
    let median_throughput = Float.of_int item_count /. median_time in
    Benchmark_result.create_generic ~median_time ~median_throughput Q.name
end

module Spsc_queue = Bench (struct
  include Lockfree.Spsc_queue

  let make () = create ~size_exponent:3
  let rec push t x = try Lockfree.Spsc_queue.push t x with Full -> push t x
  let name = "spsc-queue"
end)

module Mpmc_queue = Bench (struct
  include Lockfree.Mpmc_queue

  let make () = make ~dummy:(-1) ()
  let name = "mpmc-queue"
end)

let bench = [ Spsc_queue.bench; Mpmc_queue.bench ]
