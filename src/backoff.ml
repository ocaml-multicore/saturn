type t = { min_wait : int; max_wait : int; current : int ref }

let k = Domain.DLS.new_key Random.State.make_self_init

let create ?(min_wait = 17) ?(max_wait = 32 * 4096) () =
  { max_wait; min_wait; current = ref min_wait }

let once { max_wait; current; _ } =
  let t = Random.State.int (Domain.DLS.get k) !current in
  current := min (2 * !current) max_wait;
  if t = 0 then ()
  else
    for _ = 1 to t do
      Domain.cpu_relax ()
    done

let reset { min_wait; current; _ } = current := min_wait
