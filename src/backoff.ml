type t = int * int ref

let k = Domain.DLS.new_key Random.State.make_self_init 

let create ?(max = 32) () = (max, ref 1)

let once (maxv, r) =
  let t = Random.State.int (Domain.DLS.get k) !r in
  r := min (2 * !r) maxv;
  if t = 0 then ()
  else begin
      for _ = 1 to 4096 * t do
        Domain.cpu_relax ()
      done
    end

let reset (_, r) = r := 1
