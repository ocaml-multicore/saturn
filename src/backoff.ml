type t = int * int ref

let create ?(max = 32) () = (max, ref 1)

let once (maxv, r) =
  r := min (2 * !r) maxv;
  for _ = 1 to 4096 * !r do
    Domain.Sync.cpu_relax ()
  done

let reset (_, r) = r := 1