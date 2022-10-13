type t = { currently : int Atomic.t; total_expected : int }

let init ~total_expected = { currently = Atomic.make 0; total_expected }

let wait { currently; total_expected } =
  Atomic.incr currently;
  while Atomic.get currently < total_expected do
    ()
  done
