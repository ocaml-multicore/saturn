type t = { waiters : int Atomic.t; size : int; passed : int Atomic.t }

let create n = { waiters = Atomic.make n; size = n; passed = Atomic.make 0 }

let await { waiters; size; passed } =
  if Atomic.fetch_and_add passed 1 = size - 1 then (
    Atomic.set passed 0;
    Atomic.set waiters 0);

  while Atomic.get waiters = size do
    Domain.cpu_relax ()
  done;

  Atomic.incr waiters;
  while Atomic.get waiters < size do
    Domain.cpu_relax ()
  done
