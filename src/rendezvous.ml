type 'a t = unit -> ('a -> unit) * (unit -> 'a)

let fail () = failwith "Rendezvous: double send"

let semaphore () =
  let r = ref None in
  let s = Semaphore.Binary.make false in
  let send v =
    r := Some v;
    Semaphore.Binary.release s
  and recv () =
    Semaphore.Binary.acquire s;
    Option.get !r
  in
  (send, recv)

let semaphore_unit () =
  let s = Semaphore.Binary.make false in
  let send () = Semaphore.Binary.release s
  and recv () = Semaphore.Binary.acquire s in
  (send, recv)

let backoff ?min_wait ?max_wait () =
  let r = Atomic.make None in
  let backoff = Backoff.create ?min_wait ?max_wait () in
  let rec send v = if not (Atomic.compare_and_set r None (Some v)) then fail ()
  and recv () =
    match Atomic.get r with
    | Some v -> v
    | None ->
        Backoff.once backoff;
        recv ()
  in
  (send, recv)

let backoff_unit ?min_wait ?max_wait () =
  let r = Atomic.make false in
  let backoff = Backoff.create ?min_wait ?max_wait () in
  let rec send () = if not (Atomic.compare_and_set r false true) then fail ()
  and recv () =
    if not (Atomic.get r) then (
      Backoff.once backoff;
      recv ())
  in
  (send, recv)

let spinlock () =
  let r = Atomic.make None in
  let rec send v = if not (Atomic.compare_and_set r None (Some v)) then fail ()
  and recv () =
    match Atomic.get r with
    | Some v -> v
    | None ->
        Domain.cpu_relax ();
        recv ()
  in
  (send, recv)

let spinlock_unit () =
  let r = Atomic.make false in
  let rec send () = if not (Atomic.compare_and_set r false true) then fail ()
  and recv () =
    if not (Atomic.get r) then (
      Domain.cpu_relax ();
      recv ())
  in
  (send, recv)
