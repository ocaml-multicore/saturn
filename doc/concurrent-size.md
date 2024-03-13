# Concurrent Size

We recently added a `Size` counter abstraction for keeping track of the size of
a linearizable data structure. The design is very much inspired by the approach
described in the paper [Concurrent Size](https://arxiv.org/pdf/2209.07100.pdf)
by Gal Sela and Erez Petrank. Roughly speaking, their approach uses an array of
counters, updated using wait-free operations, and wait-free snapshotting of the
counters to compute the size. This is a clever approach and performs well. In
this document we'll briefly motivate and describe the changes we made in our
version. The rest of this document assumes that you are largely familiar with
the approach presented in the paper.

## The problem

Unfortunately, the wait-free counter updates of
[Concurrent Size](https://arxiv.org/pdf/2209.07100.pdf) require that there is a
unique counter for each independent thread of execution. In addition to allowing
scalability, this is a key requirement for making the counter updates, i.e.
increments and decrements, idempotent such that multiple threads may safely
attempt to complete a single counter update in parallel.

While it is definitely possible to create applications where a hard limit on the
number of threads of execution can be enforced, it is arguably too strong a
requirement for a general purpose abstraction. Consider the architecture of
OCaml. While it makes sense to limit the number of domains, and, at the time of
writing this, OCaml actually has a hard limit on the number of domains, OCaml
also allows running relatively large numbers of concurrent (sys)threads on those
domains. This means that the number of independent threads of execution can be
impractically large to have a counter for each thread of execution.

## Idempotent lock-free counter increment

Unlike in [Concurrent Size](https://arxiv.org/pdf/2209.07100.pdf) we do not
strictly require each thread of execution to have its own counter. Rather, we
only _try_ to have one counter per domain to allow scalable counter updates.
This means that counter updates can no longer use the wait-free technique
described in the paper, because it is possible, for example, that two or more
threads within a domain might concurrently try to update the counter allocated
for the domain. Instead, we use a kind of lock-free transactional approach to
performing counter updates.

To understand how the technique works, it is perhaps helpful to first look at a
simplified version of the algorithm for performing idempotent lock-free counter
increments.

Below is a commented signature of what we are trying to achieve:

```ocaml
module type Counter = sig
  type t
  (** Represents a counter. *)

  val create : unit -> t
  (** [create ()] returns a new counter with an initial value of [0]. *)

  val get : t -> int
  (** [get counter] returns the current value of the [counter]. *)

  type increment
  (** Represents an increment operation that can be performed to increment a
      counter at most once. *)

  val new_increment : unit -> increment
  (** [new_increment ()] returns a new increment operation. *)

  val perform : t -> increment -> unit
  (** [perform counter increment] increments the [counter] or does nothing in
      case the [increment] operation has already been performed on the
      [counter].

      Note that an [increment] must only be used to target one specific
      [counter]. *)
end
```

From the signature we can read the idea is that one can create an `increment`
operation that can then later be performed to increment a counter at most once.

The implementation is mostly straightforward:

```ocaml
module Make_counter (Atomic : sig
  type 'a t
  val make : 'a -> 'a t
  val get : 'a t -> 'a
  val compare_and_set : 'a t -> 'a -> 'a -> bool
end) : Counter = struct
  type increment = [ `New | `Done ] ref
  type counter = { value : int; increment : increment }
  type t = counter Atomic.t

  let create () = Atomic.make { value = 0; increment = ref `Done }

  let get counter =
    let tx = Atomic.get counter in
    tx.value

  let new_increment () = ref `New

  let rec perform counter increment =
    let before = Atomic.get counter in
    match !increment with
    | `Done -> ()
    | `New ->
      if increment != before.increment then begin
        before.increment := `Done;
        let after = { value = before.value + 1; increment } in
        if not (Atomic.compare_and_set counter before after) then
          perform counter increment
      end
end
```

The tricky bit, of course, is the `perform` operation. It needs to ensure that
an `increment` operation against a `counter` takes effect at most once. To check
that the given increment had not been done before reading the counter, `perform`
checks that the increment isn't marked as done and that the counter isn't
pointing to the increment. If that is the case, it is safe to try to increment
the counter after ensuring that the previous increment has been marked as done.
It is possible for the `compare_and_set` to fail due to a concurrent or parallel
update attempt with same or some other increment. If that happens, the operation
needs to be retried.

> As an aside, note that in the real size counting mechanism, a single counter
> is allocated per domain. This should make `compare_and_set` failures extremely
> rare and increments should be practically, but not technically, wait-free.

We can check that the logic works when we perform an increment from a single
thread of execution:

```ocaml
# let module Counter = Make_counter (Atomic) in
  let counter = Counter.create () in
  let first = Counter.get counter in
  let increment = Counter.new_increment () in
  Counter.perform counter increment;
  let second = Counter.get counter in
  Counter.perform counter increment;
  let third = Counter.get counter in
  (first, second, third)
- : int * int * int = (0, 1, 1)
```

To build more confidence, we can e.g. use the
[DSCheck](https://github.com/ocaml-multicore/dscheck) model checker to check
that interleavings of atomic operations lead to the expected outcome:

```ocaml
# let module Counter = Make_counter (Dscheck.TracedAtomic) in
  Dscheck.TracedAtomic.trace @@ fun () ->
    let counter = Counter.create () in
    let increments =
      Array.init 2 @@ fun _ ->
      Counter.new_increment () in
    for _=1 to 2 do
      Dscheck.TracedAtomic.spawn @@ fun () ->
      Array.iter (Counter.perform counter) increments
    done;
    Dscheck.TracedAtomic.final @@ fun () ->
    Dscheck.TracedAtomic.check @@ fun () ->
    Counter.get counter = Array.length increments
- : unit = ()
```

Hopefully this simplified version of an internal counter used in the size
counting mechanism helps to understand how the real thing works.

## Foo

```ocaml
module Snapshot = struct
  type t = {
    status : [ `Collecting | `Computing | `Value of int ] Atomic.t;
    counters : int Atomic.t array
  }

  let create n =
    let status = Atomic.make `Collecting in
    let counters = Array.init n @@ fun _ -> Atomic.make 0 in
    { status; counters }

  let is_collecting s = Atomic.get s.status == `Collecting

  let set s i after =
    let snap = s.counters.(i) in
    let before = Atomic.get snap in
    if before < after then
      Atomic.compare_and_set snap before after |> ignore

  let rec forward s i after =
    let snap = s.counters.(i) in
    let before = Atomic.get snap in
    if before < after then
      if not (Atomic.compare_and_set snap before after) then
        forward s i after

  let rec sum_counters s sum i =
    if i < Array.length s.counters then
      let decr = Atomic.get s.counters.(i) in
      let incr = Atomic.get s.counters.(i+1) in
      sum_counters s (sum - decr + incr) (i + 2)
    else
      sum

  let compute s =
    if Atomic.get s.status == `Collecting then
      Atomic.compare_and_set s.status `Collecting `Computing |> ignore;
    if Atomic.get s.status == `Computing then begin
      let n = sum_counters s 0 0 in
      if Atomic.get s.status == `Computing then
        Atomic.compare_and_set s.status `Computing (`Value n) |> ignore
    end;
    match Atomic.get s.status with
    | `Value n -> n
    | (`Collecting | `Computing) -> failwith "impossible"
end
```

```ocaml
type once =
  [ `New of [ `New of int | `Done ] ref
  | `Done ]

type counter = {
  value : int;
  once : [ `New of int | `Done ] ref
}

type size = {
  snapshot : Snapshot.t Atomic.t;
  counters : counter Atomic.t array;
}

type t = size Atomic.t
```

```ocaml
let create () : t =
  let width = 2 in
  let snapshot = Atomic.make (Snapshot.create width) in
  let counters =
    Array.init width @@ fun _ ->
    Atomic.make { value = 0; once = ref `Done }
  in
  Atomic.make { snapshot; counters }
```

```ocaml
let rec update_once size once counter =
  let before = Atomic.get counter in
  match !once with
  | `Done -> ()
  | `New index ->
    if before.once != once then begin
      before.once := `Done;
      let value = before.value + 1 in
      let after = { value; once } in
      if Atomic.compare_and_set counter before after then begin
        let snapshot = Atomic.get size.snapshot in
        if Snapshot.is_collecting snapshot then
          Snapshot.forward snapshot index value
      end
      else update_once size once size.counters.(index)
    end

let update_once (t : t) (once : once) =
  match once with
  | `Done -> ()
  | `New once ->
    match !once with
    | `Done -> ()
    | `New index ->
      let size = Atomic.get t in
      update_once size once size.counters.(index)
```
