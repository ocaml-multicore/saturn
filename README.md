[API Reference](https://ocaml-multicore.github.io/saturn/) &middot;
[Benchmarks](https://bench.ci.dev/ocaml-multicore/saturn/branch/main?worker=pascal&image=bench.Dockerfile)
&middot;
[Stdlib Benchmarks](https://bench.ci.dev/ocaml-multicore/multicore-bench/branch/main?worker=pascal&image=bench.Dockerfile)

<!--
```ocaml
# #thread
```
-->

# Saturn — Parallelism-Safe Data Structures for Multicore OCaml

This repository is a collection of parallelism-safe data structures for OCaml 5.
They are contained in two packages:

- Saturn that includes all data structures (including the lock-free ones) and
  should be used by default if you just want parallelism-safe data structures;
- Saturn_lockfree that includes only lock-free data structures.

It aims to provide an industrial-strength, well-tested (and possibly
model-checked and verified in the future), well documented, and maintained
parallelism-safe data structure library. We want to make it easier for Multicore
OCaml users to find the right data structures for their uses.

**Saturn** is published on [opam](https://opam.ocaml.org/packages/saturn/) and
is distributed under the
[ISC license](https://github.com/ocaml-multicore/saturn/blob/main/LICENSE.md).

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml-multicore%2Fsaturn%2Fmain&logo=ocaml&style=flat-square)](https://ci.ocamllabs.io/github/ocaml-multicore/saturn)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/ocaml-multicore/saturn?style=flat-square&color=09aa89)](https://github.com/ocaml-multicore/saturn/releases/latest)
[![docs](https://img.shields.io/badge/doc-online-blue.svg?style=flat-square)](https://ocaml-multicore.github.io/saturn/)

# Contents

- [Installation](#installation)
- [Introduction](#introduction)
  - [Provided data structures](#provided-data-structures)
  - [Motivation](#motivation)
- [Usage](#usage)
  - [Data structures with domain roles](#data-structures-with-domain-roles)
  - [About composability](#about-composability)
- [Testing](#testing)
- [Benchmarks](#benchmarks)

# Installation

## Getting OCaml 5.0

You'll need OCaml 5.0.0 or later. Note that Saturn also works with OCaml 4.14
but only for compatibility reasons, as there is no need for parallelism-safe
data structures without OCaml 5.0.

To install OCaml 5.0 yourself, first make sure you have opam 2.1 or later. You
can run this command to check:

```sh
opam --version
```

Then use opam to install OCaml 5.0.0:

```sh
opam switch create 5.0.0
```

If you want a later version, you can run the following line to get a list of all
available compiler versions:

```sh
opam switch list-available
```

## Getting Saturn

`saturn` can be installed from `opam`:

```sh
opam install saturn
```

# Introduction

## Provided data structures

| Name                            | Module in `Saturn` <br> (in `Saturn_lockfree`)                                                                                                                                                          | Description                                                                                                                                                                                                                                                                      | Sources                                                                                                                                                                                                      |
| ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Treiber Stack                   | [`Stack`](https://ocaml-multicore.github.io/saturn/saturn_lockfree/Lockfree/Stack/index.html) (same)                                                                                                    | A classic multi-producer multi-consumer stack, robust and flexible. Recommended starting point when needing a LIFO structure                                                                                                                                                     |                                                                                                                                                                                                              |
| Michael-Scott Queue             | [`Queue`](https://ocaml-multicore.github.io/saturn/saturn_lockfree/Lockfree/Queue/index.html) (same)                                                                                                    | A classic multi-producer multi-consumer queue, robust and flexible. Recommended starting point when needing a FIFO structure.                                                                                                                                                    | [Simple, Fast, and Practical Non-Blocking and Blocking Concurrent Queue Algorithms](https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf)                                                         |
| Chase-Lev Work-Stealing Dequeue | [`Work_stealing_deque`](https://ocaml-multicore.github.io/saturn/saturn_lockfree/Lockfree/Work_stealing_deque/index.html) (same)                                                                        | Single-producer, multi-consumer, dynamic-size, double-ended queue (deque). Ideal for throughput-focused scheduling using per-core work distribution. Note, `pop` and `steal` follow different ordering (respectively LIFO and FIFO) and have different linearisation contraints. | [Dynamic Circular Work-Stealing Deque](https://dl.acm.org/doi/10.1145/1073970.1073974) and [Correct and Efficient Work-Stealing for Weak Memory Models](https://dl.acm.org/doi/abs/10.1145/2442516.2442524)) |
| SPSC Queue                      | [`Single_prod_single_`<br>`cons_queue`](https://ocaml-multicore.github.io/saturn/saturn_lockfree/Lockfree/Single_prod_single_cons_queue/index.html) (same)                                              | Simple single-producer single-consumer fixed-size queue. Thread-safe as long as at most one thread acts as producer and at most one as consumer at any single point in time.                                                                                                     |                                                                                                                                                                                                              |
| MPMC Bounded Relaxed Queue      | [`Relaxed_queue`](https://ocaml-multicore.github.io/saturn/saturn/Saturn/Relaxed_queue/index.html) ([same](https://ocaml-multicore.github.io/saturn/saturn_lockfree/Lockfree/Relaxed_queue/index.html)) | Multi-producer, multi-consumer, fixed-size relaxed queue. Optimised for high number of threads. Not strictly FIFO. Note, it exposes two interfaces: a lockfree and a non-lockfree (albeit more practical) one. See the `mli` for details.                                        |                                                                                                                                                                                                              |
| MPSC Queue                      | [`Single_consumer_queue`](https://ocaml-multicore.github.io/saturn/saturn_lockfree/Lockfree/Single_consumer_queue/index.html) (same)                                                                    | A multi-producer, single-consumer, thread-safe queue without support for cancellation. This makes a good data structure for a scheduler's run queue. It is used in [Eio](https://github.com/ocaml-multicore/eio).                                                                | It is a single consumer version of the queue described in [Implementing Lock-Free Queues](https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf).                      |

## Motivation

The following part is a beginner-friendly example to explain why we need data
structures specifically designed for multicore programming.

Let's write a classic mutable data structure : a queue. We are going to do a
basic implementation the way it may be done for sequential use and show why it
is not working well with multiple domains.

```ocaml
type queue = int list ref

let create () : queue = ref []

let push q a = q := a :: !q
```

What happens if we try to use this queue with multiple domains? First, let's
define the work we want a single domain to do: each domain will push 10 times
its `id` in the queue.

```ocaml
let work id q = for i = 0 to 9 do push q id done
```

Then let's define our test : it spawns 2 domains that each execute `work` in
parallel. `test` returns the content of the queue as well as its length, so we
can easily see if it contains the 20 elements we expect.

```ocaml
 let test () =
   let q = create () in
   let domainA = Domain.spawn (fun () -> work 1 q) in
   let domainB = Domain.spawn (fun () -> work 2 q) in
   Domain.join domainA;
   Domain.join domainB;
   (List.length !q, !q)
```

Let's try it :

```ocaml
# test ()
- : int * int list =
(20, [2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1])
```

Everything seems fine, right? Except, it is not running in parallel, as we can
see from the consecutive `2` (pushed by `domainB`) and `1` pushed by `domainA`.
This is because spawning a domain takes way more time than executing `work`, so
`domainA` is long finished before `domainB` is even spawned. One way to overpass
this issue is to increase the amount of work done in `work` (for example, by
pushing more elements). Another way is to make sure the domains wait for each
other before beginning their workload.

We use a basic
[barrier implementation](https://github.com/ocaml-multicore/saturn/blob/main/test/barrier/barrier.mli)
to do that. Each domain will now wait for the other to reach the barrier before
beginning to push.

```ocaml
let work_par id barrier q =
  Barrier.await barrier;
  for i = 0 to 9 do
    push q id
  done
```

The `test` function is now:

```ocaml
let test_par () =
  let barrier = Barrier.create 2 in
  let q = create () in
  let domainA = Domain.spawn (fun () -> work_par 1 barrier q) in
  let domainB = Domain.spawn (fun () -> work_par 2 barrier q) in
  Domain.join domainA;
  Domain.join domainB;
  (List.length !q, !q)
```

Let's run it:

```ocaml
# test_par ();;
- : int * int list =
(18, [2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 2; 1; 1; 1; 1; 2; 1])
```

Now, the `1` and the `2` are interleaved: domains are running in parallel. The
resulting queue however only contains `18` elements whereas `20` were pushed.
This is because we not only have a
[race condition](https://en.wikipedia.org/wiki/Race_condition) here but also
because `push` is a non-atomic operation. It requires first to read the content
of the queue (`!q`) then to write in it (`q := ...`). So, for example, when two
domains try to push in parallel into an empty queue the following sequence can
happen:

- domain A reads the queue : it is empty
- domain B reads the queue : it is still empty
- domain A pushes `1` on the empty queue it has read before
- domain B pushes `2` on the empty queue it has read before.

This sequence results in a queue containing only one element of value `2`. The
element pushed by A is lost because B did not see it.

This is a very common issue in parallel programming. To prevent it, functions
need to be atomically consistent (aka linearisable), meaning they must have a
linearisation point at which they appear to occur instantly. Such functions can
be written with different techniques, including:

- use of [`Atomic`](https://v2.ocaml.org/api/Atomic.html) for mutable variables,
- use of a mutual exclusion mechanism like
  [`Mutex`](https://v2.ocaml.org/api/Mutex.html).

However both solutions have their limits. Using mutexes or locks open the way to
deadlock, livelock, priority inversion, etc; it also often restrics considerably
the performance gained by using multiple cores as the parts of the code
effectively running in parallel is limited. On the other hand, atomics are -
without a complex algorithm to combine them - only a solution for a single
shared variable.

Let's try to replace references by atomics in our code to demonstrate this
point:

```ocaml
type queue = int list Atomic.t

let create () : queue = Atomic.make []

let push (q : queue) a =
  let curr = Atomic.get q in
  let prev = a :: curr in
  Atomic.set q prev
```

We still need to read and write to fulfill the whole push operation.

```ocaml
# test ();;
- : int * int list = (15, [1; 1; 1; 1; 1; 2; 1; 1; 1; 2; 2; 2; 2; 1; 2])
```

and, as expected it is not working. The interleaving scenario described
previously can still happen, meaning our function is not linearisable (or
atomically consistent). Note that, though it is not observable here, this is
still better than the previous implementation, as we are now race-free (see
[here](#a-note-about-races-in-ocaml) for a quick note about races in OCaml). As
a matter of fact, writting a queue with `push` and `pop` functions that are both
atomic and parallelism-safe is not as easy as it might sound and often requires
advanced techniques to perform well. This is the type of algorithms
Saturn_lockfree provided.

To continue with our example, here is how it will be written using the queue
provided in Saturn.

```ocaml
let work_saturn id barrier q () =
  Barrier.await barrier;
  for i = 0 to 9 do
    Saturn.Queue.push q id
  done

let test_saturn () =
  let barrier = Barrier.create 2 in
  let q = Saturn.Queue.create () in
  let d1 = Domain.spawn (work_saturn 1 barrier q) in
  let d2 = Domain.spawn (work_saturn 2 barrier q) in
  Domain.join d1;
  Domain.join d2;
  let rec pop_all acc =
    match Saturn.Queue.pop q with
    | None -> List.rev acc
    | Some elt -> pop_all (elt :: acc)
  in
  let res = pop_all [] in
  (List.length res, res)
```

Running it results in the expected result:

```ocaml
# test_saturn ();;
- : int * int list =
(20, [2; 2; 1; 2; 2; 2; 2; 2; 2; 1; 2; 1; 2; 1; 1; 1; 1; 1; 1; 1])
```

### A note about races in OCaml

Because of the great properties of OCaml 5 memory model (see the
[OCaml Manual](https://v2.ocaml.org/manual/parallelism.html#s%3Apar_mm_easy) for
more details), not a lot can go wrong here. At least, data corruption or
segmentation fault won't happen like it can in other languages.

# Usage

This part describes how to use the provided data structures, and more exactly,
what not to do with them. Two main points are discussed:

- some data structures have restrictions on what operations can be performed in
  a single domain or a set of domains
- the currently provided data structures are non-composable

## Data Structures With Domain Roles

There are several provided data structures that are intended to be used in
combination with a specific domain configuration. These restrictions make the
corresponding implementation optimized but not respected them may break safety
properties. Obviously, these restrictions are not only described in the
documentation but also on the name of the data structure itself. For example, a
single consumer queue can only have a single domain popping at any given time.

Let's take the example of `Single_prod_single_cons_queue`. As suggested by the
name, it should be used with only one domain performing `push` (a producer) and
one domain performing `pop` (a consumer) at the same time. Having two or more
domains simultaneously perform `pop` (or `push`) will break the safety
properties of the `queue` and more likely result in unexpected behaviors.

Let's say we give a bad alias to this queue and misuse it.

```ocaml
module Queue = Saturn.Single_prod_single_cons_queue
```

Each domain is going to try to push 10 times in parallel.

```ocaml
let work id barrier q =
  Barrier.await barrier;
  for i = 0 to 9 do
    Queue.push q id
  done
```

Our `test` function returns the queue after two domains try to simustaneously
push.

```ocaml
let test () =
  let q = Queue.create ~size_exponent:5 in
  let barrier = Barrier.create 2 in
  let d1 = Domain.spawn (fun () -> work 1 barrier q) in
  let d2 = Domain.spawn (fun () -> work 2 barrier q) in
  Domain.join d1;
  Domain.join d2;
  q
```

We can then inspect the content of the queue by popping it into a list.

```ocaml
let get_content q =
  let rec loop acc =
    match Queue.pop q with
    | None -> acc
    | Some a -> loop (a :: acc)
  in
  loop [] |> List.rev
```

Let's run it :

```ocaml
test () |> get_content;;
- : int list = [2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 2]
```

This run results in a queue with only 11 elements. 9 elements are lost because
of the misuse of the single consumer single producer queue.

## About Composability

Composability is the ability to compose functions while preserving their
properties. For Saturn data structures, the properties one could expect to
preserve are atomic consistency (or linearizability) and all eventual progress
properties, like lock freedom. Unfortunately, Saturn's data structures are not
composable.

Let's illustrate that with an example. We want to write a slitting algorithm on
Saturn's queue: several domains simultaneously slit a source queue into two
destination queues in respect to a predicate. We expect our splitting function
to be linearisable, which would manifest here by the source queue order is
preserved in the destination queues. For example, `slit [0;1;2;3;4]`, with a
predicate that returns `true` for even numbers and `false` otherwise, should
returns `[0';2;4]` and `[1;3]`.

Here is how we can write `slit` with the functions provided by Saturn's queue.

```ocaml
let slit source pred true_dest false_dest : bool =
  match Queue.pop source with
  | None -> false
  | Some elt ->
      if pred elt then Queue.push true_dest elt else Queue.push false_dest elt;
      true
```

Domains run `split` until the source queue is empty:

```ocaml
let work source pred true_dest false_dest =
  while split source pred true_dest false_dest do
    ()
  done
```

Now to test it, we will run:

```ocaml
let test input =
  (* Initialisation *)
  let true_dest = Queue.create () in
  let false_dest = Queue.create () in
  let source = Queue.create () in
  List.iter (Queue.push source) input;

  let barrier = Barrier.create 2 in

  (* Predicate : split by parity *)
  let pred elt = elt mod 2 = 0 in

  let d1 =
    Domain.spawn (fun () ->
        Barrier.await barrier;
        work source pred true_dest false_dest)
  in
  let d2 =
    Domain.spawn (fun () ->
        Barrier.await barrier;
        work source pred true_dest false_dest)
  in
  Domain.join d1;
  Domain.join d2;
  (get_content true_dest, get_content false_dest)
```

The expected result for `test [0; 1; 2; 3; 4]` is `([0; 2; 4], [1; 3])`. And if
you try it, you will most probably get that result. Except it can also return in
unsorted queues.

As the chance of getting an unsorted queue, we write a `check` function that
runs `test` multiple times and counts the number of times the result is not what
we wanted.

```ocaml
let check inputs max_round =
  let expected_even = List.filter (fun elt -> elt mod 2 = 0) inputs in
  let expected_odd = List.filter (fun elt -> elt mod 2 = 1) inputs in
  let rec loop round bugged =
    let even, odd = test inputs in
    if round >= max_round then bugged
    else if even <> expected_even || odd <> expected_odd then
      loop (round + 1) (bugged + 1)
    else loop (round + 1) bugged
  in
  Format.printf "%d/%d rounds are bugged.@." (loop 0 0) max_round
```

and try it:

```ocaml
# check [0;1;2;3;4;5;6] 1000;;
35/1000 rounds are bugged.
```

As expected, it is not working, and the reason is simply because our `split`
function is not linerisable. We could make it atomic by using mutex, but then we
loose the progress properties of the composed functions.

#### Extending Data Structures

Note that in the case above, we transfer from and to a queue of the same
`int Saturn.Queue.t` type. It is most likely possible to write a
`val transfer : t -> t -> unit` function with the right properties and add it
directly to `Saturn.Queue` module.

If you think of any such functions, that is useful and missing, let's us know by
creating an issue!

#### Composable Parallelism-Safe Data Structures

If you need composable parallelism-safe data structures, you can check
[kcas_data](https://github.com/ocaml-multicore/kcas#programming-with-transactional-data-structures).

# Testing

One of the many difficulties of implementating parallelism-safe data structures
is that in addition to providing the same safety properties as sequental ones,
they may also have to observe some
[liveness properties](https://en.wikipedia.org/wiki/Safety_and_liveness_properties)
as well as additional safety properties specific to concurrent programming, like
deadlock-freedom.

In addition to the expected safety properties, the main properties we want to
test for are:

- linearisability
- lock-freedom for all the lock-free data structures
- no potentially harmful data races

Here is a list of the tools we use to ensure them:

- _safety_ : unitary tests and `qcheck` tests check semantics and expected
  behaviors with one and more domains.
- _safety and liveness_ : `STM` tests check _linearisability_ for two domains
  (see
  [`multicoretests` library](https://github.com/ocaml-multicore/multicoretests)).
- _liveness_ : `dscheck` checks _non-blocking_ property for as many domains as
  wanted (for two domains most of the time). See
  [dscheck](https://github.com/ocaml-multicore/dscheck).
- _safety_ : no data race with
  [tsan](https://github.com/ocaml-multicore/ocaml-tsan)

See [test/README.md](test/README.md) for more details.

# Benchmarks

There are a number of benchmarks in `bench` directory. You can run them with
`make bench`. See [bench/README.md](bench/README.md) for more details.

## Contributing

Contributions are appreciated! If you intend to add a new data structure, please
read [this](CONTRIBUTING.md) before.
