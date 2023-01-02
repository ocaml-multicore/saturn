# `lockfree` — Lock-free data structures for Multicore OCaml
--------------------------------------------------------

A collection of Concurrent Lockfree Data Structures for OCaml 5. It contains:

* [Treiber Stack](src/treiber_stack.mli) A classic multi-producer multi-consumer stack, robust and flexible. Recommended starting point when needing LIFO structure. 
  
* [Michael-Scott Queue](src/michael_scott_queue.mli) A classic multi-producer multi-consumer queue, robust and flexible. Recommended starting point when needing FIFO structure. It is based on [Simple, Fast, and Practical Non-Blocking and Blocking Concurrent Queue Algorithms](https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf).

* [Chase-Lev Work-Stealing Deque](src/ws_deque.mli) Single-producer, multi-consumer dynamic-size double-ended queue (deque) (see [Dynamic circular work-stealing deque](https://dl.acm.org/doi/10.1145/1073970.1073974) and [Correct and efficient work-stealing for weak memory models](https://dl.acm.org/doi/abs/10.1145/2442516.2442524)). Ideal for throughput-focused scheduling using per-core work distribution. Note, [pop] and [steal] follow different ordering (respectively LIFO and FIFO) and have different linearization contraints.

* [SPSC Queue](src/spsc_queue.mli) Simple single-producer single-consumer fixed-size queue. Thread-safe as long as at most one thread acts as producer and at most one as consumer at any single point in time.

* [MPMC Relaxed Queue](src/mpmc_relaxed_queue.mli) Multi-producer, multi-consumer, fixed-size relaxed queue. Optimised for high number of threads. Not strictly FIFO. Note, it exposes two interfaces: a lockfree and a non-lockfree (albeit more practical) one. See the mli for details. 

* [MPSC Queue](src/mpsc_queue.mli) A multi-producer, single-consumer, thread-safe queue without support for cancellation. This makes a good data structure for a scheduler's run queue. It is used in [Eio](https://github.com/ocaml-multicore/eio). It is a single consumer version of the queue described in [Implementing lock-free queues](https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf).

* [CTrie](src/paradict.mli) A parallel dictionnary, to be used as a thread-safe, lockfree alternative to stdlib's Hashtbl or Map. See also [Wikipedia](https://en.wikipedia.org/wiki/Ctrie).

## Usage

lockfree can be installed from `opam`: `opam install lockfree`. Sample usage of
`Ws_deque` is illustrated below.

```ocaml
module Ws_deque = Ws_deque.M

let q = Ws_deque.create ()

let () = Ws_deque.push q 100

let () = assert (Ws_deque.pop q = 100)
```

## Benchmarks

There is a number of benchmarks in `bench/` directory. You can run them with `make bench`. See [bench/README.md](bench/README.md) for more details.

## Contributing

Contributions of more lockfree data structures appreciated! Please create
issues/PRs to this repo.
