# `lockfree` — Lock-free data structures for Multicore OCaml
--------------------------------------------------------

A collection of Concurrent Lockfree Data Structures for OCaml 5. It contains:

* [Chase-Lev Work-Stealing Deque](src/ws_deque.mli) Single-producer, multi-consumer dynamic-size double-ended queue (deque) (see [Dynamic circular work-stealing deque](https://dl.acm.org/doi/10.1145/1073970.1073974) and [Correct and efficient work-stealing for weak memory models](https://dl.acm.org/doi/abs/10.1145/2442516.2442524)). Ideal for throughput-focused scheduling using per-core work distribution. Note, [pop] and [steal] follow different ordering (respectively LIFO and FIFO) and have different linearization contraints.

* [SPSC Queue](src/spsc_queue.mli) Simple single-producer single-consumer fixed-size queue. Thread-safe as long as at most one thread acts as producer and at most one as consumer at any single point in time.

* [MPMC Queue](src/mpmc_queue.mli) Multi-producer, multi-consumer fixed-size queue. Optimised for high number of threads.

## Usage

lockfree cam be installed from `opam`: `opam install lockfree`. Sample usage of
`Ws_deque` is illustrated below.

```ocaml
module Ws_deque = Ws_deque.M

let q = Ws_deque.create ()

let () = Ws_deque.push q 100

let () = assert (Ws_deque.pop q = 100)
```

## Contributing

Contributions of more lockfree data structures appreciated! Please create
issues/PRs to this repo.
