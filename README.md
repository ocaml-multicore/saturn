# `lockfree` — Lock-free data structures for Multicore OCaml
--------------------------------------------------------

A collection of Concurrent Lockfree Data Structures for OCaml 5. It contains:

* [Chase-Lev Work-Stealing Queue](src/ws_deque.mli) Single-producer, multi-consumer dynamic-size queue. Ideal for throughput-focused scheduling using per-core queue. 

* [SPMC Queue](src/spmc_queue.mli) Single-producer, multi-consumer dynamic-size queue. Ideal for latency-focused scheduling using per-core queue. 

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