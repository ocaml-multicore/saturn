# `lockfree` — Lock-free data structures for Multicore OCaml
--------------------------------------------------------
## What is in this library ?
This library is a collection of Concurrent Lockfree Data Structures for OCaml 5. It contains:

| Name | What is it ? | Sources |
|-------|---------|----------|
| [Treiber Stack](src/treiber_stack.mli) | A classic multi-producer multi-consumer stack, robust and flexible. Recommended starting point when needing LIFO structure | |
| [Michael-Scott Queue](src/michael_scott_queue.mli) | A classic multi-producer multi-consumer queue, robust and flexible. Recommended starting point when needing FIFO structure. | [Simple, Fast, and Practical Non-Blocking and Blocking Concurrent Queue Algorithms](https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf)|
| [Chase-Lev Work-Stealing Deque](src/ws_deque.mli) | Single-producer, multi-consumer dynamic-size double-ended queue (deque). Ideal for throughput-focused scheduling using per-core work distribution. Note, `pop` and `steal` follow different ordering (respectively LIFO and FIFO) and have different linearization contraints. | [Dynamic circular work-stealing deque](https://dl.acm.org/doi/10.1145/1073970.1073974) and [Correct and efficient work-stealing for weak memory models](https://dl.acm.org/doi/abs/10.1145/2442516.2442524)) |
| [SPSC Queue](src/spsc_queue.mli) | Simple single-producer single-consumer fixed-size queue. Thread-safe as long as at most one thread acts as producer and at most one as consumer at any single point in time. | |
| [MPMC Relaxed Queue](src/mpmc_relaxed_queue.mli) | Multi-producer, multi-consumer, fixed-size relaxed queue. Optimised for high number of threads. Not strictly FIFO. Note, it exposes two interfaces: a lockfree and a non-lockfree (albeit more practical) one. See the mli for details. | |
| [MPSC Queue](src/mpsc_queue.mli) | A multi-producer, single-consumer, thread-safe queue without support for cancellation. This makes a good data structure for a scheduler's run queue. It is used in [Eio](https://github.com/ocaml-multicore/eio). | It is a single consumer version of the queue described in [Implementing lock-free queues](https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf). |

## Lockfree ?
### Definition
[Wikipedia definition](https://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedom) is pretty clear. Briefly (and a bit inaccurately), it says that lock-freedom means:
- obstruction freedom : any domain running in *isolation** for long enough can finish on its own
- guarantees overall progress : at least one domain is doing progress and will finish its call in a finite number of steps.

**In isolation* : a domain is said to run *in isolation* when it is the only domain to take steps. The other domains do nothing during that time even if they are in the middle of a method call. It is used to described properties, but not an actual domain behaviour.

### How does it works ?
The basic idea is that all operations on shared data use `atomics` variables.

### Why use lock free data structures ?
#### In brief
| Pros |  Cons |
|------|-------|
| thread safe | **not** starvation free |
| non blocking and dead-lock safe |  not easily composable (see [reagents](https://github.com/ocaml-multicore/reagents)) |
| usually good performances as it runs in parallel most often than blocking data structures | still subject to contention issues |

#### What does that means ?
If you need a *non-blocking* data structure, you are at the right place. However, if you only need a *thread safe* one you may not find what you need here. Especially, lock free data structures does not necessary have better performances than blocking ones. However,  it ensures that is a thread is delayed, that won't delay the other ones (this is the obstruction freedom property)

Also lock freedom is a restraining property, which means it is not easy (and it may not even be possible), to modify or add a function to a lock free data structure while maintaining this property. That explains why most of time the provided data structure are a very simple API.

### How can we ensure the provided data structures are lock free  ?
We don't provide on-paper lock freedom  proofs for each data structure provided here but, as much as possible, they are an OCaml implementation of algorithms from reviewed papers that provide such a proof. We also try to implement well know and well tested lock free data structures from other languages libraries.

Also (and obviously), data structures are carefully reviewed but above all, they are extensively tested. We provide multiple kind of tests for every data structures. In particular, `dscheck` tests provide some guaranties of lock freedom (see [Tests](#Tests)).

## Usage

lockfree can be installed from `opam`: `opam install lockfree`. Sample usage of
`Spsc` is illustrated below.

```ocaml
module Spsc_queue = Lockfree.Spsc_queue

let main () =
	let q = Spsc_queue.create ~size_exponent:8 in
	let producer =
	 	Domain.spawn (fun () ->
			Spsc_queue.push q 1;
			Spsc_queue.push q 2) in
	let consumer =
		Domain.spawn (fun () ->
			Spsc_queue.pop q,
			Spsc_queue.pop q) in
	let _ = Domain.join producer in
	let v1, v2 = Domain.join consumer in
	assert (v1, v2 = None, None ||
	        v1, v2 = Some 1, None ||
	        v1, v2 = Some 2, Some 1)
```

## Tests
Several kind of tests are provided for each data structure:
- unitary tests and `qcheck` tests: check semantics and expected behaviors with one and more domains;
- `STM` tests: check _linearizability_ for two domains (see [multicoretests library](https://github.com/ocaml-multicore/multicoretests));
- `dscheck`: checks _non-blocking_ property for as many domains as wanted (for two domains most of the time). See [dscheck](https://github.com/ocaml-multicore/dscheck).

See [test/README.md](test/README.md) for more details.
## Benchmarks
There is a number of benchmarks in `bench` directory. You can run them with `make bench`. See [bench/README.md](bench/README.md) for more details.

## Contributing

Contributions of more lockfree data structures are appreciated! Please read [this](CONTRIBUTING.md) before.
