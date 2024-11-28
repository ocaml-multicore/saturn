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

This repository is a collection of concurrent-safe data structures for OCaml 5. It aims to provide an industrial-strength, well-tested (and possibly model-checked and verified in the future), well documented, and maintained concurrent-safe data structure library. We want to make it easier for Multicore OCaml users to find the right data structures for their uses.

You can learn more about the **motivation** behind `Saturn` through the implemnetation of a lock-free stack [here](doc/motivation.md). 

**Saturn** is published on [opam](https://opam.ocaml.org/packages/saturn/) and is distributed under the [ISC license](https://github.com/ocaml-multicore/saturn/blob/main/LICENSE.md).

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml-multicore%2Fsaturn%2Fmain&logo=ocaml&style=flat-square)](https://ci.ocamllabs.io/github/ocaml-multicore/saturn)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/ocaml-multicore/saturn?style=flat-square&color=09aa89)](https://github.com/ocaml-multicore/saturn/releases/latest)
[![docs](https://img.shields.io/badge/doc-online-blue.svg?style=flat-square)](https://ocaml-multicore.github.io/saturn/)


# Contents

- [Saturn — Parallelism-Safe Data Structures for Multicore OCaml](#saturn--parallelism-safe-data-structures-for-multicore-ocaml)
- [Contents](#contents)
- [Installation](#installation)
  - [Getting OCaml 5.0](#getting-ocaml-50)
  - [Getting Saturn](#getting-saturn)
- [Provided data structures](#provided-data-structures)
    - [Treiber Lock-free Stack](#treiber-lock-free-stack)
    - [Lock-free Bounded Stack](#lock-free-bounded-stack)
    - [Michael-Scott Lock-free Queue](#michael-scott-lock-free-queue)
    - [Lock-free Bounded Queue](#lock-free-bounded-queue)
    - [Lock-free Chase-Lev Work-Stealing Dequeue](#lock-free-chase-lev-work-stealing-dequeue)
    - [Lock-free Single Producer Single Consumer Queue](#lock-free-single-producer-single-consumer-queue)
    - [Lock-free Multiple Producers Single Consumer Queue](#lock-free-multiple-producers-single-consumer-queue)
    - [Lock-free Skip List](#lock-free-skip-list)
    - [Lock-free Hash Table](#lock-free-hash-table)
- [About the Unsafe Data Structures](#about-the-unsafe-data-structures)
- [Usage](#usage)
    - [Data Structures with Domain Roles](#data-structures-with-domain-roles)
    - [Composability](#composability)
- [Testing](#testing)
- [Benchmarks](#benchmarks)
- [Contributing](#contributing)

# Installation

## Getting OCaml 5.0

You’ll need OCaml 5.0.0 or later. Note that Saturn also works with OCaml 4.14, but only for compatibility reasons, as parallelism-safe data structures are not needed without OCaml 5.0. We also recommend using OCaml 5.2 or later, as some bugs in the `Atomic` module have been fixed.

To install OCaml 5.0 yourself, first make sure you have opam 2.1 or later. You
can run this command to check:

```sh
opam --version
```

Then use opam to install OCaml 5.2.0:

```sh
opam switch create 5.2.0
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

# Provided data structures

### Treiber Lock-free Stack
- **Module**: [Stack](https://ocaml-multicore.github.io/saturn/saturn/Stack/index.html)
- **Description**: A classic multi-producer, multi-consumer, lock-free stack, known for robustness and flexibility.
- **Recommendation**: It's a recommended starting point when a LIFO structure is needed.

### Lock-free Bounded Stack

- **Module**: [Bounded_stack](https://ocaml-multicore.github.io/saturn/saturn/Bounded_stack/index.html)
- **Description**: A stack based on the Treiber stack algorithm, with a limited capacity and a `length` function.
- **Recommendation**: Adding a capacity introduces a general overhead to the operations. It is recommended to use the unbounded stack if neither the capacity nor the `length` function is needed.

### Michael-Scott Lock-free Queue

- **Module**: [Queue](https://ocaml-multicore.github.io/saturn/saturn/Queue/index.html)
- **Description**: A multi-producer, multi-consumer lock-free queue that is both robust and flexible.
- **Recommendation**: This structure is ideal when a FIFO setup is required.
- **Sources**: [Simple, Fast, and Practical Non-Blocking and Blocking Concurrent Queue Algorithms](https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf)

### Lock-free Bounded Queue

- **Module**: [Bounded_queue](https://ocaml-multicore.github.io/saturn/saturn/Bounded_queue/index.html)
- **Description**: A queue based on the Michael-Scott queue algorithm, with a limited capacity and a `length` function.
- **Recommendation**: Adding a capacity introduces a general overhead to the operations. It is recommended to use the unbounded queue if neither the capacity nor the `length` function is needed.

### Lock-free Chase-Lev Work-Stealing Dequeue

- **Module**: [Work_stealing_deque](https://ocaml-multicore.github.io/saturn/saturn/Work_stealing_deque/index.html)
- **Description**: Single-producer, multi-consumer dynamic-size deque (double-ended queue).
- **Recommendation**: Designed for high-throughput scheduling using per-core work distribution. Note that `pop` and `steal` operations follow different ordering (LIFO and FIFO) with distinct linearization constraints. It is a role-oriented data structure: most functions can't be used by all domains.
- **Sources**:
  - [Dynamic Circular Work-Stealing Deque](https://dl.acm.org/doi/10.1145/1073970.1073974)
  - [Correct and Efficient Work-Stealing for Weak Memory Models](https://dl.acm.org/doi/abs/10.1145/2442516.2442524)

### Lock-free Single Producer Single Consumer Queue

- **Module**: [Single_prod_single_cons_queue](https://ocaml-multicore.github.io/saturn/saturn/Single_prod_single_cons_queue/index.html)
- **Description**: A single-producer, single-consumer fixed-size queue. This specific configuration enables strong optimizations but also makes the data structure unsafe if used improperly, i.e., with more than one producer or one consumer at any time.
- **Recommendation**: It's concurrent-safe as long as only one thread acts as producer and one as consumer at any time.

### Lock-free Multiple Producers Single Consumer Queue

- **Module**: [Single_consumer_queue](https://ocaml-multicore.github.io/saturn/saturn/Single_consumer_queue/index.html)
- **Description**: A multi-producer, single-consumer concurrent-safe queue with a closing mechanism to prevent further pushes.
- **Recommendation**: Designed for scheduler run queues. It is not concurrent-safe if used by multiple consumers simultaneously.

### Lock-free Skip List

- **Module**: [Skiplist](https://ocaml-multicore.github.io/saturn/saturn/Skiplist/index.html)
- **Description**: A skiplist is a probabilistic data structure that has an average logarithmic complexity for search and insertion operations.
- **Recommendation**: The skiplist is not resizable. It will, however, continue to work once the limit capacity is reached, but performance will decrease as the depth of the structure won't be enough to maintain logarithmic performance.
- **Sources**: See Chapter 14 in [The Art of Multiprocessor Programming](https://www.researchgate.net/profile/Maurice-Herlihy/publication/213876653_The_Art_of_Multiprocessor_Programming/links/0deec516186548c25c000000/The-Art-of-Multiprocessor-Programming.pdf)

### Lock-free Hash Table

- **Module**: [Hstbl](https://ocaml-multicore.github.io/saturn/saturn/htbl/index.html)
- **Description**: A resizable lock-free hash table with a snapshot mechanism.
- **Recommendation**: Contains useful high-level operations designed to work as building blocks of non-blocking algorithms.


# About the Unsafe Data Structures

Some data structures are available in two versions: a normal version and a more optimized but **unsafe** version. The **unsafe** version utilizes `Obj.magic` in a way that may be unsafe with `flambda2` optimizations.

The reason for providing the unsafe version is that certain optimizations require features that are currently not available in OCaml, such as arrays of atomics or atomic fields in records. We recommend using the normal version of a data structure unless its performance is not sufficient for your use case. In that case, you can try the unsafe version.

Currently, the following data structures have an unsafe version:

- `Single_cons_single_prod_unsafe`: a single consumer single producer lock-free queue
- `Queue_unsafe`: a Michael-Scott lock-free queue
- `Bounded_queue_unsafe`: a lock-free bounded queue based on Michael-Scott queue algorithm 
- `Htbl_unsafe` : a lock-free hashtable

# Usage

This part describes how to use the provided data structures, and more exactly,
what not to do with them. Two main points are discussed:

- some data structures have restrictions on what operations can be performed in a single domain or a set of domains
- the currently provided data structures are non-composable

### Data Structures with Domain Roles
Some provided data structures are designed to work with specific domain configurations. These restrictions optimize their implementation, but failing to respect them may compromise safety properties. These limitations are clearly indicated in the documentation and often reflected in the name of the data structure itself. For instance, a single-consumer queue must have only one domain performing `pop` operations at any given time.

To learn more about it, see this [document](doc/domain-role.md).

### Composability 

Composability refers to the ability to combine functions while preserving their properties. For Saturn data structures, the expected properties include atomic consistency (or linearizability) and progress guarantees, such as lock-freedom. Unfortunately, Saturn's data structures are not composable.

To learn more about it, see this [document](doc/composability.md).

# Testing

One of the many difficulties of implementating parallelism-safe data structures is that in addition to providing the same safety properties as sequental ones, they may also have to observe some [liveness properties](https://en.wikipedia.org/wiki/Safety_and_liveness_properties) as well as additional safety properties specific to concurrent programming, like deadlock-freedom.

In addition to the expected safety properties, the main properties we want to test for are:
- linearisability
- lock-freedom for all the lock-free data structures
- no potentially harmful data races

Here is a list of the tools we use to ensure them:
- _safety_ : unitary tests and `qcheck` tests check semantics and expected behaviors with one and more domains.
- _safety and liveness_ : `STM` tests check _linearisability_ for two domains (see [`multicoretests` library](https://github.com/ocaml-multicore/multicoretests)).
- _liveness_ : `dscheck` checks _non-blocking_ property for as many domains as wanted (for two domains most of the time). See [dscheck](https://github.com/ocaml-multicore/dscheck).
- _safety_ : no data race with [tsan](https://github.com/ocaml-multicore/ocaml-tsan)

See [test/README.md](test/README.md) for more details.

# Benchmarks

There are a number of benchmarks in `bench` directory. You can run them with
`make bench`. See [bench/README.md](bench/README.md) for more details.

# Contributing

Contributions are appreciated! If you intend to add a new data structure, please
read [this](CONTRIBUTING.md) before.
