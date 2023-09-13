### Introduction

Several kind of tests are provided for each data structure:

- unitary tests and `qcheck` tests: check semantics and expected behaviors with
  one and more domains;
- `STM` tests: also check semantics as well as _linearizability_ for two domains
  (see
  [multicoretests library](https://github.com/ocaml-multicore/multicoretests));
- `dscheck` (for lockfree data structures): checks lock-freedom for as many
  domains as wanted (for two domains most of the time). It is limited to the
  paths explored by the tests.

### Unitary parallel tests and `QCheck` tests

Every data structure should have separate parallel tests for all core operations
and for most useful combinations of them to make sure things work the way you
expected them to, even in parallel.

Inside the parallel tests, it's important to ensure there's a high chance of
actual parallel execution. Keep in mind that spawning a domain is an expensive
operation: if a domain only launches a few simple operations, it will most
likely run in isolation. Here are a few tricks to ensure your tests actually
runs with parallelism:

- make a domain repeat its operations. In particular, that usually works well
  when testing a single function.
- use the provided [barrier](barrier/barrier.mli) module to make all domains
  wait for each other before starting (see
  [these tests for example) ](ws_deque/qcheck_ws_deque.ml)).
- if you are still not sure your tests have a good chance to run in parallel,
  try it in the top level, and use print or outputs to understand what is
  happening.

### Linearizability test with `STM` (see [multicoretests](https://github.com/ocaml-multicore/multicoretests))

#### How to write `STM` tests ?

`STM` tests work by comparing the results of two domains each executing a random
list of method calls to a sequential model provided by the user. Most of the
time, these tests are easy and quick to write and can catch a lot of bugs.

If all domains can use every functions of the tested data structure, you can
have a look to
[stm test for Treiber's stack](treiber_stack/stm_treiber_stack.ml). If domains
have specific roles (producer/consumer/stealer etc..),
[this one](ws_deque/stm_ws_deque.ml) is for a work-stealing deque and is a
better example.

### Lock-free property with [`dscheck`](https://github.com/ocaml-multicore/dscheck).

`dscheck` is a model checker. Each provided test is run multiple times, each
time exploring a different interleaving between atomic operations. This checks
that there is no `blocking` paths in the ones explored by these tests (if a
trace is blocking, the test does not end)

#### How is `Atomic` library shadowed ?

Dscheck tests need to use dscheck `TracedAtomic` library, which adds effects to
`Stdlib.Atomic` to track calls to atomic functions. To make it work, every
datastructure implementation is copied in its respective tests directory and
compiled using the `dscheck` [atomic library](atomic/atomic.ml).

For example, in [ws_deque/dune](ws_deque/dune) :

```
; Copy implementation file and its dependencies
(rule
 (copy ../../src/ArrayExtra.ml ArrayExtra.ml))

(rule
 (copy ../../src/ws_deque.ml ws_deque.ml))

 ; rule to build dscheck tests
(test
 (name ws_deque_dscheck)
 (libraries atomic dscheck alcotest)
 (modules ArrayExtra ws_deque ws_deque_dscheck))
```

We can see that `dscheck` test compilation does not depend on `lockfree` (since
the data structure code is copy-paste in the test directory) but require
`atomic` which is the shadowing atomic library.

#### What about other progress properties ?

Right now, `dscheck` only checks for lock-freedom. In a near future, it should
also be able to deal with lock and checks for deadlock-freedom.
