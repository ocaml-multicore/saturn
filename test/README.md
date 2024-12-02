### Introduction

Several kind of tests are provided for each data structure:

- unitary tests and `qcheck` tests: check semantics and expected behaviors with
  one and more domains;
- `STM` tests: also check semantics as well as _linearizability_ for two domains
  (see
  [multicoretests library](https://github.com/ocaml-multicore/multicoretests));
- `dscheck` (for lockfree data structures): checks semantics and lock-freedom for as many
  domains as wanted (for two domains most of the time). It is limited to the
  paths explored by the tests.

**Note: ** `Qcheck` tests are most of the time redondant with `STM`.

### Debugging Workflow with `DScheck` and `STM`

Using `DScheck` and `STM` together provides an effective workflow for debugging. 
Here is how to make the most of them:

1. **Start with `STM` tests**: Writing `STM` tests is quick and can catch most 
bugs. If `STM` detects a bug, it will return the test (a list of function calls) that 
is failing.

1. **Move to `DScheck` for parallel tests**: If the bug is related to parallel 
execution, write the test in `DScheck`. `DScheck` will provide an interleaving 
that is not working, helping you to precisely localize the issue.

By following this workflow, you can efficiently identify and fix bugs in your 
concurrent data structures.

### Unitary Parallel Tests and `QCheck` Tests

Each data structure includes separate parallel tests for all core operations and
the most useful combinations of them to ensure they function correctly, even in parallel scenarios.

To maximize the likelihood of actual parallel execution within these tests, 
consider the following tips:

- **Repeat Operations**: Have a domain repeat its operations multiple times. This approach is particularly effective when testing a single function.
- **Use Barriers**: Utilize the provided [barrier](barrier/barrier.mli) module to synchronize domains, ensuring they all wait for each other before starting. Refer to [these tests](ws_deque/qcheck_ws_deque.ml) for examples.
- **Verify Parallelism**: If you're uncertain whether your tests are running in parallel, try executing them in the top level and use print statements or other outputs to observe the behavior.

### Correctness and Linearizability test with `STM` (see [multicoretests](https://github.com/ocaml-multicore/multicoretests))

#### How to write `STM` tests ?

`STM` tests are designed to compare the results of two domains, each executing a random list of method calls, against a sequential model provided by the user. These tests are typically straightforward and quick to write, and they can identify many bugs effectively. 

When developing a new data structure, it is recommended to write these tests from the start.

If all domains can use every function of the tested data structure, refer to the [STM test for Treiber's stack](treiber_stack/stm_treiber_stack.ml). If the domains have specific roles (e.g., producer, consumer, stealer), the [STM test for a work-stealing deque](ws_deque/stm_ws_deque.ml) is a more suitable example.

### Correctness and Lock-freedom with [`dscheck`](https://github.com/ocaml-multicore/dscheck).

`dscheck` is a model checker designed to verify the correctness of concurrent programs. It executes each provided test multiple times, exploring different interleavings of atomic operations during each run. This thorough exploration helps in testing the specified properties of the program. Additionally, `dscheck` ensures that there are no blocking paths in the explored interleavings. If a trace is found to be non-lock-free, the test will not complete.

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

We can see that `dscheck` test compilation does not depend on `saturn` (since
the data structure code is copy-paste in the test directory) but requires
`atomic` which is the shadowing atomic library.

