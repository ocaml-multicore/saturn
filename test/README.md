## About dscheck tests

Dscheck tests need to use dscheck `TracedAtomic` library, which adds effects to `Stdlib.Atomic` to track calls to atomic functions. To make it work :
- the `Atomic` standard library is shadowed by [`Dscheck.TracedAtomic`](atomic/atomic.ml)
- every datastructure implementation is copied in its respective tests directory. The copied version is used only for dscheck tests and is compiled with the `shadowing` atomic library

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
Dscheck test rule does not require `lockfree` but require `atomic` which is the shadowed atomic library.
