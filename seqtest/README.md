## A sequential test of the work-stealing queue

This test exercises the work-stealing queue in `Ws_deque`
in a purely sequential mode.

To compile this test, type `make` or `dune build --profile seqtest`.

To run this test, type `make random` or `dune exec --profile seqtest ./seqtest.exe`.
The test runs until it is interrupted.

This test requires the `monolith` package. Because we do not wish
to create a hard dependency on `monolith`, the code in this directory
is built by `dune` only when `--profile seqtest` is passed on the command line.
