# Benchmarks for Saturn

Benchmarks are written using [multicore-bench](https://github.com/ocaml-multicore/multicore-bench).

## General Usage

To execute benchmarks, you can run:
```shell
make bench
```

Alternatively, you can use:
```shell
dune exec -- ./bench/main.exe
```

It is recommended to run the benchmarks with a budget of at least `1` second (as done with `make bench`):
```shell
dune exec -- ./bench/main.exe -budget 1
```

You can also print a brief version of the benchmarks with the `-brief` option. Additionally, it is possible to run only selected benchmarks by providing a part of the benchmark names. You can get the list of available benchmarks with the `--help` option.

For example, running:
```shell
dune exec -- ./bench/main.exe --help
```
returns:

```
Usage: main.exe <option>* filter*

The filters are regular expressions for selecting benchmarks to run.

Benchmarks:

  Saturn Queue
  Saturn Queue_unsafe
  Saturn Bounded_Queue
  Saturn Bounded_Queue_unsafe
  Saturn Single_prod_single_cons_queue
  Saturn Size
  Saturn Skiplist
  Saturn Htbl
  Saturn Htbl_unsafe
  Saturn Stack
  Saturn Work_stealing_deque
  Saturn Bounded_Stack

Options:

  -budget seconds   Budget for a benchmark
  -debug            Print progress information to help debugging
  -diff path.json   Show diff against specified base results
  -brief            Show brief human-readable results
  -help             Show this help message
  --help            Show this help message
```

For example, if you want to run only the `Htbl` benchmarks to compare the performance of `Htbl` and its unsafe version `Htbl_unsafe`, you can run:
```shell
dune exec -- ./bench/main.exe -budget 1 -brief Htbl
```

## Current-bench

The output is in JSON format, as it is intended to be consumed by [current-bench](https://bench.ci.dev/ocaml-multicore/saturn/branch/main/benchmark/default).