let benchmarks =
  [
    ("Saturn Relaxed_queue", Bench_relaxed_queue.run_suite);
    ("Saturn_lockfree Queue", Bench_queue.run_suite);
    ("Saturn_lockfree Lazy_queue", Bench_lazy_queue.run_suite);
    ("Saturn_lockfree Single_prod_single_cons_queue", Bench_spsc_queue.run_suite);
    ("Saturn_lockfree Size", Bench_size.run_suite);
    ("Saturn_lockfree Skiplist", Bench_skiplist.run_suite);
    ("Saturn_lockfree Stack", Bench_stack.run_suite);
    ("Saturn_lockfree Work_stealing_deque", Bench_ws_deque.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
