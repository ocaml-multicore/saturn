let benchmarks =
  [
    ("Saturn Queue", Bench_queue.run_suite);
    ("Saturn Single_prod_single_cons_queue", Bench_spsc_queue.run_suite);
    ("Saturn Size", Bench_size.run_suite);
    ("Saturn Skiplist", Bench_skiplist.run_suite);
    ("Saturn Htbl", Bench_htbl.run_suite);
    ("Saturn Stack", Bench_stack.run_suite);
    ("Saturn Work_stealing_deque", Bench_ws_deque.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
