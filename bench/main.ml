let benchmarks =
  [
    ("Saturn Queue", Bench_queue.Safe.run_suite);
    ("Saturn Queue_unsafe", Bench_queue.Unsafe.run_suite);
    ("Saturn Cue", Bench_cue.Safe.run_suite);
    ("Saturn Cue_unsafe", Bench_cue.Unsafe.run_suite);
    ("Saturn Single_prod_single_cons_queue", Bench_spsc_queue.run_suite);
    ("Saturn Size", Bench_size.run_suite);
    ("Saturn Skiplist", Bench_skiplist.run_suite);
    ("Saturn Htbl", Bench_htbl.Safe.run_suite);
    ("Saturn Htbl_unsafe", Bench_htbl.Unsafe.run_suite);
    ("Saturn Stack", Bench_stack.run_suite);
    ("Saturn Work_stealing_deque", Bench_ws_deque.run_suite);
    ("Saturn Bounded_Stack", Bench_bounded_stack.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
