let benchmarks =
  [
    ("Saturn Queue", Bench_queue.Safe.run_suite);
    ("Saturn Queue_unsafe", Bench_queue.Unsafe.run_suite);
    ("Saturn Bounded_Queue", Bench_bounded_queue.Safe.run_suite);
    ("Saturn Bounded_Queue_unsafe", Bench_bounded_queue.Unsafe.run_suite);
    ("Saturn Single_prod_single_cons_queue", Bench_spsc_queue.Safe.run_suite);
    ( "Saturn Single_prod_single_cons_queue_unsafe",
      Bench_spsc_queue.Unsafe.run_suite );
    ("Saturn Single_consumer_queue", Bench_mpsc.run_suite);
    ("Saturn Size", Bench_size.run_suite);
    ("Saturn Skiplist", Bench_skiplist.run_suite);
    ("Saturn Htbl", Bench_htbl.Safe.run_suite);
    ("Saturn Htbl_unsafe", Bench_htbl.Unsafe.run_suite);
    ("Saturn Stack", Bench_stack.run_suite);
    ("Saturn Work_stealing_deque", Bench_ws_deque.run_suite);
    ("Saturn Bounded_Stack", Bench_bounded_stack.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
