Benchmarks for lockfree

# General usage 

Execute `make bench` from root of the repository to run the standard set of benchmarks. The output is in JSON, as it is intended to be consumed by ocaml-benchmark CI (in progress). 

# Specific structures 

Some benchmarks expose commandline interface targeting particular structures:

* [mpmc_queue.exe](mpmc_queue_cmd.ml)