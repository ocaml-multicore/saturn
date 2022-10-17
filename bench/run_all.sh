#!/bin/bash
set -x

date=$(date)

echo "$date" > benchmarks_output.txt

# first bench
with_cas=("" "-use-cas")
rw_balance=("-takers 4 -pushers 4" "-takers 1 pushers 8" "-takers 8 pushers 1")
path="./../_build/default/bench/mpmc_queue.exe "

for i in "${with_cas[@]}" 
do
  for j in "${rw_balance[@]}" 
  do
	  cmd="$path $j $i"
    output=$($cmd)

    echo "$cmd" >> benchmarks_output.txt
    echo "$output" >> benchmarks_output.txt
  done 
done


# second bench
rw_balance=("-stealers 0" "-stealers 2" "-stealers 4")
path="./../_build/default/bench/spmc_queue.exe "

for j in "${rw_balance[@]}" 
do
  cmd="$path $j "
  output=$($cmd)

  echo "$cmd" >> benchmarks_output.txt
  echo "$output" >> benchmarks_output.txt
done