.PHONY: all clean test bench

all:
	dune build

test:
	dune runtest

clean:
	dune clean

bench:
	@dune exec --release -- ./bench/main.exe -budget 1
