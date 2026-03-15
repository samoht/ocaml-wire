.PHONY: build test bench memtrace clean

build:
	dune build

test:
	dune runtest

bench:
	BUILD_EVERPARSE=1 dune exec bench/bench_perf.exe

memtrace:
	MEMTRACE=trace.ctf dune exec bench/bench_wire_memtrace.exe
	memtrace_hotspots trace.ctf

clean:
	dune clean
