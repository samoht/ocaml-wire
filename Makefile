.PHONY: build test bench bench-everparse bench-tcpip bench-sample memtrace clean

build:
	dune build

test:
	dune runtest

bench:
	dune exec bench/timing.exe

bench-everparse:
	BUILD_EVERPARSE=1 dune exec bench/perf.exe

bench-tcpip:
	dune exec bench/tcpip/example.exe

bench-tcpip-everparse:
	BUILD_EVERPARSE=1 dune exec bench/tcpip/bench.exe

bench-sample:
	BUILD_EVERPARSE=1 dune build bench/perf.exe
	@echo "Starting bench (50M iters), sampling for 5s..."
	@_build/default/bench/perf.exe 50000000 & \
	  PID=$$!; \
	  sleep 1; \
	  sudo sample $$PID 5 -file /tmp/bench_sample.txt; \
	  wait $$PID; \
	  echo "Sample saved to /tmp/bench_sample.txt"

memtrace:
	MEMTRACE=trace.ctf dune exec bench/memtrace.exe
	memtrace_hotspots trace.ctf

clean:
	dune clean
