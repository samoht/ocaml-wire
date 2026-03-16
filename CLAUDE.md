# CLAUDE.md

## Build & Test

```
make build              # dune build
make test               # dune runtest
make bench              # dune exec bench/timing.exe
make bench-everparse    # BUILD_EVERPARSE=1 dune exec bench/perf.exe
make bench-tcpip        # dune exec examples/tcpip/example.exe
make memtrace           # MEMTRACE=trace.ctf dune exec bench/memtrace.exe && memtrace_hotspots trace.ctf
make clean              # dune clean
```

`make bench-everparse` requires EverParse (`3d.exe` in PATH or `~/.local/everparse/bin/`).

## Project structure

- `lib/` -- core `wire` library: DSL types, 3D codegen, Codec (parse/encode), FFI stub generators
- `lib/c/` -- `wire.c` sublibrary: EverParse pipeline (generate .3d, run 3d.exe, generate C tests)
- `lib/diff/` and `lib/diff-gen/` -- differential test infrastructure
- `examples/` -- protocol definitions as Wire DSL examples
  - `examples/space/` -- space protocols (CLCW, SpacePacket, TMFrame, etc.)
  - `examples/tcpip/` -- TCP/IP headers (Ethernet, IPv4, TCP, UDP) with zero-copy `byte_slice` demo
- `bench/` -- benchmarks using schemas from `examples/`
  - `bench/timing.ml` -- decode vs zero-copy get/set timing and allocation
  - `bench/memtrace.ml` -- allocation profiling
  - `bench/perf.ml` -- EverParse C vs OCaml vs FFI comparison (space protocols)
  - `bench/tcpip/` -- EverParse C vs OCaml vs FFI comparison (TCP/IP)
- `fuzz/` -- Crowbar fuzz tests (`fuzz_wire.ml`) covering all DSL combinators
- `test/` -- Alcotest unit tests and differential tests (`test/diff/`)

## Code generation pipeline

All C code generation flows through `Wire_c` (no duplication):
1. `Wire.to_3d` / `Wire.to_3d_file` -- generate .3d files from Wire DSL
2. `Wire_c.run_everparse` -- invoke EverParse to produce C parsers
3. `Wire.to_c_stubs` / `Wire.to_ml_stubs` -- generate OCaml FFI bindings calling `Validate` directly (not through EverParse Wrapper)

## Benchmarking principles

Benchmarks compare four columns, all derived from the same Wire DSL definitions:

1. **Pure OCaml** -- `Wire.Codec.decode` and `Wire.Codec.get` (zero-copy). Always use ocaml-wire, never custom OCaml parsers.
2. **EverParse C** -- generated verified C validator from `.3d` files, timed in a pure C loop.
3. **OCaml→C FFI** -- OCaml calling the EverParse-generated C validator via generated stubs.
4. **Existing C implementations** -- when available (e.g., lwip, mirage-tcpip). Never write custom C parsers.

Never benchmark hand-written/custom parsers on either side. The point is to compare the Wire DSL's OCaml codegen against its 3D/EverParse C codegen, and optionally against existing third-party implementations.

## Style

- Use `Fmt.pf` / `Fmt.pr` instead of `Printf.fprintf` / `Printf.printf`
- Use `Alcotest.failf` instead of `Alcotest.fail (Fmt.str ...)`
- Avoid `Cu.` qualified names for Crowbar_util -- use `open Crowbar_util`
- No `make_` prefix on data constructor functions (e.g., `minimal_data` not `make_minimal_data`)
