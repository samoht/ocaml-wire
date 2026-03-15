# CLAUDE.md

## Build & Test

```
make build       # dune build
make test        # dune runtest
make bench       # BUILD_EVERPARSE=1 dune exec bench/bench_perf.exe
make memtrace    # MEMTRACE=trace.ctf dune exec bench/bench_wire_memtrace.exe && memtrace_hotspots trace.ctf
make clean       # dune clean
```

`make bench` requires EverParse (`3d.exe` in PATH or `~/.local/everparse/bin/`).

## Project structure

- `lib/` -- core `wire` library: DSL types, 3D codegen, Codec (parse/encode), FFI stub generators
- `lib/c/` -- `wire.c` sublibrary: EverParse pipeline (generate .3d, run 3d.exe, generate C tests)
- `lib/diff/` and `lib/diff-gen/` -- differential test infrastructure
- `bench/` -- benchmarks: `bench_perf.exe` (EverParse C vs OCaml vs FFI), `bench_wire_memtrace.exe` (allocation profiling)
- `bench/bench_schemas.ml` -- all benchmark schemas defined once, shared by all bench targets
- `fuzz/` -- Crowbar fuzz tests (`fuzz_wire.ml`) covering all DSL combinators
- `test/` -- Alcotest unit tests and differential tests (`test/diff/`)

## Code generation pipeline

All C code generation flows through `Wire_c` (no duplication):
1. `Wire.to_3d` / `Wire.to_3d_file` -- generate .3d files from Wire DSL
2. `Wire_c.run_everparse` -- invoke EverParse to produce C parsers
3. `Wire.to_c_stubs` / `Wire.to_ml_stubs` -- generate OCaml FFI bindings calling `Validate` directly (not through EverParse Wrapper)

## Style

- Use `Fmt.pf` / `Fmt.pr` instead of `Printf.fprintf` / `Printf.printf`
- Use `Alcotest.failf` instead of `Alcotest.fail (Fmt.str ...)`
- Avoid `Cu.` qualified names for Crowbar_util -- use `open Crowbar_util`
- No `make_` prefix on data constructor functions (e.g., `minimal_data` not `make_minimal_data`)
