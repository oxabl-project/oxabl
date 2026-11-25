# Performance guide

This guide tells you how to measure oxabl: the benches, the generated
workspace, the memory numbers, the command line script and CodSpeed. Measure
before and after a change, on the same input, and put both numbers in the pull
request.

## The benches

All benches use Criterion through `codspeed-criterion-compat`. CodSpeed runs
every `[[bench]]` target of the workspace on each push and pull request
(`.github/workflows/codspeed.yml`).

The end-to-end benches are in `crates/oxabl_bench` (not published) and in
`crates/oxabl_daemon/benches/daemon_features_bench.rs`. They read every file
from an in-memory file system, so the Simulation instrument counts all of
their work. `workspace_symbols/query` is the only exception: its workspace pass
reads the disk, but outside the measured part.

| Bench | Input | Surface |
| --- | --- | --- |
| `check/file` | `resources/lsp_bench.p` and its two includes | `oxabl check <file>`: the lint run, then the format check, on a new pipeline |
| `check/workspace` | the 300 roots of the generated `ci` workspace | `oxabl check <dir>`: one pipeline for the walk |
| `format/file_end_to_end` | `lsp_bench.p` | `oxabl format`, Format Document: `FormatPipeline::format` from the text |
| `navigation/definitions` | `lsp_bench.p` | one go to definition request: expansion, analysis, query |
| `navigation/links` | the `ci` root with the most includes | one document links request |
| `workspace_symbols/query` | the `ci` workspace, on disk | one `oxabl/symbolSearch` request after the workspace pass |
| `daemon_cycle/features_on_one_revision` | `lsp_bench.p` | an edit, then the diagnostics, semantic tokens, inlay hints and outline of that revision |
| `lexer/large_mixed/{lsp_bench,1mb}` | `lsp_bench.p`; the ten error-free `bench_parser_*` fixtures 40 times (1,048,400 bytes) | `oxabl conformance` |
| `parser/large_mixed/{lsp_bench,1mb}` | the same | `oxabl conformance` |
| `semantic/large/lsp_bench/{declare,resolve,check}` | `lsp_bench.p` with its includes expanded | `oxabl analyze` |
| `semantic/large/nested_blocks/{declare,resolve,check}` | 400 procedures with blocks 8 deep | the scope lookup of the resolve pass |
| `parser/temp_tables_v2` | `resources/bench_parser_temp_tables_v2.abl` | the parse of temp-table definitions |
| `parser/pipeline_with_includes_in_memory` | `bench_preprocessor_pipeline.abl` and its includes in memory | preprocess, lex, parse |

`parser/temp_tables` measures the error recovery: its fixture names a field
`status`, which is a reserved keyword. Its input does not change, so its
history stays comparable. `parser/temp_tables_v2` is the corrected copy.
`parser/pipeline_with_includes` reads its includes from the disk, and the
Simulation instrument does not count system calls; the `_in_memory` bench is
the same input with no system call.

### Rules for a new bench

- Put a new input in a new fixture with its own bench name. CodSpeed compares a
  bench by name, so a grown fixture reads as a regression of the old bench.
- Keep the CI benches small: the CodSpeed job has a time limit, and the
  Simulation instrument is about 50 times slower than the run. Use the large
  generated workspace locally.
- An input must parse with no error, or the bench measures the error recovery.
  `crates/oxabl_bench/tests/fixtures_parse.rs` parses every
  `resources/bench_*.abl`, the large inputs and `lsp_bench.p` after expansion.
- Read every file from an `InMemoryFileSystem`. CodSpeed marks a bench that
  reads the disk as "Dominated by system calls", and its number is too low.

### Run the benches

```sh
cargo bench -p oxabl_bench                                  # e2e_bench and large_bench
cargo bench -p oxabl_bench --bench e2e_bench -- check/      # one group
cargo bench -p oxabl_daemon --bench daemon_features_bench
cargo bench -p oxabl_bench -- --warm-up-time 1 --measurement-time 3   # a quick run
```

## Memory

### Allocations of each bench

The benches of `oxabl_bench` and `daemon_features_bench` install a counting
global allocator (`oxabl_bench::alloc::Counting`, over the system allocator).
Before Criterion measures a bench, the bench runs one time under
`oxabl_bench::alloc::measure` and prints one row to stderr:

```text
memory check/file: 44857 allocations, 28645632 bytes requested, peak 11078973 bytes
```

- `allocations`: calls of `alloc`, `alloc_zeroed` and `realloc`.
- `bytes requested`: the bytes of those calls. A `Vec` that grows counts each
  of its sizes.
- `peak`: the most bytes live at one time during the run, above the live bytes
  at its start.

Set `OXABL_BENCH_MEMORY=<file>` to append each row to the file as one JSON
object, to compare two commits:

```sh
OXABL_BENCH_MEMORY=$PWD/target/mem-before.jsonl cargo bench -p oxabl_bench -- --warm-up-time 1 --measurement-time 1
```

The counts are for the system allocator and do not depend on the time, so they
are the same on a loaded machine.

### CodSpeed's Memory instrument

The job `codspeed-memory` runs the benches of `oxabl_lexer`, `oxabl_parser`,
`oxabl_bench` and `oxabl_daemon` with `mode: memory`. CodSpeed shows the
allocated bytes and the peak of each bench on the pull request, next to the
Simulation result.

### Allocation budgets

`crates/oxabl_bench/tests/alloc_budget.rs` lexes and parses
`resources/lsp_bench.p` (20,311 tokens) under the counting allocator, and
compares the allocations and the bytes with a budget:

| Stage | Allocations | Bytes |
| --- | --- | --- |
| lex | 169 | 1,245,667 |
| parse | 11,877 | 3,943,096 |

The test fails when a number is more than 2% above its budget. It also fails
when a number is more than 10% below its budget: then lower the budget in the
same pull request, so the gain stays.

### Type sizes

A `const` assertion holds the size of each type that a file has many of:
`Token` (48 bytes), `TokenValue` (24), `Identifier` (32), `Literal` (40),
`Expression` (80), `Statement` (744), `Scope` (1,464), `BindingMap` (144) and
`Symbol` (64). A type that grows does not compile. To see the sizes of the
variants:

```sh
touch crates/oxabl_ast/src/lib.rs
CARGO_INCREMENTAL=0 cargo +nightly rustc -q -p oxabl_ast --lib --target-dir target/nightly-sizes -- -Zprint-type-sizes \
  | grep -A4 'type: .statement::StatementKind'
```

## The generated workspace

`scripts/bench/gen-workspace` writes a synthetic ABL workspace. The same knobs
give the same files, byte for byte. Every file is synthetic, and every root
preprocesses and parses with no finding (`crates/oxabl_bench/tests/generate.rs`).

```sh
scripts/bench/gen-workspace --preset erp --stats          # the shape, no files
scripts/bench/gen-workspace --preset erp target/ws-erp    # write it
scripts/bench/gen-workspace --preset ci --programs 1000 target/ws-1000
```

| Preset | Files | Roots | Size | Use |
| --- | --- | --- | --- | --- |
| `ci` | 372 | 300 | 0.8 MB | `check/workspace`, `navigation/links`, `workspace_symbols/query` |
| `medium` | 1,408 | 700 | 23 MB | a quick local run |
| `erp` | 13,998 | 7,000 | 229 MB | the shape of a large ERP code base |

The `erp` preset has six includes that more than 1,000 roots read (the largest
fan-in is 5,125 roots), roots that read about 300 includes, chains of includes
10 deep, and 248 roots of more than 30,000 lines after expansion. The knobs are
in `crates/oxabl_bench/src/generate.rs` (`Knobs`): the numbers of programs and
classes, the file sizes (on a log scale), the include fan-in and fan-out, the
depth of the include chains, the density of `&SCOPED-DEFINE`, `&IF`, `{&name}`
and include arguments, the temp-tables and datasets of the includes, and the
tables of the `.df` schema.

## The command line

`scripts/bench/cli.sh` measures the binary that ships. It builds the release
binary, generates `big.p` (1 MB) and a workspace below `target/bench-cli`, and
runs `check`, `format --check`, `conformance -j 1` and `explain`:

```sh
scripts/bench/cli.sh                                  # every surface, every tool
scripts/bench/cli.sh --tools time --preset erp        # wall time and peak RSS on the erp workspace
scripts/bench/cli.sh --tools callgrind --only check-lsp,check-ws
```

| Tool | Binary | Numbers |
| --- | --- | --- |
| `time` (GNU `time -v`) | release (jemalloc) | wall time, peak RSS |
| `heaptrack` | release with `--features system-allocator` | peak heap, allocation calls |
| `callgrind` | release (jemalloc) | instructions; inclusive share and calls of `tokenize` and `parse_program`; self share of the allocator, of `memcpy` and of C library code with no symbols |

heaptrack and valgrind's DHAT see only the C library's allocator, and the
`oxabl` binary uses jemalloc. The cargo feature `system-allocator` of `oxabl`
builds the binary with the system allocator for them. Never release it.

A missing tool comes from nixpkgs through `nix shell` when `nix` is on `PATH`.
The script exits with a non-zero status when a command fails (a status other
than 0, or 1 for findings or drift).

### Read the numbers

- Wall time and RSS change with the load of the machine. Use them as a rough
  size only.
- Instruction counts from callgrind are the same on each run. Compare two
  commits with them when the machine is loaded.
- `tokenize` and `parse_program` show how many times a command lexes and parses
  the same text, and the share of the run that this costs.
- When the C library has no symbols, `memcpy` shows as an address. Its cost is
  then in the column `unnamed libc self`.

### Callgrind by hand

```sh
cargo build --release -p oxabl
valgrind --tool=callgrind --callgrind-out-file=target/check.out target/release/oxabl check resources/lsp_bench.p -I resources
callgrind_annotate --inclusive=yes target/check.out | head -40      # inclusive cost
callgrind_annotate --tree=caller target/check.out | grep -B8 '\*  ???:oxabl_lexer::tokenize '   # who calls it, how often
python3 -I scripts/bench/callgrind_summary.py target/check.out      # the summary of cli.sh, as JSON
```

To count only one stage, use `--toggle-collect='*parse_program*'`.

## CodSpeed

The job `codspeed` uses the Simulation instrument: it runs each bench one time
under valgrind and counts instructions and cache misses. The result does not
depend on the load of the runner. The pull request comment shows each bench that
changed by more than the threshold.

- A new bench shows as "new". It has no result on the base to compare with.
- "Dominated by system calls" means that the bench reads files or a socket.
  Simulation does not count system calls, so the number is too low.
- A bench whose fixture changed is not comparable. Give a new input a new name.
- To find the cause of a change, open the flame graph of the bench on CodSpeed
  and compare it with the base.

The job `codspeed-memory` uses the Memory instrument (see [Memory](#memory)).

## Baseline

x86-64 Linux, rustc 1.100.0, a machine that was not idle. Criterion times are
the mean of 10 samples.

| Bench | Time | Allocations | Bytes requested | Peak |
| --- | --- | --- | --- | --- |
| `check/file` | 11.0 ms | 44,857 | 28.6 MB | 11.1 MB |
| `check/workspace` | 244 ms | 645,769 | 464 MB | 4.1 MB |
| `format/file_end_to_end` | 7.7 ms | 19,961 | 16.2 MB | 10.1 MB |
| `navigation/definitions` | 9.5 ms | 27,689 | 13.7 MB | 5.6 MB |
| `navigation/links` | 2.7 ms | 7,197 | 5.3 MB | 2.4 MB |
| `workspace_symbols/query` | 0.75 ms | 5,395 | 0.6 MB | 0.5 MB |
| `daemon_cycle/features_on_one_revision` | 23.2 ms | 75,568 | 39.9 MB | 8.3 MB |
| `parser/large_mixed/lsp_bench` | 1.9 ms | | | |
| `parser/large_mixed/1mb` | 18.4 ms | 74,226 | 53.0 MB | 29.8 MB |
| `semantic/large/lsp_bench/resolve` | 0.47 ms | 12 | 0.35 MB | 0.34 MB |
| `semantic/large/nested_blocks/declare` | 1.9 ms | 4,798 | 12.8 MB | 6.5 MB |
| `semantic/large/nested_blocks/resolve` | 14.4 ms | 14 | 1.8 MB | 1.7 MB |

`scripts/bench/cli.sh` on the same machine (`ci` workspace):

| Surface | Wall | Peak RSS | Heap peak | Allocation calls | Instructions | `tokenize` | `parse_program` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `check lsp_bench.p` | 0.01 s | 24 MB | 10.5 MB | 45,471 | 107.9 M | 36.5% (172 calls) | 25.1% (3 calls) |
| `check big.p` | 5.09 s | 120 MB | 100.2 MB | 623,866 | 66,943 M | 0.6% | 0.3% |
| `check src` (300 roots) | 0.04 s | 57 MB | 16.6 MB | 819,211 | 1,512 M | 34.3% | 29.0% |
| `format --check big.p` | 0.08 s | 98 MB | 89.7 MB | 154,855 | 457 M | 49.1% (3 calls) | 16.2% (1 call) |
| `conformance -j 1 big.p` | 0.03 s | 59 MB | 41.1 MB | 74,854 | 160 M | 43.4% | 46.2% |
| `explain lsp_bench.p` | 0.01 s | 20 MB | 7.0 MB | 25,058 | 46.3 M | 17.1% | 20.1% |

`check big.p` spends 98% of its instructions in `SourceMap::new`, called for
each label of each finding when the findings are reported
(`LabelPlaces::rows` in `crates/oxabl/src/main.rs`). The in-process benches do
not report findings, so only `cli.sh` shows this cost.
