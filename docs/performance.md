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
| `check/workspace` | the 300 roots of the generated `ci` workspace: no finding, no drift | `oxabl check <dir>`: one pipeline for the walk |
| `check/workspace_include_findings` | the same, with 100 unused variables in each hot include (42,700 findings in includes, about 140 for each root) | `oxabl check <dir>` where shared includes have findings: the fold of include findings (`IncludeRows::add` in `oxabl_analyze`). Each row has 1 expansion, so no finding joins an earlier row. `oxabl check` of this workspace with the release binary: `IncludeRows::add` was 7.4% of the run with a key of a path and a message, and is 1.3% with a key of plain data |
| `check/include_findings_repeated` | one root with 60 procedures that each include a file of 100 unused variables: 100 rows with 60 expansions each | the fold where a finding joins an earlier row (5,900 of the 6,000 findings). `IncludeRows::add` is 6.7% of `oxabl check` of the same input with the old key and 2.0% with the new one. That gain is too small for CodSpeed to report as a change |
| `format/file_end_to_end` | `lsp_bench.p` | `oxabl format`, Format Document: `FormatPipeline::format` from the text |
| `format/synthetic_mixed`, `format/synthetic_mixed_with_tokens` | a synthetic program of 40 repeated units | the formatter engine on a parsed tree: `format()` lexes the source itself, `format_with_tokens()` takes the tokens of the caller, as the pipeline does. The lex and parse are outside the timed loop |
| `navigation/definitions` | `lsp_bench.p` | one go to definition request: expansion, analysis, query |
| `navigation/links` | the `ci` root with the most includes | one document links request |
| `workspace_symbols/query` | the `ci` workspace, on disk | one `oxabl/symbolSearch` request after the workspace pass |
| `daemon_cycle/features_on_one_revision` | `lsp_bench.p` | an edit, then the diagnostics, semantic tokens, inlay hints, outline and document links of that revision. No schema is loaded, so the inlay hints have almost no work |
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
the same input with no system call. The expansion of that input has two parse
errors, so both benches also measure error recovery;
`tests/fixtures_parse.rs` pins the two errors.

The counting allocator of the `oxabl_bench` and `daemon_features_bench` benches
adds a few atomic operations to each allocation. So their instruction counts
are not comparable with those of the older `lexer/*` and `parser/*` benches,
which use the plain system allocator.

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
cargo bench -p oxabl_bench                                  # e2e_bench and large_bench (the library has no bench)
cargo bench -p oxabl_bench --bench e2e_bench -- check/      # one group
cargo bench -p oxabl_daemon --bench daemon_features_bench
cargo bench -p oxabl_bench -- --warm-up-time 1 --measurement-time 3   # a quick run
```

## Memory

### Allocations of each bench

The benches of `oxabl_bench` and `daemon_features_bench` install a counting
global allocator (`oxabl_bench::alloc::Counting`, over the system allocator).
Before Criterion measures a bench, the bench runs one time to fill the lazy
statics and caches, then one time under `oxabl_bench::alloc::measure`, and
prints one row to stderr:

```text
memory check/file: 44569 allocations, 28481330 bytes requested, peak 10916557 bytes, retained 0 bytes
```

- `allocations`: calls of `alloc`, `alloc_zeroed` and `realloc`.
- `bytes requested`: the bytes of those calls. A `Vec` that grows counts each
  of its sizes.
- `peak`: the most bytes live at one time during the run, above the live bytes
  at its start.
- `retained`: the bytes still live after the run, above the live bytes at its
  start. Not 0 when the bench keeps what it made (the semantic passes return
  their tables).

Both runs happen also when a Criterion filter leaves the bench out.

Set `OXABL_BENCH_MEMORY=<file>` to append each row to the file as one JSON
object, to compare two commits. Give an absolute path: `cargo bench` runs each
bench in the directory of its crate.

```sh
OXABL_BENCH_MEMORY=$PWD/target/mem-before.jsonl cargo bench -p oxabl_bench -- --warm-up-time 1 --measurement-time 1
```

The counts are for the system allocator and do not depend on the time, so they
are the same on a loaded machine. They can differ by a few hundred between
machines, libc versions and Rust toolchains: compare two commits on the same
machine with the same toolchain.

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

The counts are the same on each run of one toolchain. The test fails when a
number is more than 0.5% above its budget. It also fails when a number is more
than 0.5% below its budget, and the message gives the new number: lower the
budget in the same pull request, so the gain stays. A new stable Rust can move
the counts (CI uses the newest stable). Then re-measure with
`cargo test -p oxabl_bench --test alloc_budget -- --nocapture` and set the
budgets to the printed numbers.

`crates/oxabl_bench/tests/include_rows_alloc.rs` guards the allocations of the
fold of the findings in includes. It lints a root that includes a file of
unused variables, and counts the allocations of the lint run for two kinds of
finding:

| Finding | Limit | Now | With the old key (a cloned path and message) |
| --- | --- | --- | --- |
| makes a row (50 and 250 findings, one expansion) | 9 | 8.6 | 10.6 |
| joins the row of an earlier expansion (2 and 6 expansions of 50 findings) | 7 | 6.8 | 9.8 |

The guard counts allocations only. A slower hash or a longer key that
allocates nothing does not fail it. Find that cost with callgrind on
`oxabl check` (see "The command line"), and with the benches
`check/workspace_include_findings` and `check/include_findings_repeated`. A gain
of the size of this one is below the threshold of CodSpeed.

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
give the same files, byte for byte, on one platform (the file sizes use
floating point, so another libm can differ). Every file is synthetic. Every
root preprocesses and parses with no finding, and `oxabl format` changes no
root and no include (`crates/oxabl_bench/tests/generate.rs`, on `ci` and a
small `erp`). `--include-findings N` writes N unused variables in each hot
include, so each root that reads one has findings in an include.

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
binary, generates `big.p` (1 MB) and two workspaces below `target/bench-cli`
(one clean, one with findings in its hot includes), and runs `check`,
`format --check`, `conformance -j 1` and `explain`. It finds the binary
through cargo, so `CARGO_TARGET_DIR` is followed. An input is made again when
what makes it changed (a stamp file beside it holds a hash of the generator or
of the fixtures), and is written to a temporary name first, so an interrupted
run leaves no partial input.

```sh
scripts/bench/cli.sh                                  # every surface, every tool
scripts/bench/cli.sh --tools time --preset erp        # wall time and peak RSS on the erp workspace
scripts/bench/cli.sh --tools callgrind --only check-lsp,check-ws
```

| Tool | Binary | Numbers |
| --- | --- | --- |
| `time` (GNU `time -v`) | release (jemalloc) | wall time, peak RSS |
| `heaptrack` | release with `RUSTFLAGS="--cfg oxabl_system_allocator"` | peak heap, allocation calls |
| `callgrind` | release (jemalloc) | instructions; inclusive share and calls of `tokenize` and `parse_program`; self share of the allocator, of `memcpy` and of C library code with no symbols |

heaptrack and valgrind's DHAT see only the C library's allocator, and the
`oxabl` binary uses jemalloc. The flag `--cfg oxabl_system_allocator` builds the
binary with the system allocator for them. It is a flag and not a cargo
feature, so no dependent crate and no `--all-features` build can turn jemalloc
off. `cli.sh` builds that binary in its own target directory
(`<target>/system-allocator`) and stops when it still links jemalloc.

A missing tool comes from nixpkgs through `nix shell` when `nix` is on `PATH`.

Each surface has the exit status that its input gives: `check` exits 1 on
`lsp_bench.p`, `big.p` and the workspace with include findings
(`--include-findings`), and 0 on the clean workspace; `format --check` exits 1
on `big.p` and 0 on the workspace; `conformance` and `explain` exit 0. `check`
also exits 1 when it cannot read or analyse a file, so the status alone cannot
tell a finding from a failure. A run fails when a command exits with another
status, or writes a line to stderr that starts with `error:` (an analysis or
formatting failure, a file it cannot read). A failed run gives numbers for
less work, which look like a gain.

### Read the numbers

- Wall time and RSS change with the load of the machine. Use them as a rough
  size only.
- Instruction counts from callgrind do not depend on the load. Compare two
  commits with them when the machine is loaded. `cli.sh` runs the walks
  (`check-ws*`, `format-ws`) with `-j 1` under callgrind: with several threads,
  each worker indexes some files again, as the threads happen to run, so the
  calls change between runs and the instructions by about 1%, and the walk
  costs about 20% more than with `-j 1`. With `-j 1` the call counts are the
  same on each run, and the instructions still change by about 0.5% (hash maps
  with a random seed). `time` and heaptrack use the default `-j`.
- `tokenize` and `parse_program` show the share of the run and the calls, as
  "N full + M small". A call is full when its caller pays, for each call, at
  least a tenth of what the most expensive caller pays: a lex or parse of a
  whole file. Some passes lex short pieces of text many times (the resolve
  pass, and the formatter's `apply_end_type`); these are the small calls. So
  on one file, "5 full" means that the command lexes the file five times. In a
  walk, the count is not a number for each root: the index lexes and parses
  the includes too (`index_parsed`), so read the callers.
  `callgrind_summary.py` without `--row` names each full caller with its calls
  and its share.
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

## The scan for SHARED producers

A `DEFINE SHARED` consumer finds its `NEW SHARED` producer in a map of the
walk's files (`oxabl_index::producers`). `oxabl check`, `oxabl watch` and the
daemon's workspace pass build the map when the first file asks for a
producer (`DeferredProducers`), on the threads of `-j`. A file that asks
nothing does not wait for the map, and a walk with no `SHARED` consumer does
not scan. A file that asks waits until the map is complete, so no answer
comes from a part of the map.

The scan reads each file one time. A file that has no `{` and does not say
`NEW SHARED` or `NEW GLOBAL` is not parsed. Any other file is parsed at most
one time as written: a file that says the words, to find its own
definitions, and a file with a `{`, to know whether the index can read it
through its includes. Such a file is expanded one time. The names come from
a token scan of the two texts: each `DEFINE` with `NEW` after it is parsed
alone. The scan of the expanded text stops after the last `NEW SHARED` in
it. There is no declare pass and no parse of the whole expanded text.

The map can list a file that is not a producer: one whose expanded text does
not parse, or one whose `NEW SHARED` definition is in a later arm of an
`&IF` that the declare pass does not keep. The index of the consumer reads
the facts of each listed file and keeps only a file that defines the name,
so the answer does not change.

`crates/oxabl/tests/check_producer_scan.rs` counts the parses and expansions
of the scan for each file. A test build prints them when `OXABL_TEST_TALLY` is
set (`oxabl_common::test_tally`). The test fails when the scan parses or
expands a file two times, or scans a walk with no consumer.

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

x86-64 Linux, rustc 1.100.0. The machine was loaded, so this table has no
times: read them on CodSpeed. The memory numbers are of the warm run.

| Bench | Allocations | Bytes requested | Peak |
| --- | --- | --- | --- |
| `check/file` | 44,569 | 28.5 MB | 10.9 MB |
| `check/workspace` | 618,870 | 457.1 MB | 3.1 MB |
| `check/workspace_include_findings` | 1,070,457 | 684.4 MB | 3.2 MB |
| `format/file_end_to_end` | 19,961 | 16.2 MB | 10.1 MB |
| `navigation/definitions` | 27,690 | 13.7 MB | 5.6 MB |
| `navigation/links` | 7,198 | 5.3 MB | 2.4 MB |
| `workspace_symbols/query` | 5,395 | 0.6 MB | 0.5 MB |
| `daemon_cycle/features_on_one_revision` | 87,891 | 47.2 MB | 6.9 MB |
| `lexer/large_mixed/lsp_bench` | 169 | 1.2 MB | 1.2 MB |
| `lexer/large_mixed/1mb` | 43 | 10.1 MB | 10.1 MB |
| `parser/large_mixed/lsp_bench` | 11,877 | 3.9 MB | 2.6 MB |
| `parser/large_mixed/1mb` | 74,226 | 53.0 MB | 29.8 MB |
| `semantic/large/lsp_bench/declare` | 43 | 0.9 MB | 0.4 MB |
| `semantic/large/lsp_bench/resolve` | 12 | 0.35 MB | 0.34 MB |
| `semantic/large/nested_blocks/declare` | 4,798 | 12.8 MB | 6.5 MB |
| `semantic/large/nested_blocks/resolve` | 14 | 1.8 MB | 1.7 MB |

`scripts/bench/cli.sh` on the same machine (`ci` workspace). Wall time and RSS
are rough:

| Surface | Wall | Peak RSS | Heap peak | Allocation calls | Instructions | `tokenize` | `parse_program` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `check lsp_bench.p` | 0.04 s | 29 MB | 10.5 MB | 45,472 | 107.6 M | 36.6% (5 full + 167 small) | 25.2% (3 full) |
| `check big.p` | 7.76 s | 122 MB | 100.2 MB | 623,866 | 66,898 M | 0.6% (6 full + 159 small) | 0.3% (3 full) |
| `check src` (300 roots; callgrind with `-j 1`) | 0.14 s | 54 MB | 16.6 MB | 820,382 | 1,236 M | 34.4% (1,707 full) | 27.3% (1,107 full) |
| `check --include-findings src` (42,700 include findings) | 0.46 s | 87 MB | 36.1 MB | 6,344,394 | 15,479 M | 3.6% (1,707 full) | 3.0% (1,107 full) |
| `format --check src` (`-j 1` under callgrind) | 0.01 s | 30 MB | 4.0 MB | 160,963 | 362 M | 47.7% (900 full) | 16.4% (300 full) |
| `conformance -j 1 big.p` | 0.04 s | 53 MB | 41.1 MB | 74,854 | 160 M | 43.3% (1 full) | 46.2% (1 full) |
| `explain lsp_bench.p` | 0.01 s | 18 MB | 7.0 MB | 25,059 | 46.4 M | 17.0% (1 full) | 20.0% (1 full) |

The rows above are from before the formatter took the tokens of its caller
(`format_with_tokens`), and before `check` built each file's line table once.
For this change alone, from commit `2285153` on the same machine and
toolchain: `check lsp_bench.p` goes from 103.6 M to 94.5 M instructions
(`tokenize`: 4 full calls, 0 small), and `format --check src` goes from 355.8 M
to 288.6 M (600 full calls instead of 900). The warm run of `check/file` makes
44,318 allocations (27.2 MB requested, peak 9.7 MB) and `format/file_end_to_end`
makes 19,710 (14.9 MB, peak 8.9 MB); the memory table above has the numbers
from before.

The test-only feature `test-counters` of `oxabl_lexer` counts the lexers and
the calls of `tokenize`. Cargo unifies a feature of a dev-dependency over a
workspace build, so a workspace-wide build, such as `cargo test --workspace` or
the CodSpeed CPU build, has it on. Its cost is up to three atomic additions
for each lexer, which is very small, and a build of the shipped binary does
not have it.

On the `erp` workspace (7,000 roots, 16 cores, a loaded machine, before the
generated text was made format-clean), `oxabl check src` took 23.3 s with a
peak RSS of 1,523 MB, a heap peak of 1.34 GB and 417 million allocation calls;
`oxabl format --check src` took 1.6 s and 275 MB.

`check big.p` spends 98% of its instructions in `SourceMap::new`, which
`LabelPlaces::rows` (`crates/oxabl/src/main.rs`) builds for each label of each
finding when the findings are reported (10,895 findings on `big.p`). This is a
regression, not an expected cost: the same run took 1,183 M instructions
before related locations were added to the findings. Issue #570 tracks it. The
in-process benches do not report findings, so only `cli.sh` shows it.

`check --include-findings src` has the same shape: 63% of its instructions are
in `SourceMap::new`, which `LabelPlaces::resolver` builds for each include
finding when it is reported. `check/workspace_include_findings` runs the fold
of the findings, not their report, so it does not show this cost.
