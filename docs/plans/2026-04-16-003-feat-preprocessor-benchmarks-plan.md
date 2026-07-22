---
title: Preprocessor Benchmark Suite
type: feat
status: completed
date: 2026-04-16
origin: docs/brainstorms/2026-04-16-preprocessor-benchmarks-requirements.md
---

# Preprocessor Benchmark Suite

## Overview

Add CodSpeed-tracked benchmarks for the preprocessor pass at three granularities — isolation, full pipeline (with real include resolution), and hot-path micros — using hand-crafted synthetic fixtures checked into `resources/`. The purpose is to establish a baseline so future work (include cache, hot-path tuning) has a measurable signal. No throughput target.

## Problem Statement / Motivation

The current bench suite (`oxabl_lexer`, `oxabl_parser`, `oxabl_common`) covers self-contained ABL fixtures only. Real-world throughput on the ABL corpus drops from ~1k files/sec (parse-only) to ~70 files/sec once preprocessing and include resolution run. Today, oxabl has zero CodSpeed signal on that drop, on the preprocessor itself, or on its hot paths. Before optimizing anything (especially before introducing an include-file cache), we need a stable baseline; otherwise every "improvement" is unfalsifiable.

(see origin: docs/brainstorms/2026-04-16-preprocessor-benchmarks-requirements.md)

## Proposed Solution

Three bench targets, split across two crates to respect the existing dependency layering:

1. **`oxabl_preprocessor/benches/preprocessor_bench.rs`** — preprocessor-isolation bench + hot-path micros. Hosted in `oxabl_preprocessor` since it has no parser dependency.
2. **Extend `oxabl_parser/benches/parser_bench.rs`** with a new `pipeline_with_includes` group that runs tokenize → preprocess (resolving real `.i` files from `resources/`) → parse. Lives in `oxabl_parser` because the parser already depends on the preprocessor; the preprocessor must not gain a dev-dep on the parser (layering inversion).
3. New synthetic fixture set under `resources/` covering preprocessor features, with naming aligned to the existing `bench_*` convention.

CodSpeed CI auto-discovers `[[bench]]` targets, so the workflow at `.github/workflows/codspeed.yml` needs no changes.

## Technical Considerations

### Crate / Cargo wiring

- `crates/oxabl_preprocessor/Cargo.toml` — add `[dev-dependencies] codspeed-criterion-compat = ...` (match version used in `oxabl_lexer`/`oxabl_parser`) and a `[[bench]]` entry:
  ```toml
  [[bench]]
  name = "preprocessor_bench"
  harness = false
  ```
- No changes to `crates/oxabl_parser/Cargo.toml` — `[[bench]]` for `parser_bench` already exists; the new group is added inside the existing harness.

### Fixture loading

Reuse the established `load_fixture(name)` helper pattern from `crates/oxabl_lexer/benches/lexer_bench.rs:7` and `crates/oxabl_parser/benches/parser_bench.rs:8` (resolves `CARGO_MANIFEST_DIR/../../resources/<name>`). Copy verbatim into `preprocessor_bench.rs`; do not factor it to a shared crate yet (YAGNI — three call sites doesn't justify it).

### Driving the preprocessor

`Preprocessor::new(fs: &dyn FileSystem, include_paths: &[PathBuf])` takes a `FileSystem` trait object (`oxabl_workspace::FileSystem`). Two flavors:

- **Isolation + micros**: use `oxabl_workspace::InMemoryFileSystem` (already exists at `crates/oxabl_workspace/src/file_system.rs:53`) for fixtures that need includes, or no FS at all when the fixture is self-contained. In-memory keeps the bench measuring preprocessor work, not I/O.
- **Pipeline**: use `oxabl_workspace::RealFileSystem` rooted such that `include_paths` contains the workspace's `resources/` directory. The point of this bench is to mirror the corpus run, including disk I/O.

`Preprocessor::process(file: FileId, source: &str)` is the single entry point per the public API in `crates/oxabl_preprocessor/src/lib.rs:6` and `src/preprocessor.rs:37`. All three bench groups call it; only the FS impl, fixtures, and follow-on stages differ.

### Throughput metric

Use `Throughput::Bytes(source.len() as u64)` consistent with the existing parser/lexer benches. For pipeline benches, the byte count is the source of the *entry* file, not the post-expansion size — this matches the "files/sec" framing the corpus number uses.

### Fixtures

Naming follows existing pattern `bench_<crate>_<topic>.abl`:

- `resources/bench_preprocessor_isolation.abl` — mid-sized self-contained ABL exercising `&SCOPED-DEFINE`, `{&var}` substitution, nested `&IF`/`&ELSEIF`/`&ENDIF`, and a few `{include.i}` references. Includes are satisfied by `InMemoryFileSystem` so the bench measures preprocessor work, not disk.
- `resources/bench_preprocessor_pipeline.abl` + `resources/bench_preprocessor_pipeline_a.i`, `_b.i`, `_nested.i` — pipeline entry file with real disk-resolved includes (deep nesting, ~3–4 levels), shaped to reflect the kind of include-heavy file dominating the corpus.
- `resources/bench_preprocessor_micro_scoped_define.abl` — concentrated `&SCOPED-DEFINE` + `{&var}` chain; minimal noise.
- `resources/bench_preprocessor_micro_var_substitution.abl` — many `{&var}` substitutions on already-defined vars.
- `resources/bench_preprocessor_micro_if_eval.abl` — many `&IF`/`&ELSEIF`/`&ENDIF` blocks with a mix of true/false conditions and nested branches.

Target per-iteration time ~10µs–10ms so CodSpeed has enough signal without thrashing — confirm during implementation by running `cargo bench` locally.

## System-Wide Impact

- **Interaction graph**: bench-only code; no production code paths change. New benches call into existing `Preprocessor::process`, `tokenize`, and `Parser::new/parse_program` exactly as today's pipelines do.
- **Error propagation**: benches `unwrap()` on fixture loads (existing pattern). If a fixture is malformed, the bench panics — visible immediately in CI.
- **State lifecycle**: none. Benches are pure functions of input.
- **API surface parity**: this validates the public `Preprocessor` API is sufficient for a downstream consumer (the bench itself is an existence proof). If we hit a wall driving it from outside the crate, that's a finding worth flagging in the brainstorm's deferred questions.
- **Integration test scenarios**: none — benches don't replace tests. Existing preprocessor unit tests remain authoritative.

## Acceptance Criteria

- [ ] `crates/oxabl_preprocessor/benches/preprocessor_bench.rs` exists and runs `cargo bench -p oxabl_preprocessor --bench preprocessor_bench` successfully.
- [ ] Bench has at least three groups: `isolation`, and a `micro/{scoped_define,var_substitution,if_eval}` set.
- [ ] `crates/oxabl_parser/benches/parser_bench.rs` gains a `pipeline_with_includes` bench (or group) running tokenize → preprocess → parse against `bench_preprocessor_pipeline.abl` with the include path set to `resources/`.
- [ ] All required fixtures exist in `resources/` with the names listed in the Technical Considerations section.
- [ ] `[dev-dependencies]` and a `[[bench]]` entry are added to `crates/oxabl_preprocessor/Cargo.toml`.
- [ ] `cargo codspeed build` succeeds locally and emits the new bench target. CodSpeed CI on the PR shows the new benches reporting numbers.
- [ ] `cargo fmt --check` and `cargo clippy -D warnings` pass on the new code.
- [ ] No production code changes outside Cargo.toml metadata.
- [ ] No changes to `.github/workflows/codspeed.yml`.

## Success Metrics

- After merge, CodSpeed dashboard shows baseline numbers for: `preprocessor/isolation`, `preprocessor/micro/scoped_define`, `preprocessor/micro/var_substitution`, `preprocessor/micro/if_eval`, `parser/pipeline_with_includes`.
- A future PR that touches preprocessor or include resolution produces a non-trivial CodSpeed delta on at least one of the new benches.

## Dependencies & Risks

- **`Preprocessor` public API sufficiency** — driving it from a bench requires `Preprocessor::new` + `process` + access to a `FileSystem` impl + `FileId` construction. All exist (`crates/oxabl_preprocessor/src/lib.rs:6`, `oxabl_common::FileId`, `oxabl_workspace::{InMemoryFileSystem, RealFileSystem}`). Low risk; confirm during implementation.
- **CodSpeed auto-discovery** — relies on `cargo codspeed` honoring all workspace `[[bench]]` entries. Existing setup at `.github/workflows/codspeed.yml` suggests yes; verify locally with `cargo codspeed build`.
- **Fixture realism** — synthetic fixtures may not exercise the same hot paths as the real corpus. Acceptable for v1 — the brainstorm explicitly chose synthetic over corpus-vendoring for reproducibility. Mitigate by sizing the pipeline fixture's include nesting to reflect what we observe in the ABL corpus.
- **Per-iteration time band** — CodSpeed has a sweet spot; fixtures that run too fast are noisy, too slow are flaky. Mitigate by sizing fixtures iteratively when running `cargo bench` locally.

## Implementation Steps

1. **Wire the bench harness.**
   - Edit `crates/oxabl_preprocessor/Cargo.toml`: add `[dev-dependencies] codspeed-criterion-compat = ...` (match existing version in `oxabl_lexer`/`oxabl_parser`), add `[[bench]] name = "preprocessor_bench" harness = false`.
   - Create `crates/oxabl_preprocessor/benches/preprocessor_bench.rs` with the `load_fixture` helper, `criterion_main!`/`criterion_group!`, and an empty `preprocessor_benchmarks` function.
2. **Author micro fixtures** in `resources/`:
   - `bench_preprocessor_micro_scoped_define.abl`
   - `bench_preprocessor_micro_var_substitution.abl`
   - `bench_preprocessor_micro_if_eval.abl`
3. **Add the micro bench group** in `preprocessor_bench.rs` — three `bench_function` calls driving `Preprocessor::process` with a no-include `InMemoryFileSystem`.
4. **Author the isolation fixture** `resources/bench_preprocessor_isolation.abl` plus any in-memory `.i` files registered into `InMemoryFileSystem` at bench setup time.
5. **Add the isolation bench group** in `preprocessor_bench.rs` — single bench, byte throughput on the entry file, `InMemoryFileSystem` carrying the includes.
6. **Author pipeline fixtures** in `resources/`: `bench_preprocessor_pipeline.abl` + `bench_preprocessor_pipeline_a.i`, `_b.i`, `_nested.i`. Shape: 3–4 level include nesting, mix of `&SCOPED-DEFINE`/`{&var}` substitution, ABL bodies non-trivial enough that the parse step has work to do.
7. **Extend `crates/oxabl_parser/benches/parser_bench.rs`** with a `pipeline_with_includes` bench:
   - Load `bench_preprocessor_pipeline.abl`.
   - Construct `RealFileSystem` and `include_paths = vec![resources_dir]`.
   - In `b.iter()`: `Preprocessor::process` → tokenize the resulting expanded source → `Parser::new(...).parse_program()`.
   - Throughput on the entry file's byte length.
8. **Local validation**:
   - `cargo bench -p oxabl_preprocessor --bench preprocessor_bench` runs cleanly.
   - `cargo bench -p oxabl_parser --bench parser_bench -- pipeline_with_includes` runs cleanly.
   - `cargo codspeed build` succeeds.
   - Tune fixture sizes if any iteration falls outside the ~10µs–10ms band.
9. **CI validation**:
   - `cargo fmt --check`, `cargo clippy -D warnings`, `cargo test`.
   - Push PR; confirm CodSpeed reports the new benches.
10. **Commit** following Conventional Commits: `feat(bench): add preprocessor benchmark suite`.

## Sources & References

- **Origin document:** [docs/brainstorms/2026-04-16-preprocessor-benchmarks-requirements.md](../brainstorms/2026-04-16-preprocessor-benchmarks-requirements.md) — carries forward: synthetic-fixture-only decision, three-target structure (isolation + pipeline + micros), success criterion = baseline-for-regressions (no throughput target), no cold-vs-warm cache split, no real-corpus vendoring.
- **Existing bench patterns**:
  - `crates/oxabl_lexer/benches/lexer_bench.rs` — `load_fixture` helper and group structure to copy.
  - `crates/oxabl_parser/benches/parser_bench.rs` — closest analog; the pipeline bench extends this file.
  - `crates/oxabl_common/benches/source_map_bench.rs` — multi-bench-per-group example.
- **Preprocessor API**: `crates/oxabl_preprocessor/src/lib.rs:6`, `crates/oxabl_preprocessor/src/preprocessor.rs:18-72`.
- **FileSystem impls**: `crates/oxabl_workspace/src/file_system.rs:9` (trait), `:33` (`RealFileSystem`), `:53` (`InMemoryFileSystem`).
- **CodSpeed workflow**: `.github/workflows/codspeed.yml` — auto-discovers `[[bench]]` targets, no edits needed.
- **Project benchmarking conventions**: `CLAUDE.md` § Benchmarks — "consider whether a new benchmark is warranted... add a benchmark or extend an existing fixture file".
