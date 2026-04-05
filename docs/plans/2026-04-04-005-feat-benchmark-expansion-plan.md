---
title: "feat: Expand benchmark suite to cover parser, lexer scenarios, and source map"
type: feat
status: completed
date: 2026-04-04
origin: docs/brainstorms/2026-04-04-benchmark-expansion-brainstorm.md
---

# feat: Expand benchmark suite to cover parser, lexer scenarios, and source map

## Overview

Add 13 new benchmarks across the parser, lexer, and source map crates to catch performance regressions via CodSpeed and identify optimization targets as the tooling grows. Currently only `oxabl_lexer` has benchmarks (2 benchmarks, same file). The parser (1800+ LOC, 50+ parse functions, 262 tests) has zero performance tracking.

## Problem Statement / Motivation

The parser is actively developed and growing. Without benchmarks, performance regressions land silently. The project's history shows that small changes can have outsized impact (PR #17 ~8%, PR #19 ~20%), making regression detection critical. CodSpeed CI is already wired up and auto-discovers `[[bench]]` targets — we just need to add them.

## Proposed Solution

Add benchmarks organized by Criterion benchmark groups with dedicated ~50-100 line `.abl` fixture files per scenario. Parser benchmarks measure tokenize+parse end-to-end (see brainstorm: decisions #6). Source map gets separate construction and lookup benchmarks.

### Cargo.toml Changes

**`crates/oxabl_parser/Cargo.toml`** — add:
```toml
[dev-dependencies]
criterion = { version = "4.3.0", package = "codspeed-criterion-compat" }

[[bench]]
name = "parser_bench"
harness = false
```

Note: `oxabl_lexer` is already a regular dependency of the parser, so `tokenize()` is available without an extra dev-dependency.

**`crates/oxabl_common/Cargo.toml`** — add:
```toml
[dev-dependencies]
criterion = { version = "4.3.0", package = "codspeed-criterion-compat" }

[[bench]]
name = "source_map_bench"
harness = false
```

No CI changes needed — `cargo codspeed build` auto-discovers all `[[bench]]` targets.

### Parser Benchmarks (`crates/oxabl_parser/benches/parser_bench.rs`)

7 benchmarks in a `"parser"` group, each with `Throughput::Bytes`:

| Benchmark ID | Fixture File | What It Measures |
|---|---|---|
| `parser/full_program` | `bench_keywords.abl` (existing, 633 lines) | End-to-end `parse_program()` on comprehensive ABL |
| `parser/expressions` | `bench_parser_expressions.abl` | Arithmetic, comparison, logical, ternary, function calls, nested expressions via `assign` statements |
| `parser/declarations` | `bench_parser_declarations.abl` | DEFINE VARIABLE, VAR, PARAMETER with various data types and options |
| `parser/control_flow` | `bench_parser_control_flow.abl` | DO, IF/THEN/ELSE, REPEAT, FOR EACH, CASE with nesting |
| `parser/oo_abl` | `bench_parser_oo_abl.abl` | CLASS, INTERFACE, METHOD, PROPERTY, CONSTRUCTOR, DESTRUCTOR, USING |
| `parser/temp_tables` | `bench_parser_temp_tables.abl` | DEFINE TEMP-TABLE, DEFINE BUFFER |
| `parser/procs_funcs` | `bench_parser_procs_funcs.abl` | PROCEDURE, FUNCTION definitions with parameters and bodies |

**Pattern** (same as lexer):
```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use std::fs;
use std::path::Path;

fn load_fixture(name: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent().unwrap().parent().unwrap()
        .join("resources").join(name);
    fs::read_to_string(path).unwrap()
}

fn bench_fixture(group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>, name: &str, fixture: &str) {
    let source = load_fixture(fixture);
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function(name, |b| {
        b.iter(|| {
            let tokens = tokenize(black_box(&source));
            let mut parser = Parser::new(&tokens, &source);
            parser.parse_program()
        })
    });
}

fn parser_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");
    bench_fixture(&mut group, "full_program", "bench_keywords.abl");
    bench_fixture(&mut group, "expressions", "bench_parser_expressions.abl");
    bench_fixture(&mut group, "declarations", "bench_parser_declarations.abl");
    bench_fixture(&mut group, "control_flow", "bench_parser_control_flow.abl");
    bench_fixture(&mut group, "oo_abl", "bench_parser_oo_abl.abl");
    bench_fixture(&mut group, "temp_tables", "bench_parser_temp_tables.abl");
    bench_fixture(&mut group, "procs_funcs", "bench_parser_procs_funcs.abl");
    group.finish();
}
```

### Additional Lexer Benchmarks (`crates/oxabl_lexer/benches/lexer_bench.rs`)

4 new benchmarks added to the existing file in the `"lexer"` group (same group as the consolidated keywords benchmark):

| Benchmark ID | Fixture File | What It Measures |
|---|---|---|
| `lexer/strings` | `bench_lexer_strings.abl` | String-heavy: concatenation, tilde escapes, quoted strings |
| `lexer/comments` | `bench_lexer_comments.abl` | Comment-heavy: block comments, line comments, interspersed code |
| `lexer/numeric` | `bench_lexer_numeric.abl` | Numeric literals: integers, decimals, mixed with expressions |
| `lexer/preprocessor` | `bench_lexer_preprocessor.abl` | Preprocessor: &IF, &SCOPED-DEFINE, {&references} |

Also **consolidate existing benchmarks**: the current `tokenize_keywords` and `tokenize_full` (aka `tokenize_with_stats`) both benchmark the same file. Merge into a single `lexer/keywords` benchmark and remove the `println!` from the timing path.

### Source Map Benchmarks (`crates/oxabl_common/benches/source_map_bench.rs`)

2 benchmarks in a `"source_map"` group, separated by operation:

| Benchmark ID | What It Measures |
|---|---|
| `source_map/construction` | `SourceMap::new()` on a large source (use `bench_keywords.abl`, 633 lines) |
| `source_map/lookup` | `SourceMap::lookup()` with pre-built map, querying multiple offsets per iteration |

For lookup, pre-build the `SourceMap` outside the timing loop, then benchmark lookups at various offsets (beginning, middle, end of file) to exercise different binary search paths.

### Fixture File Design

**Naming convention**: `bench_{crate}_{category}.abl` (e.g., `bench_parser_expressions.abl`, `bench_lexer_strings.abl`)

**Critical constraint**: Parser fixture files must **only contain constructs the parser currently supports**. The CLAUDE.md "Not yet implemented" list is the negative constraint — avoid: streams, frames, database manipulation (CREATE/DELETE/RELEASE), DATASET, preprocessor statements, PUBLISH/SUBSCRIBE, ON triggers.

**Size**: ~50-100 lines focused per category. The existing `bench_keywords.abl` (633 lines) serves as the large full-program benchmark. Note that `bench_keywords.abl` contains unsupported constructs — this is acceptable for the full-program benchmark since it exercises error recovery paths too, which is a realistic workload.

**Content guidelines per fixture**:
- `bench_parser_expressions.abl`: `assign` statements with arithmetic, comparison, logical, ternary, function call, and nested expressions. No variable declarations needed (parser doesn't do name resolution).
- `bench_parser_declarations.abl`: DEFINE VARIABLE/VAR/PARAMETER with all supported data types (INTEGER, CHARACTER, DECIMAL, LOGICAL, DATE, DATETIME, INT64, HANDLE, etc.), NO-UNDO, INITIAL, EXTENT.
- `bench_parser_control_flow.abl`: Nested DO/IF/REPEAT/FOR EACH/CASE blocks, LEAVE, NEXT, RETURN.
- `bench_parser_oo_abl.abl`: CLASS with INHERITS/IMPLEMENTS, METHOD with modifiers, DEFINE PROPERTY (auto + computed), CONSTRUCTOR, DESTRUCTOR, INTERFACE, USING.
- `bench_parser_temp_tables.abl`: DEFINE TEMP-TABLE with fields, indexes, LIKE; DEFINE BUFFER.
- `bench_parser_procs_funcs.abl`: PROCEDURE/FUNCTION definitions with INPUT/OUTPUT/INPUT-OUTPUT parameters, RETURNS, FORWARD, bodies with mixed statements.
- `bench_lexer_strings.abl`: String assignments, concatenation with `+`, tilde escapes (`~n`, `~t`, `~"`), single and double quoted strings.
- `bench_lexer_comments.abl`: Block comments (`/* */`), line comments (`//`), comments between code lines, large block comments.
- `bench_lexer_numeric.abl`: Integer literals, decimal literals, negative numbers, arithmetic expressions heavy on numeric tokens.
- `bench_lexer_preprocessor.abl`: `&IF`/`&THEN`/`&ENDIF`, `&SCOPED-DEFINE`, `&GLOBAL-DEFINE`, `{&variable}` references, `&MESSAGE`, nested preprocessor.

## Technical Considerations

- **Throughput reporting**: All benchmarks use `Throughput::Bytes(source.len() as u64)` for MB/s comparison across benchmarks, consistent with existing lexer pattern.
- **Criterion defaults are fine**: CodSpeed simulation mode ignores wall-clock settings; for local runs, Criterion's default 5s measurement handles small files adequately.
- **No parse-only benchmarks initially**: Tokenize+parse end-to-end is the realistic cost. The lexer is benchmarked separately, so regressions in either layer surface independently (see brainstorm: resolved question #1).
- **Error recovery in full-program benchmark**: `bench_keywords.abl` contains unsupported constructs. This is intentional — it benchmarks the parser's real-world behavior including error recovery. Category fixtures avoid this by using only supported constructs.

## Acceptance Criteria

- [x] `cargo bench -p oxabl_parser` runs 7 parser benchmarks with throughput reporting
- [x] `cargo bench -p oxabl_lexer` runs 5 lexer benchmarks (1 consolidated + 4 new) with throughput reporting
- [x] `cargo bench -p oxabl_common` runs 2 source map benchmarks with throughput reporting
- [ ] All fixture files parse without errors (except `bench_keywords.abl` for full-program)
- [ ] CodSpeed CI (`cargo codspeed build && cargo codspeed run`) discovers and runs all benchmarks
- [x] Existing lexer benchmark redundancy resolved (2 duplicate benchmarks → 1)
- [x] `cargo test` still passes for all crates (benchmarks don't break anything)

## Dependencies & Risks

- **No new dependencies**: `codspeed-criterion-compat` is already used; just needs adding to 2 more `Cargo.toml` files
- **Risk: fixture files exercising error recovery**: If a fixture accidentally includes unsupported constructs, the benchmark measures recovery paths instead of real parsing. Mitigation: run `parse_program()` on each fixture in a test and assert zero parse errors.
- **Risk: benchmark noise on small files**: 50-line files may tokenize+parse in <10us. Mitigation: Criterion handles this with adaptive iteration counts; CodSpeed uses instruction counting anyway.

## Implementation Order

1. **Cargo.toml changes** — add criterion dev-dep and `[[bench]]` to parser and common crates
2. **Fixture files** — write all 10 new `.abl` files in `resources/`
3. **Parser bench file** — `crates/oxabl_parser/benches/parser_bench.rs` with 7 benchmarks
4. **Lexer bench cleanup + additions** — consolidate existing 2→1, add 4 new in `crates/oxabl_lexer/benches/lexer_bench.rs`
5. **Source map bench file** — `crates/oxabl_common/benches/source_map_bench.rs` with 2 benchmarks
6. **Validation** — `cargo bench` locally, verify CodSpeed discovery, add fixture-validity tests

## Sources

- **Origin brainstorm:** [docs/brainstorms/2026-04-04-benchmark-expansion-brainstorm.md](docs/brainstorms/2026-04-04-benchmark-expansion-brainstorm.md) — key decisions: tokenize+parse end-to-end, dedicated fixture files, category-level granularity, Criterion groups
- **Existing benchmark:** `crates/oxabl_lexer/benches/lexer_bench.rs` — template for all new benchmarks
- **Parser API:** `crates/oxabl_parser/src/parser/mod.rs` — `Parser::new()`, `parse_program()`
- **Source map API:** `crates/oxabl_common/src/source_map.rs` — `SourceMap::new()`, `lookup()`
- **Performance learnings:** `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md` — methodology for measuring perf wins
