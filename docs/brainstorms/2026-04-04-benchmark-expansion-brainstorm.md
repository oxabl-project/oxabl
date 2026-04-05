# Benchmark Expansion Brainstorm

**Date:** 2026-04-04
**Status:** Final

## What We're Building

Expanded benchmark suite covering the lexer, parser, and source map to:
1. **Catch regressions** via CodSpeed as the tokenizer and parser grow
2. **Identify optimization targets** by isolating hot paths per construct category

Currently only `oxabl_lexer` has benchmarks (2 benchmarks, same test file). The parser has 262 tests but zero performance tracking.

## Why This Approach

- **Dedicated fixture files** in `resources/` for each scenario — realistic ABL code, easy to maintain, isolates construct-specific performance
- **Key category grouping** (~5-7 parser benchmarks) — enough granularity to pinpoint regressions without benchmark maintenance overhead
- **Same Criterion/CodSpeed setup** already proven in the lexer — no new dependencies or CI changes needed

## Proposed Benchmarks

### Parser Benchmarks (`crates/oxabl_parser/benches/parser_bench.rs`)

| Benchmark | Fixture File | What It Measures |
|---|---|---|
| `parse_full_program` | `bench_keywords.abl` (existing) | End-to-end `parse_program()` on comprehensive ABL |
| `parse_expressions` | `bench_expressions.abl` | Arithmetic, comparison, logical, ternary, function calls, nested expressions |
| `parse_variable_declarations` | `bench_declarations.abl` | DEFINE VARIABLE, VAR, PARAMETER with various data types and options |
| `parse_control_flow` | `bench_control_flow.abl` | DO, IF/THEN/ELSE, REPEAT, FOR EACH, CASE with nesting |
| `parse_oo_abl` | `bench_oo_abl.abl` | CLASS, INTERFACE, METHOD, PROPERTY, CONSTRUCTOR, DESTRUCTOR, USING |
| `parse_temp_tables_buffers` | `bench_temp_tables.abl` | DEFINE TEMP-TABLE, DEFINE BUFFER, DATASET |
| `parse_procedures_functions` | `bench_procs_funcs.abl` | PROCEDURE, FUNCTION definitions with parameters and bodies |

### Additional Lexer Benchmarks (`crates/oxabl_lexer/benches/lexer_bench.rs`)

| Benchmark | Fixture File | What It Measures |
|---|---|---|
| `tokenize_strings` | `bench_strings.abl` | String-heavy code: concatenation, tilde escapes, substitutions |
| `tokenize_comments` | `bench_comments.abl` | Comment-heavy files: block comments, line comments, nested |
| `tokenize_numeric` | `bench_numeric.abl` | Numeric literals: integers, decimals, scientific notation |
| `tokenize_preprocessor` | `bench_preprocessor.abl` | Preprocessor directives: &IF, &SCOPED-DEFINE, {&references} |

### Source Map Benchmark (`crates/oxabl_common/benches/source_map_bench.rs`)

| Benchmark | What It Measures |
|---|---|
| `source_map_lookup` | Line/column resolution via binary search on large files (use `bench_keywords.abl` or generate a large synthetic source) |

## Key Decisions

1. **One bench file per crate** — `parser_bench.rs`, extended `lexer_bench.rs`, `source_map_bench.rs`
2. **Dedicated `.abl` fixture files** in `resources/` per scenario
3. **Category-level granularity** for parser (~7 benchmarks), not per-function
4. **Criterion throughput measurement** (bytes/sec) on all benchmarks for consistency with existing lexer benchmarks
5. **Reuse existing `bench_keywords.abl`** for full-program parser benchmark — no need to duplicate
6. **Parser benchmarks include tokenization time** — measures realistic end-to-end cost (tokenize + parse), not parse-only with pre-tokenized input

## Resolved Questions

1. **Parser benchmark isolation**: Tokenize+parse (realistic end-to-end). The lexer is already benchmarked separately, so regressions in either layer will still surface.
2. **Fixture file size**: ~50-100 lines focused per construct category. The existing `bench_keywords.abl` (632 lines) serves as the large full-program benchmark.
3. **Benchmark groups**: Yes, use Criterion benchmark groups (e.g., `parser/expressions`, `lexer/strings`) for clean CodSpeed display and cross-benchmark comparison.

## Next Steps

- Create fixture `.abl` files for each benchmark scenario
- Add `codspeed-criterion-compat` dependency to `oxabl_parser` and `oxabl_common`
- Implement benchmark files
- Verify CodSpeed picks up new benchmarks automatically (it should via `cargo codspeed build`)
