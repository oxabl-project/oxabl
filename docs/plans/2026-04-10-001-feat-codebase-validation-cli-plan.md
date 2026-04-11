---
title: "feat: Codebase Validation CLI"
type: feat
status: completed
date: 2026-04-10
---

# feat: Codebase Validation CLI

## Overview

Add a CLI binary to oxabl that recursively scans a directory for ABL source files, parses each one, and produces a summary report of what parsed successfully and what failed. This serves as a real-world validation tool to identify gaps in the parser by running it against a large legacy codebase.

## Problem Statement / Motivation

The parser is reaching a maturity level where it can handle simple ABL files, but there's no way to measure coverage against real-world code. The organization collaborating on this project has a large legacy ABL codebase that serves as the ideal test corpus. A CLI tool that can be pointed at a directory and produce a pass/fail report will:

1. Quantify exactly what percentage of real files the parser handles today
2. Identify the specific constructs/patterns causing failures
3. Provide a feedback loop for prioritizing parser improvements
4. Serve as a regression test as the parser evolves

## Proposed Solution

A binary target in the `oxabl` crate that accepts a path, discovers ABL files, parses each one, and prints a structured report.

### Usage

```bash
# Scan a directory
oxabl check /path/to/abl/codebase

# Scan a single file
oxabl check /path/to/file.p

# JSON output for tooling
oxabl check /path/to/codebase --json
```

### Example Output

```
Scanning /path/to/abl/codebase...
Found 2,847 ABL files

[========================================] 2847/2847

Results: 1,923 passed, 924 failed (67.5% success rate)

Failures:
  src/ap/ap-main.p:142:18  Unexpected token 'FIND'
  src/ap/ap-aging.w:87:5   Unexpected token 'DEFINE TEMP-TABLE'
  src/gl/gl-post.p:203:12  Unexpected token 'FOR'
  ... (921 more)

Top error patterns:
  412  Unexpected token 'FIND'
  198  Unexpected token 'DEFINE TEMP-TABLE'
  156  Unexpected token 'FOR'
   89  Unexpected token 'CLASS'
   69  Other

Total time: 3.2s (890 files/sec)
```

## Technical Approach

### Binary Target

Add a `[[bin]]` section to `crates/oxabl/Cargo.toml` with a `main.rs` entry point. The `oxabl` crate already depends on all sub-crates, making it the natural home.

### New Dependencies

| Crate | Purpose |
|-------|---------|
| `clap` | CLI argument parsing (derive API) |
| `walkdir` | Recursive directory traversal |
| `indicatif` | Progress bar for large codebases |
| `serde` + `serde_json` | JSON output mode |

### Architecture

```
main.rs
  └── cli args (clap)
        └── check command
              ├── discover_files(path, extensions) -> Vec<PathBuf>
              │     └── walkdir, filter by extension
              ├── parse_file(path) -> FileResult
              │     ├── fs::read_to_string (handle I/O errors)
              │     ├── tokenize(source)
              │     ├── Parser::new(&tokens, &source).parse_statements()
              │     └── SourceMap for error line/column
              ├── collect results + update progress bar
              └── render_report(results, format)
                    ├── summary stats
                    ├── failure list (sorted by path)
                    ├── top error patterns (grouped/counted)
                    └── timing data
```

### Key Types

```rust
enum FileResult {
    Success { path: PathBuf },
    ParseError { path: PathBuf, line: usize, col: usize, message: String },
    IoError { path: PathBuf, error: String },
    LexerPanic { path: PathBuf },
}
```

### File Discovery

Scan for extensions: `.p`, `.w`, `.i`, `.cls`, `.v` (case-insensitive matching since some systems may use `.P`, `.W`, etc.)

### Error Handling Strategy

- **I/O errors** (permission denied, non-UTF-8 content): Reported as `IoError`, counted separately from parse failures
- **Lexer panics**: Wrap `tokenize()` in `std::panic::catch_unwind` to prevent one bad file from aborting the entire run. Reported as `LexerPanic`
- **Parse errors**: The parser's existing `ParseError` with `Span` → converted to line/column via `SourceMap`
- **Empty files**: Parse as normal — `parse_statements()` on `[EOF]` returns `Ok(vec![])`, counted as a pass

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All files parsed successfully |
| 1 | One or more files failed to parse |
| 2 | Operational error (invalid path, no files found) |

### Output Formats

- **Default (human)**: Colored terminal output with progress bar, summary, failure list sorted by path, top error patterns grouped/counted, and timing
- **`--json`**: Machine-readable JSON for downstream tooling (CI, dashboards)

```json
{
  "total": 2847,
  "passed": 1923,
  "failed": 924,
  "io_errors": 3,
  "lexer_panics": 0,
  "success_rate": 67.5,
  "elapsed_secs": 3.2,
  "files_per_sec": 890,
  "failures": [
    { "path": "src/ap/ap-main.p", "line": 142, "col": 18, "message": "Unexpected token 'FIND'" }
  ],
  "error_patterns": [
    { "pattern": "Unexpected token 'FIND'", "count": 412 }
  ]
}
```

## Pre-requisite: Remove Debug Print Statements

There are 17 `println!` debug statements in the parser's non-test code that will flood stdout during batch processing. These must be removed before the CLI is usable:

- `crates/oxabl_parser/src/parser/statements.rs` — 10 debug println! calls (lines 13, 97, 102, 427, 430, 444, 449, 457, 463, 583, 585)
- `crates/oxabl_parser/src/parser/expressions.rs` — 7 debug println! calls (lines 58, 81, 84, 89, 91, 329, 330)

These should simply be removed (not gated behind a flag — they're development-time noise, not useful diagnostics).

## System-Wide Impact

- **No impact on existing crates**: The binary is additive — a new `main.rs` in the `oxabl` crate, which is currently just a lib facade
- **The `oxabl` crate gains a binary target** alongside its existing library target. Both coexist naturally in Cargo
- **New dependencies** (`clap`, `walkdir`, `indicatif`, `serde_json`) are only needed by the binary, not the library crates. They should be added to the `oxabl` crate's `Cargo.toml` only

## Acceptance Criteria

- [ ] `oxabl check <directory>` recursively finds all `.p`, `.w`, `.i`, `.cls`, `.v` files
- [ ] `oxabl check <file>` works on a single file
- [ ] Each file is tokenized and parsed; first error per file is captured
- [ ] Lexer panics are caught and reported without aborting the run
- [ ] I/O errors (permission denied, non-UTF-8) are reported separately
- [ ] Output includes: file count, pass/fail counts, success rate percentage
- [ ] Failed files are listed with path, line, column, and error message, sorted by path
- [ ] Top error patterns are grouped and counted
- [ ] Progress bar shows progress during scanning
- [ ] Total elapsed time and files/sec are reported
- [ ] `--json` flag produces machine-readable JSON output
- [ ] Exit code 0 when all pass, 1 when any fail, 2 for operational errors
- [ ] All 17 debug `println!` statements are removed from the parser
- [ ] Case-insensitive file extension matching (handles `.P`, `.W`, etc.)

## Implementation Phases

### Phase 1: Cleanup (pre-requisite)

Remove the 17 debug `println!` calls from `statements.rs` and `expressions.rs`. Run existing tests to confirm nothing breaks.

### Phase 2: Minimal CLI

- Add `clap` and `walkdir` dependencies to `crates/oxabl/Cargo.toml`
- Add `[[bin]]` target pointing to `src/main.rs`
- Implement file discovery and basic parse-and-report loop
- Human-readable output with summary + error list
- Exit codes
- Single file mode

### Phase 3: Polish

- Add `indicatif` for progress bar
- Add error pattern grouping/counting
- Add timing data
- Add `--json` output with `serde_json`
- Case-insensitive extension matching

## Dependencies & Risks

- **Risk: Lexer panics on malformed input** — Mitigated by `catch_unwind` wrapper. Should be verified against actual codebase files early
- **Risk: Performance on very large codebases** — Single-threaded should be sufficient for initial version (the lexer+parser are fast). Parallel parsing with `rayon` is a natural follow-up if needed but is out of scope
- **Risk: Non-UTF-8 files** — `fs::read_to_string` will error on these. Report as I/O error and move on

## Future Considerations (Out of Scope)

- Parallel parsing with `rayon` for performance
- `--filter` flag to scan only specific extensions
- `--exclude` patterns (e.g., skip `build/` directories)
- Watch mode for continuous validation during development
- Error recovery in the parser to report multiple errors per file
- Integration with CI pipelines

## Sources & References

- Existing parser API: `crates/oxabl_parser/src/parser/mod.rs:29-37`
- Existing lexer API: `crates/oxabl_lexer/src/lib.rs:23`
- SourceMap API: `crates/oxabl_common/src/source_map.rs:42`
- Existing binary example: `crates/oxabl_codegen/src/main.rs`
