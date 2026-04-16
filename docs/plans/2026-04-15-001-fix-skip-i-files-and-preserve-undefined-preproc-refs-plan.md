---
title: "fix: Skip .i files in check command and preserve undefined preprocessor references"
type: fix
status: active
date: 2026-04-15
---

# Skip .i files in check and preserve undefined preprocessor references

## Overview

Two related fixes that reduce false-positive parse failures when running `oxabl check` against real ABL codebases:

1. **Don't parse `.i` files standalone** — include files are parsed when the preprocessor expands them into their parent. Standalone `.i` files are often syntactically incomplete fragments.
2. **Preserve undefined `{&variable}` and `{N}` references** — when the preprocessor encounters an undefined variable or missing positional argument, keep the original source text instead of erasing it. The downstream lexer tokenizes them as `Kind::Preprop` / `Kind::IncludeArgReference`, which the parser already handles in all positions.

## Acceptance Criteria

- [ ] `ABL_EXTENSIONS` in `crates/oxabl/src/main.rs` no longer includes `"i"`
- [ ] Undefined `{&variable}` references are preserved in preprocessor output (not erased)
- [ ] Missing positional `{N}` references are preserved in preprocessor output (not erased)
- [ ] Existing tests pass
- [ ] New preprocessor tests verify undefined references are preserved

## Technical Approach

### Change 1: Remove `.i` from `ABL_EXTENSIONS`

**File:** `crates/oxabl/src/main.rs:17`

```rust
// Before
const ABL_EXTENSIONS: &[&str] = &["p", "w", "i", "cls", "v"];
// After
const ABL_EXTENSIONS: &[&str] = &["p", "w", "cls", "v"];
```

### Change 2: Preserve undefined references in preprocessor

**File:** `crates/oxabl_preprocessor/src/preprocessor.rs`

**Lines 334-344 (undefined `{&variable}`):** Instead of emitting the chunk-before and skipping the reference, just advance `i = ref_end` without touching `chunk_start`. This leaves the `{&name}` bytes inside the current chunk.

**Lines 450-460 (missing positional `{N}`):** Same approach — advance past the reference without breaking the chunk.

In both cases the fix is: remove the chunk-emit + chunk_start reset, keep only `i = ref_end; continue;`.
