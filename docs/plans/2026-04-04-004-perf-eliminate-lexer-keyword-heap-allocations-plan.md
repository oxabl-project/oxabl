---
title: "perf: Eliminate lexer keyword heap allocations"
type: refactor
status: completed
date: 2026-04-04
origin: docs/brainstorms/2026-04-04-lexer-runtime-allocation-removal-brainstorm.md
---

# perf: Eliminate lexer keyword heap allocations

## Overview

Replace `to_lowercase()` heap allocations in the lexer's keyword matching hot path with stack-based ASCII case folding. Three targeted changes — one codegen template, one lexer function, one parser function.

## Acceptance Criteria

- [x] `match_keyword()` in generated `kind.rs` uses a `[u8; 64]` stack buffer with `b.to_ascii_lowercase()` instead of `s.to_lowercase()`
- [x] Generated code includes a compile-time assertion that the longest keyword fits in the buffer
- [x] Inputs exceeding 64 bytes return `None` immediately (cannot be a keyword)
- [x] `try_read_space_separated_lock()` in `lib.rs` uses `eq_ignore_ascii_case()` directly — no `to_lowercase()` allocation
- [x] `parse_procedure_name()` in `statements.rs` uses byte-length + byte-value matching instead of `eq_ignore_ascii_case()` for single-char extensions
- [x] All 37 lexer tests pass
- [x] All 239 parser tests pass
- [x] No new dependencies introduced
- [x] Codegen regenerated: `cargo run -p oxabl_codegen -- kind`

## Implementation

### Step 1: Codegen template — `match_keyword()` stack buffer

**File:** `crates/oxabl_codegen/src/main.rs`, function `generate_keyword_match()` (line 690)

Replace the emitted `to_lowercase()` call (line 700) with a stack-buffer fold:

```rust
// Emitted into kind.rs by codegen:
pub fn match_keyword(s: &str) -> Option<Kind> {
    let bytes = s.as_bytes();
    if bytes.len() > 64 {
        return None;
    }
    let mut buf = [0u8; 64];
    for (i, &b) in bytes.iter().enumerate() {
        buf[i] = b.to_ascii_lowercase();
    }
    let lower = unsafe { std::str::from_utf8_unchecked(&buf[..bytes.len()]) };
    match lower {
        // ... existing match arms unchanged ...
    }
}
```

Note: `from_utf8_unchecked` is safe here because the input `s` is already valid UTF-8 and `to_ascii_lowercase()` preserves UTF-8 validity. Alternatively, use `std::str::from_utf8(&buf[..bytes.len()]).unwrap()` if the team prefers avoiding `unsafe` — the branch is trivially eliminated by the compiler since the invariant holds.

Also emit a compile-time assertion after the function:

```rust
const _: () = assert!(LONGEST_KEYWORD <= 64);
```

Where `LONGEST_KEYWORD` is computed by codegen from the keyword list.

**Run:** `cargo run -p oxabl_codegen -- kind` to regenerate.

### Step 2: Lock-type detection — drop `to_lowercase()`

**File:** `crates/oxabl_lexer/src/lib.rs`, function `try_read_space_separated_lock()` (line 332)

Replace lines 334-339:

```rust
// Before:
let first_lower = first_word.to_lowercase();
let lock_kind = match first_lower.as_str() {
    "no" => Kind::NoLock,
    "share" => Kind::ShareLock,
    "exclusive" => Kind::ExclusiveLock,
    _ => return None,
};

// After:
let lock_kind = if first_word.eq_ignore_ascii_case("no") {
    Kind::NoLock
} else if first_word.eq_ignore_ascii_case("share") {
    Kind::ShareLock
} else if first_word.eq_ignore_ascii_case("exclusive") {
    Kind::ExclusiveLock
} else {
    return None;
};
```

The existing `eq_ignore_ascii_case("lock")` check on line 368 is already correct — no change needed.

### Step 3: Parser file extension check — byte matching

**File:** `crates/oxabl_parser/src/parser/statements.rs`, function `parse_procedure_name()` (lines 1556-1560)

Replace the `eq_ignore_ascii_case` chain with a match on byte length and values:

```rust
// Before:
if ext.eq_ignore_ascii_case("p")
    || ext.eq_ignore_ascii_case("w")
    || ext.eq_ignore_ascii_case("r")
    || ext.eq_ignore_ascii_case("i")
    || ext.eq_ignore_ascii_case("cls")

// After:
let ext_bytes = ext.as_bytes();
if match ext_bytes.len() {
    1 => matches!(ext_bytes[0] | 0x20, b'p' | b'w' | b'r' | b'i'),
    3 => ext.eq_ignore_ascii_case("cls"),
    _ => false,
}
```

This is principled cleanup (parser avoids string comparison where possible per CLAUDE.md), not a performance-critical change — `parse_procedure_name` runs once per RUN statement.

### Step 4: Validate

```bash
cargo run -p oxabl_codegen -- kind   # regenerate kind.rs
cargo test                            # all tests pass
cargo clippy -- -D warnings           # no new warnings
```

## Context

- **Why stack buffer:** ABL keywords are ASCII-only, max ~25 chars. A `[u8; 64]` buffer is 3x headroom with zero heap allocation. (see brainstorm: docs/brainstorms/2026-04-04-lexer-runtime-allocation-removal-brainstorm.md)
- **Why `b.to_ascii_lowercase()`:** Branchless on modern CPUs (conditional move). Safe for all byte values — only folds `A-Z`, passes everything else through unchanged. Preferred over raw `| 0x20` which is correct for current keywords but fragile.
- **Prior art:** Plan 003 (`docs/plans/2026-04-04-003-refactor-migrate-parser-keyword-workarounds-plan.md`) eliminated `to_uppercase()` in `parse_data_type()` for an ~8% parsing improvement using the same principle.
- **Dead code noted:** `match_keyword()` call at `lib.rs:471` (preprocessor references) never matches — the input includes `{&...}` delimiters. Left as-is; tracked for separate cleanup.

## Sources

- **Origin brainstorm:** [docs/brainstorms/2026-04-04-lexer-runtime-allocation-removal-brainstorm.md](docs/brainstorms/2026-04-04-lexer-runtime-allocation-removal-brainstorm.md)
- Codegen template: `crates/oxabl_codegen/src/main.rs:690` (`generate_keyword_match()`)
- Generated output: `crates/oxabl_lexer/src/kind.rs:540` (`match_keyword()`)
- Lock detection: `crates/oxabl_lexer/src/lib.rs:332` (`try_read_space_separated_lock()`)
- Parser extension check: `crates/oxabl_parser/src/parser/statements.rs:1537` (`parse_procedure_name()`)
- CLAUDE.md lexer performance principle
