---
title: "Heap allocation in match_keyword() on every token"
category: performance-issues
date: 2026-04-04
tags: [lexer, codegen, allocation, ascii, match_keyword]
related_prs: ["#17", "#19"]
---

# Heap allocation in match_keyword() on every token

## Problem

`match_keyword()` called `s.to_lowercase()` on every token, producing a heap-allocated `String` per identifier. Since ABL is keyword-heavy, this hit on nearly every token in a source file. The lock-type detection path (`try_read_space_separated_lock`) had the same issue.

**Symptom:** ~20% of tokenization time spent on unnecessary heap allocations in keyword matching.

## Root Cause

The codegen template (`oxabl_codegen/src/main.rs`, `generate_keyword_match()`) emitted `let lower = s.to_lowercase();` ��� a Unicode-aware lowering that heap-allocates. ABL keywords are ASCII-only, so Unicode support was unnecessary overhead.

Similarly, `try_read_space_separated_lock()` in `lib.rs` called `first_word.to_lowercase()` before matching against `"no"`, `"share"`, `"exclusive"`.

## Solution

### 1. Stack-buffer ASCII fold in match_keyword()

Replace `s.to_lowercase()` with a fixed `[u8; 64]` stack buffer:

```rust
const MAX_KEYWORD_LEN: usize = 64;
let bytes = s.as_bytes();
if bytes.len() > MAX_KEYWORD_LEN {
    return None;
}
let mut buf = [0u8; MAX_KEYWORD_LEN];
for (i, &b) in bytes.iter().enumerate() {
    buf[i] = b.to_ascii_lowercase();
}
let lower = unsafe { std::str::from_utf8_unchecked(&buf[..bytes.len()]) };
```

Key details:
- `to_ascii_lowercase()` is branchless on modern CPUs (conditional move), only folds `A-Z`, passes everything else through unchanged
- The longest ABL keyword is 20 bytes; 64-byte buffer provides 3x headroom
- Inputs > 64 bytes return `None` immediately (cannot be a keyword)
- Codegen emits a compile-time assertion: `const _: () = assert!(LONGEST <= 64);`
- `from_utf8_unchecked` is safe because `to_ascii_lowercase()` preserves UTF-8 validity

### 2. Direct eq_ignore_ascii_case for lock types

Replace `to_lowercase()` + match with zero-allocation comparisons:

```rust
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

### Result

~20% overall performance improvement in benchmarks.

## Prevention

- **Never use `to_lowercase()` or `to_uppercase()` on hot paths in the lexer.** Use `to_ascii_lowercase()` on bytes or `eq_ignore_ascii_case()` for comparisons.
- **The codegen owns `match_keyword()`.** Performance fixes go into the codegen template (`generate_keyword_match()` in `main.rs`), not into the generated `kind.rs` directly.
- **Parser should dispatch on `Kind` variants, not string comparisons.** If the parser needs a new keyword, add it to `keyword_overrides.toml` and regenerate. See PR #17 for the established pattern (~8% improvement from that migration).

## Compounding Principle

Two sequential optimizations to the same pipeline:
1. PR #17: Migrate parser string comparisons to Kind-based dispatch (~8% parsing improvement)
2. PR #19: Eliminate lexer `to_lowercase()` heap allocation (~20% overall improvement)

The pattern: **push classification as far upstream as possible** (into the lexer/codegen), so downstream consumers (parser) do zero string work. Each layer of precision compounds.
