---
title: "perf: Fix parser/lexer performance regressions identified via CodSpeed (2026-04-11)"
type: perf
status: completed
date: 2026-04-11
---

# perf: Fix parser/lexer performance regressions identified via CodSpeed

## Overview

Four parser benchmarks regressed significantly today after a high-velocity batch of
feature commits. CodSpeed flamegraph analysis identified four distinct bottlenecks.
Two are critical and easy to fix (token Vec reallocation, AssignPair SmallVec). Two
are medium-effort improvements (expression tower fast-path, PHF keyword matching).
This plan addresses them in priority order.

Measured regressions vs yesterday's baseline (hardware-corrected estimates):

| Benchmark | Baseline | Current | Δ |
|-----------|----------|---------|---|
| `full_program` | ~666 µs | ~1,046 µs | **−36%** |
| `expressions` | ~274 µs | ~389 µs | **−29%** |
| `oo_abl` | ~242 µs | ~307 µs | **−21%** |
| `procs_funcs` | ~259 µs | ~315 µs | **−18%** |

Note: all four parser benchmarks are affected; lexer-only and source-map benchmarks
are unaffected, confirming this is parser-path overhead.

## Problem Statement

### P1 — Token Vec reallocation (all benchmarks, highest impact)

`tokenize()` in `crates/oxabl_lexer/src/lib.rs:25` initialises the token Vec with
`Vec::new()`, meaning it starts with capacity 0 and grows via amortised doubling.
For any real source file the Vec must grow several times, each growth triggering a
`realloc` syscall. When the Vec is large enough that `realloc` must extend the heap,
libc calls `mmap2` — measured in the flamegraphs as `Unknown symbol (0x188a40)` from
`libc.so.6`, costing **18–24% of total benchmark time** across all four benchmarks.

The problem worsened today because commit `69d9b5e5` added `Kind::Comment` tokens to
the live token stream (previously discarded). Source files with comments now produce
significantly more tokens, exhausting the initial doubling budget sooner and requiring
an extra `mmap` growth step.

Flamegraph evidence:
- `expressions`: `RawVec::grow_one` 31.5 µs (early today) → 92.8 µs (latest) — tripled
- `procs_funcs`: `RawVec::grow_one` = 73.9 µs = **23.4%** of total
- `oo_abl`: `RawVec::grow_one` = 67.7 µs = **22.0%** of total

### P2 — AssignPair list heap allocation (expressions benchmark)

`parse_assign_pairs()` in `statements.rs:3068` allocates a `Vec<AssignPair>` per
ASSIGN statement. The `expressions` benchmark fixture is dominated by ASSIGN
statements. Dropping the AST at the end of each benchmark iteration calls
`drop_in_place::<Vec<AssignPair>>`, which was measured at **23.5 µs = 8.5%** of the
`expressions` total. Each `Vec` has a separate heap allocation even for the common
1–4 pair case.

The `AssignPair` struct (`crates/oxabl_ast/src/statement.rs:769`) stores two
`Expression` values; the `Statement::Assign` variant stores `assignments:
Vec<AssignPair>`.

### P3 — Expression entry point depth in parse_assign_pairs (expressions benchmark)

The call tree in `expressions` changed today. `parse_assign_pairs` now calls
`self.parse_expression()` for the value (line 3073), which enters the full descent:
`parse_ternary → parse_or → parse_and → parse_comparison → parse_additive → ...`

The early-today flamegraph showed `parse_assign_pairs → parse_additive` directly (76.7
µs). The latest shows `parse_assign_pairs → parse_ternary → [full tower]` (104.6 µs).
The additional 4 function levels add overhead via the `?`/`branch<Expression,
ParseError>` return-path cost, which appears 32 times in the flamegraph at 8.2 µs
total (2.1%). For the expressions benchmark with many ASSIGN statements this multiplies.

This is architecturally correct behaviour — ASSIGN values can be ternary expressions.
The overhead is from the Rust `Result` propagation at each level, not wrong logic.

### P4 — match_keyword byte comparison cost (full_program benchmark)

`match_keyword` in `crates/oxabl_lexer/src/kind.rs:608` was already optimised via the
stack-buffer ASCII fold in PR #19. However it still consumes **206 µs = 19.7%** of the
`full_program` benchmark, with `equal<u8,u8>` (57 µs) and `eq<u8,u8>` (31 µs)
dominating within it. Today's batch of commits added many `can_be_identifier()` variants
and new statement keywords, growing the `match` function and increasing per-keyword
comparison work.

The current approach (stack fold → `match lower.len() { 1 => { ... }, 2 => { ... }, ...
}`) is a hand-dispatched table. Within each length bucket the compiler emits a sequence
of byte-slice equality checks (the `equal`/`eq` calls). A perfect hash function (PHF)
would reduce each lookup to a single hash computation + one equality check.

## Proposed Solution

### Fix 1 — Pre-allocate token Vec with capacity hint

**File:** `crates/oxabl_lexer/src/lib.rs:25`

**Change:**
```rust
// Before
let mut tokens = Vec::new();

// After
let mut tokens = Vec::with_capacity(source.len() / 5);
```

A ratio of ~1 token per 5 source bytes is a conservative lower bound for ABL. Most
files will produce 1 token per 5–8 bytes, so this hint either hits exactly or
over-allocates slightly — both are better than the current 0-capacity start.

Note: commits `0829044` and `f6914bf` (2026-04-12) added `self.chars.clone()` lookahead
calls for trailing-minus detection and line-continuation handling. These add a small
amount of per-character work in the lexer hot path, making the Vec reallocation overhead
proportionally larger and further increasing Fix 1's priority.

The divisor should be tuned by measuring with the benchmark fixtures:
- Instrument tokenize() to print `tokens.len()` and `source.len()` for each benchmark
  file, compute the actual ratio, then set the hint to the 10th-percentile ratio
  (most conservative estimate) to avoid wasteful over-allocation.

A reasonable range to test: `source.len() / 4` through `source.len() / 8`.

**Expected impact:** Eliminates 2–3 `realloc` calls per tokenize invocation, recovering
most of the 18–24% overhead seen in the flamegraphs. Estimated recovery: **15–22% on
all four benchmarks.**

### Fix 2 — SmallVec for AssignPair list

**File:** `crates/oxabl_ast/src/statement.rs` (Statement::Assign variant)
**File:** `crates/oxabl_parser/src/parser/statements.rs:3068` (parse_assign_pairs)

Add `smallvec` to `oxabl_ast/Cargo.toml`:
```toml
smallvec = { version = "1", features = ["union"] }
```

Change the AST type:
```rust
// crates/oxabl_ast/src/statement.rs
use smallvec::SmallVec;

// In Statement::Assign:
Assign {
    assignments: SmallVec<[AssignPair; 4]>,
},
```

Change the parser:
```rust
// crates/oxabl_parser/src/parser/statements.rs:3068
fn parse_assign_pairs(&mut self) -> ParseResult<SmallVec<[AssignPair; 4]>> {
    let mut assignments = SmallVec::new();
    // ... rest unchanged
}
```

An inline capacity of 4 covers the vast majority of real ASSIGN statements without any
heap allocation. For the rare ASSIGN with 5+ pairs, `SmallVec` spills to the heap
transparently.

**Expected impact:** Eliminates the heap allocation + drop cost for ~95% of ASSIGN
statements. Estimated recovery: **5–8% on `expressions` benchmark.**

### Fix 3 — Ternary fast-path in parse_assign_pairs

**File:** `crates/oxabl_parser/src/parser/statements.rs:3073`

The `Result` propagation overhead from 4 extra function levels is small per call but
multiplied across all assignment values in a benchmark. A cheap peek eliminates it for
the common (non-ternary) case:

```rust
// In parse_assign_pairs, replace:
let value = self.parse_expression()?;

// With:
// Fast path: skip the ternary/or/and levels unless KwIf is next
let value = if self.check(Kind::KwIf) {
    self.parse_expression()?
} else {
    self.parse_additive()?  // covers the vast majority of ASSIGN values
};
```

**Correctness concern:** `parse_additive` handles arithmetic, string concat, and most
expressions. It does NOT handle `OR`/`AND`/comparisons. Real ASSIGN values can be
`target = (a > b)` or `target = a AND b`. The fast path must be expanded to at least
`parse_comparison` or `parse_ternary` with a non-`KwIf` skip.

A safer fast path: enter at `parse_comparison` for the non-ternary case:

```rust
let value = if self.check(Kind::KwIf) {
    self.parse_expression()?  // full ternary tower
} else {
    self.parse_comparison()?  // covers a > b, a AND b, etc.
};
```

This skips only the `parse_ternary` level for non-ternary values, reducing call depth
by 1 level. Low risk, small gain.

**Expected impact:** **~3–5% on `expressions`.** Verify by running the benchmark before
and after; if not measurable in simulation mode, consider skipping as noise.

### Fix 4 — Perfect hash for match_keyword (long-term)

**Files:** `crates/oxabl_codegen/src/main.rs` (generate_keyword_match function)

Replace the current hand-dispatched length + byte-slice match with a compile-time
perfect hash function using the `phf` crate:

```toml
# In crates/oxabl_lexer/Cargo.toml
phf = { version = "0.11", features = ["macros"] }
```

In the codegen template, emit:
```rust
use phf::phf_map;

static KEYWORDS: phf::Map<&'static str, Kind> = phf_map! {
    "define" => Kind::Define,
    "def"    => Kind::Define,
    // ... all keywords and their abbreviation expansions
};

pub fn match_keyword(s: &str) -> Option<Kind> {
    const MAX_KEYWORD_LEN: usize = 64;
    let bytes = s.as_bytes();
    if bytes.len() > MAX_KEYWORD_LEN {
        return None;
    }
    let mut buf = [0u8; MAX_KEYWORD_LEN];
    for (i, &b) in bytes.iter().enumerate() {
        buf[i] = b.to_ascii_lowercase();
    }
    // SAFETY: to_ascii_lowercase preserves UTF-8
    let lower = unsafe { std::str::from_utf8_unchecked(&buf[..bytes.len()]) };
    KEYWORDS.get(lower).copied()
}
```

The `phf_map!` macro generates a compile-time perfect hash at build time. Each lookup
is O(1): one hash computation + one equality check, regardless of how many keywords
exist. The current approach does O(n/length_bucket) comparisons.

**Tradeoffs:**
- Introduces a `phf` dependency
- Codegen becomes more complex (must emit PHF syntax instead of match arms)
- Build times may increase slightly due to the compile-time hash computation
- BUT: `match_keyword` is called on every token in every file — a 20% improvement here
  is a 20% win on total lexer time

**Expected impact:** **5–10% on `full_program`**. Lower relative impact on smaller
benchmarks where tokenization is a smaller fraction. Given the ~20% current spend in
`match_keyword`, a PHF could recover ~half of that.

**Note:** This is a larger refactor and should be done as a separate PR after Fixes 1–3
are shipped and validated. It changes the codegen significantly.

## Implementation Phases

### Phase 1: Quick wins (Fixes 1 & 2) — one PR

These are mechanical, low-risk changes that can land together.

**Tasks:**

- [ ] `crates/oxabl_lexer/src/lib.rs:25` — Replace `Vec::new()` with
  `Vec::with_capacity(source.len() / 5)` (tune ratio empirically if needed)
- [ ] `crates/oxabl_ast/Cargo.toml` — Add `smallvec = { version = "1", features = ["union"] }`
- [ ] `crates/oxabl_ast/src/statement.rs` — Change `Statement::Assign.assignments` to
  `SmallVec<[AssignPair; 4]>`
- [ ] `crates/oxabl_parser/src/parser/statements.rs:3067` — Update `parse_assign_pairs`
  return type and `Vec::new()` to `SmallVec::new()`
- [ ] Run `cargo test` — all tests must pass
- [ ] Run CodSpeed benchmarks to verify recovery

**Success criteria for Phase 1:**
- `expressions` benchmark recovers to ≤ 310 µs (baseline ~274 µs + some acceptable
  overhead from new parser features)
- `procs_funcs` and `oo_abl` recover to ≤ 270 µs

### Phase 2: Expression fast-path (Fix 3)

- [ ] Add `Kind::KwIf` branch check in `parse_assign_pairs` value parsing
- [ ] Verify correct parse output with existing ASSIGN tests (`cargo test -p
  oxabl_parser -- assign`)
- [ ] Confirm improvement vs Phase 1 baseline in CodSpeed

### Phase 3: PHF keyword lookup (Fix 4) — separate PR

- [ ] Add `phf` dependency to `oxabl_lexer`
- [ ] Modify `generate_keyword_match()` in `oxabl_codegen/src/main.rs` to emit
  `phf_map!` instead of nested `match` arms
- [ ] Update codegen to handle the abbreviation expansion (multiple input strings →
  same `Kind` value)
- [ ] Run `cargo run -p oxabl_codegen` to regenerate `kind.rs`
- [ ] Run `cargo test` full suite
- [ ] Run CodSpeed and compare against Phase 2 baseline

## Alternative Approaches Considered

### Token Vec: count tokens in a first pass

Instead of a source-length heuristic, do a fast first pass counting tokens to get an
exact capacity. Rejected: doubles the scan of the source string, which is worse than
the occasional extra realloc for unusually token-dense files.

### Comment tokens: discard at lex time instead of storing

Comments are stored in the token stream for future formatter use. Discarding them would
reduce token count and eliminate the realloc pressure increase from today's change.
Rejected: the formatter will need comment tokens; removing them now creates tech debt.
The correct fix is Fix 1 (pre-allocation), which absorbs the comment token increase.

### AssignPair: use Box<[AssignPair]> (exact-fit allocation)

After collecting all pairs, shrink to exact size. Rejected: `Box<[T]>` still allocates
for any size including 1; SmallVec avoids allocation entirely for the common case.

### match_keyword: SIMD byte comparison

Use explicit SIMD intrinsics to compare multiple keyword bytes in parallel. Rejected as
premature given that a PHF would provide similar benefit with far less complexity and no
unsafe SIMD code. PHF first.

## System-Wide Impact

### Interaction graph

- `tokenize()` capacity hint → affects all call sites of `tokenize()` (lexer tests,
  parser, benchmarks). Change is additive; no behaviour change.
- `SmallVec<AssignPair>` → AST type changes propagate to:
  - `oxabl_parser` (parse_assign_pairs return type, parse_assign_statement)
  - Any code pattern-matching on `Statement::Assign { assignments }` (currently only in
    parser tests via `assert_eq!` on the full AST)
  - Future formatter/linter crates that will consume the AST

### Error propagation

No change to error handling paths. All fixes are on the happy path.

### State lifecycle

No persistent state. All changes are within a single parse call's lifetime.

### API surface parity

`tokenize()` signature unchanged. `Statement::Assign` variant field type changes from
`Vec<AssignPair>` to `SmallVec<[AssignPair; 4]>`. This is a breaking change to the
public AST type. Since `oxabl_ast` is pre-1.0, this is acceptable; document in the
commit message.

## Acceptance Criteria

- [ ] All existing tests pass (`cargo test` — currently 406 parser tests + 49 lexer tests)
- [ ] `expressions` benchmark: ≤ −10% vs baseline (hardware-normalised)
- [ ] `procs_funcs` benchmark: ≤ −5% vs baseline
- [ ] `oo_abl` benchmark: ≤ −5% vs baseline
- [ ] `full_program` benchmark: ≤ −15% vs baseline (Phase 1–2), ≤ −20% (Phase 3)
- [ ] No new heap allocations in `tokenize()` for files ≤ 5000 tokens (verify by
  removing `Vec::with_capacity` branch and confirming realloc count drops to 0–1)
- [ ] `cargo clippy -D warnings` passes
- [ ] `cargo fmt --check` passes

## Success Metrics

CodSpeed comparison of each phase PR against the previous baseline should show green
improvement badges on the four regressed benchmarks. The target is to recover at least
50% of the regression from today's feature work across Phases 1 and 2.

## Dependencies & Prerequisites

- `smallvec` crate (v1.x, MIT/Apache-2) — no concerns, widely used in Rust AST tooling
- `phf` crate (Phase 3 only) — well-maintained, zero-runtime-cost hash, used in
  rustc/clippy themselves
- No infrastructure changes required

## Risk Analysis

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| `Vec::with_capacity` over-allocates for tiny files | Low | Use `/ 5` as a conservative lower bound; tiny files (<100 tokens) waste < 100 bytes |
| `SmallVec` inline size wrong (too small, wastes stack) | Low | 4 inline slots × (2 × Expression size); measure stack frame growth before merging |
| PHF compile-time hash collision during codegen | Very low | `phf` guarantees no collision for given input set; codegen runs in CI |
| `parse_assign_pairs` fast-path misses valid syntax | Medium | Cover with targeted tests for `ASSIGN a = (b > c)` and `ASSIGN a = b AND c` before shipping |

## Sources & References

### Internal References

- `crates/oxabl_lexer/src/lib.rs:23–35` — `tokenize()` function (Vec::new() hot path)
- `crates/oxabl_parser/src/parser/statements.rs:3067–3088` — `parse_assign_pairs()`
- `crates/oxabl_ast/src/statement.rs:769–776` — `AssignPair` struct definition
- `crates/oxabl_lexer/src/kind.rs:608–627` — `match_keyword()` (current stack-buffer approach)
- `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md` — prior
  optimisation pattern; same stack-buffer principle applies to Vec capacity

### Related Work

- PR #19 — `perf(lexer): eliminate heap allocations in keyword matching` (~20% win)
- PR #17 — `refactor(parser): migrate keyword workarounds to proper lexer tokens` (~8% win)
- CodSpeed run `69da995c` — HEAD at plan-write time (2026-04-11); now stale — use
  the latest completed run on master when implementing
- CodSpeed run `69d85f84` — pre-regression baseline (2026-04-10)
