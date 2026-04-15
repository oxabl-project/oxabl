---
title: "Fix Parser Regression: 93% to 98%+ with Preprocessing"
type: fix
status: active
date: 2026-04-14
deepened: 2026-04-14
---

# Fix Parser Regression: 93% to 98%+ with Preprocessing

## Enhancement Summary

**Deepened on:** 2026-04-14
**Review agents used:** architecture-strategist, code-simplicity-reviewer, pattern-recognition-specialist, performance-oracle, software-architect

### Key Improvements from Review
1. Phase 1 redesigned: replace positive-list `is_display_continuation()` with negative-guard using existing `can_start_statement()` — simpler, more robust, follows existing codebase pattern
2. Phase 1 narrowed: continuation only triggers when initial expression is a string literal or preprocessor reference — prevents false-positive absorption of malformed code
3. Phase 1 unified: merge with existing preprop continuation logic (statements.rs:745-761) into a single code path
4. Phase 2b: use `ExpressionStatement` not `Empty` to preserve information for future tooling
5. Phases 2a/2b/2c collapsed into a single implementation pass
6. Phase 3a reduced to "investigate 8 files post-Phase 1" — no speculative planning

### Institutional Knowledge Applied
- `docs/solutions/corpus-remaining-failures.md` — Confirms boundary: 8 documented failures require preprocessor evaluation and are NOT parser fixes. Phase 1 is distinct — it fixes valid ABL the parser never implemented.
- `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md` — "Push classification upstream" principle upheld: all Phase 1 dispatch is on `Kind` variants (O(1)), no string comparison.

---

## Overview

After introducing the preprocessor and running corpus checks with `--preprocess`, the pass rate dropped from **99.2% (raw) to 93.4% (preprocessed)** against the pcna-erp corpus (14,008 files, 912 failures). The preprocessor correctly expands includes, exposing ABL constructs inside `.i` files that the parser has never seen. These are real parser gaps, not preprocessor bugs.

This is distinct from the preprocessor-structural failures documented in `docs/solutions/corpus-remaining-failures.md` — those involve AST structure that depends on preprocessor condition evaluation (e.g., blocks opened in one `&IF` branch and closed in another). The failures addressed here are valid ABL syntax the parser simply never implemented because it was hidden inside `.i` include files.

**Goal:** Restore pass rate to **~98%+** with preprocessing enabled by fixing the highest-impact parser gaps.

**Debug tooling:** A `--debug` flag has been added to `oxabl check` that uses `parse_program()` (error recovery) and dumps the last 10 AST statements before the error plus source context. Usage:

```bash
cargo run --release --bin oxabl check <file> --preprocess -I ~/Code/pcna-erp --debug
```

## Error Landscape

| # | Error Pattern | Count | % | Classification | Fix Priority |
|---|---|---|---|---|---|
| 1 | PUT/DISPLAY juxtaposition (string + expr + skip) | ~650 | 71% | **Parser gap** | P0 - fixes 71% of failures |
| 2 | `Expected '.' to end statement` (non-juxtaposition) | ~87 | 10% | Mixed (parser + fragment includes + bad source) | P2 - heterogeneous |
| 3 | `Expected identifier` | 29 | 3% | **Parser gap** (can_be_identifier) | P1 - easy wins |
| 4 | `Unexpected token Colon` (widget `:method()`) | 23 | 3% | **Parser gap** | P1 |
| 5 | `Unexpected token Period` | 22 | 2% | Mixed | P2 |
| 6 | `Unexpected token Then` (bare `AVAILABLE`) | 13 | 1% | **Bad ABL practice** (valid but almost certainly a bug) | P1 |
| 7 | `Unexpected token LeftBrace` (unresolved includes) | 13 | 1% | Preprocessor limitation | P3 - skip |
| 8 | `Unexpected token KwElse` (&ENDIF boundary) | 8 | 1% | **Preprocessor-structural** (see corpus-remaining-failures.md) | P3 - skip |
| 9 | `Expected ':' after CLASS header` (template files) | 6 | 1% | **Not ABL** (skip these files) | Skip |
| 10 | `Unexpected token Invalid` | 6 | 1% | Lexer gap | P3 |
| 11 | Other (55 assorted) | 55 | 6% | Mixed | P3 |

---

## Phase 1: Implicit Output Continuation (~650 fixes, P0)

### The Problem

ABL allows implicit output via juxtaposed expressions — multiple string literals, field references, and format keywords placed side-by-side with no commas. This is the dominant pattern in web-facing ABL files:

```abl
/* Inside a DO block or PROCEDURE body, this is a single implicit PUT statement: */
'<script language="javascript">' skip
   'document.location.href = "' hostURL appURL '/page.p"~;' skip
'</script>' skip.
```

The parser currently sees `'<script language="javascript">'` as a complete expression statement, expects `.` to end it, but encounters `skip` or another string.

**Classification: Parser gap.** This is valid ABL syntax. The `{&OUT}` macro in WebSpeed expands to `PUT UNFORMATTED`, but many files use bare juxtaposed expressions without the macro — the ABL runtime implicitly outputs them.

### The Fix: Unified Continuation Logic

**Core insight from review:** The codebase already has this exact pattern at `statements.rs:745-761` — the `starts_with_preprop` branch that checks "if not a period and not a new statement, skip to end." Phase 1 generalizes this to non-preprop expression statements. Rather than building a new positive-list function, use the **negative guard** already established: `can_start_statement()`.

**Implementation site:** `statements.rs`, in the fallthrough at the end of `parse_statement()` where expression statements are handled (around line 763). Unify the existing preprop continuation branch (lines 745-761) with the new general continuation logic into a single code path.

**Narrowing condition:** Only activate continuation when the initial expression is a **string literal** or **preprocessor reference**. Arbitrary identifiers or function calls followed by more tokens are more likely to be two statements with a missing period than a display-item list. This prevents false-positive absorption of malformed code.

```rust
// After parsing the expression via finish_expression():

// Fast path: statement ends with period (vast majority of cases)
if self.check(Kind::Period) {
    self.advance();
    return Ok(Statement::ExpressionStatement(expr));
}

// Implicit output continuation: string/preprop expr followed by more items.
// Only triggers when the initial expression is a string literal or preprocessor
// reference — the dominant pattern in WebSpeed/web-facing ABL files.
// Uses the negative guard: if the next token can start a new statement, don't
// treat it as continuation — it's a new statement (possibly with a missing period).
let expr_is_output_candidate = matches!(
    &expr,
    Expression::Literal(Literal::String(_)) | Expression::Identifier(_)
) || starts_with_preprop;

if expr_is_output_candidate && !self.at_end() {
    let next = self.peek().kind;
    let next_is_new_statement = can_start_statement(next)
        || matches!(next, Kind::PreprocIf | Kind::PreprocElse
            | Kind::PreprocElseif | Kind::PreprocEndif
            | Kind::PreprocScopedDefine | Kind::PreprocGlobalDefine
            | Kind::PreprocUndefine);
    let next_is_block_label = Self::can_be_identifier(next)
        && self.check_at(1, Kind::Colon)
        && matches!(self.peek_at(2).kind, Kind::Do | Kind::Repeat | Kind::KwFor);

    if !next_is_new_statement && !next_is_block_label {
        self.skip_to_statement_end();
        return Ok(Statement::ExpressionStatement(expr));
    }
}

self.expect_kind(Kind::Period, "Expected '.' to end statement")?;
Ok(Statement::ExpressionStatement(expr))
```

This replaces the existing preprop-specific continuation logic at lines 745-761, eliminating the duplicate code path.

### Why This Approach

**Performance (confirmed by review):** For the common case (statement ends with period), the `self.check(Kind::Period)` short-circuits immediately — one integer comparison, branch-predicted. `is_display_continuation()` is never reached. Net effect is **performance positive**: replacing ~650 error-recovery cycles (expensive `synchronize()` loops) with cheap `skip_to_statement_end()` calls.

**Safety:** The negative guard (`can_start_statement()`) is the single source of truth for statement boundaries, already maintained across the codebase. No parallel list to drift. The narrowing condition (string/preprop only) prevents the continuation from becoming a catch-all that masks real errors.

**Pattern consistency (confirmed by review):** Follows the exact pattern at `statements.rs:745-761`. Also consistent with how PUT (line 517), EXPORT (line 332), and FORM (line 524) use `skip_to_statement_end()` for statements not yet fully modeled in the AST. The existing `parse_display_statement()` at line 3385+ demonstrates what a full parse of display-item lists looks like — this can be evolved toward that later.

### Validation

- Must not regress the raw (non-preprocessed) pass rate from 99.2%
- Must reduce `Expected '.' to end statement` count from 737 to ~100 or less
- `cargo test` must pass (existing 406 tests)
- `cargo bench -p oxabl_parser` must show no regression (expect neutral-to-positive)
- Add **negative regression tests**: common statement sequences (`myVar = 5. anotherVar = 10.`) must NOT be absorbed by the continuation logic

### Future Evolution

The `skip_to_statement_end()` approach is a pragmatic first step. The eventual target is a proper `parse_display_item_list()` that produces structured AST nodes — reusing patterns from the existing `parse_display_statement()` (statements.rs:3385-3465) which already handles `SKIP`, `SPACE`, `FORMAT`, `WHEN`, `AT`, `VIEW-AS` between display items.

---

## Phase 2: Quick Wins (P1, ~65 fixes — single implementation pass)

All three sub-phases are independent single-site changes. Implement all in one pass, validate once.

### 2a. `Expected identifier` — Expand `can_be_identifier()` (29 fixes)

**Classification: Parser gap.** ABL allows many keywords to be used as identifiers (field names, variable names, method names). When the parser hits one of these in an identifier position, it fails with "Expected identifier."

**Fix:** Sample 5-10 representative errors, identify the missing keywords, add them to `can_be_identifier()` in `mod.rs`. This is mechanical — each keyword just needs a new match arm.

**File:** `crates/oxabl_parser/src/parser/mod.rs`, `can_be_identifier()` function (line 433+).

### 2b. Widget `:method()` Calls at Statement Start (23 fixes)

**Classification: Parser gap.** ABL allows calling methods on implicit widgets using colon syntax at the start of a statement:

```abl
:add-last(v-field) in frame .
```

This is a method call on the currently focused widget (set by a prior `CHOOSE` or selection-list interaction). The parser sees `:` (Colon) at statement start and doesn't know what to do.

**Fix:** In `parse_statement()`, when the current token is `Kind::Colon`, treat it as an implicit widget method call — skip to statement end. Insert after the CATCH/FINALLY checks (~line 99) and before the block-label check (~line 101), since block labels use `check_at(1, Kind::Colon)` (checking position 1), while this checks position 0 — no collision.

```rust
// Implicit widget method call: :method-name(args) [IN FRAME frame-name].
if self.check(Kind::Colon) {
    self.skip_to_statement_end();
    return Ok(Statement::ExpressionStatement(
        Expression::Unknown(self.tokens[self.current - 1].span())
    ));
}
```

**Why `ExpressionStatement` not `Empty`:** `Statement::Empty` implies "nothing was here," which is misleading for tooling that counts statements, measures complexity, or generates source maps. A more honest representation preserves the information that something was present.

**Pattern consistency (confirmed by review):** Follows the same skip pattern as PUT (statements.rs:517-520), FORM (524-527), COPY-LOB (511-513), VIEW/HIDE (530-538).

### 2c. Bare `AVAILABLE` Without Argument (13 fixes)

**Classification: Bad ABL practice.** In ABL, `AVAILABLE` can technically be called without a buffer argument if there is a default buffer in scope. However, this is extremely poor practice — it evaluates the availability of whatever the ambient buffer happens to be. In real code, this is almost certainly an accidental omission of the buffer name, not an intentional check.

```abl
if available then color display normal with frame .
/* Almost certainly should be: if available _field then ... */
```

**Pre-check:** Verify whether Phase 1 already fixes these 13 files (the `IF available THEN color display ...` lines contain juxtaposed display items after `THEN` which may trigger the continuation logic). Only implement this fix if Phase 1 does not resolve them.

**If needed — Fix:** In `parse_primary()` at `expressions.rs:910-920` where `AVAILABLE` is handled, add a guard before calling `parse_postfix()`:

```rust
// Bare AVAILABLE — no buffer argument (bad practice but valid ABL)
if !Self::can_be_identifier(self.peek().kind) && !self.check(Kind::LeftParen) {
    return Ok(Expression::FunctionCall { name, arguments: vec![] });
}
```

This follows the guard-before-consume pattern used at `expressions.rs:910` (paren check).

**Why parse it correctly:** The parser's job is to understand structure. This IS valid ABL. Erroring here means we can't parse the rest of the file (the `THEN` clause, `ELSE` clause, frame phrase — all lost). The linter is the right place for "this is technically valid but almost certainly wrong."

---

## Phase 3: Re-evaluate After Phase 1 (P2)

After Phase 1 and Phase 2, re-run the corpus and categorize residual failures. Split remaining "Expected '.'" errors into:
- **`.i` fragment files** (bare parameter lists, WHERE clauses, expression fragments) — these are include fragments only valid in a host context. Consider excluding from the pass-rate denominator or reporting as a separate metric.
- **`.p`/`.w`/`.cls` full programs** — these are real parser gaps or source-level bugs worth investigating.

### `Unexpected token Period` — Mixed Causes (22 fixes)

Several distinct sub-causes to investigate:

1. **`ASSIGN x = .`** — Assignment with no value, period terminates. Fix: in `parse_assign_statement()`, if token after `=` is `Kind::Period`, emit assignment with no value or treat as `Unknown` literal.
2. **`COLOR DISPLAY ...`** — COLOR statement. Fix: add `Kind::Color` to skip-to-statement-end list if not already there.
3. **Include fragments** — unfixable without context.

### `&ENDIF` + ABL `ELSE` (~8 fixes)

Investigate the 8 specific files post-Phase 1. These likely fall into the same category as the 8 documented failures in `docs/solutions/corpus-remaining-failures.md` — preprocessor-structural ambiguity requiring condition evaluation. If so, skip them.

---

## Phase 4: Low Priority / Skip (P3)

### `Unexpected token LeftBrace` (13 failures)

**Classification: Preprocessor limitation.** Include references (`{file.i}`) that the preprocessor couldn't resolve (file not found, circular reference, etc.). Not a parser issue.

### `Expected ':' after CLASS header` (6 failures)

**Classification: Not ABL.** Developer template files (`templates/*.cls`) using `{{placeholder}}` double-brace syntax. Should be excluded from corpus checks. Consider adding a `--exclude` pattern to `oxabl check`.

### `Unexpected token Invalid` (6 failures)

**Classification: Lexer gap.** Investigate individually if time permits.

---

## Implementation Order

```
Phase 1 (P0): Unified continuation logic      -> ~650 fixes -> ~71% of failures
Phase 2 (P1): All quick wins in one pass       -> ~65 fixes
  2a: can_be_identifier expansion              -> ~29 fixes
  2b: Widget :method() calls                   -> ~23 fixes
  2c: Bare AVAILABLE (if not fixed by Phase 1) -> ~13 fixes
                                      Subtotal: ~715 fixes -> 93.4% -> ~98.3%
Phase 3 (P2): Re-evaluate residuals            -> ~50-100 more fixes (varies)
```

Phase 1 alone should bring us from 93.4% to ~97.7%. Phase 2 quick wins push past 98%.

## Acceptance Criteria

- [ ] Preprocessed corpus pass rate >= 98% (from 93.4%)
- [ ] Raw corpus pass rate >= 99.2% (no regression)
- [ ] All 406 existing parser tests pass
- [ ] `cargo clippy -D warnings` clean
- [ ] `cargo fmt --check` clean
- [ ] `cargo bench -p oxabl_parser` shows no regression
- [ ] New tests for each pattern added
- [ ] Negative regression tests: common statement sequences not absorbed by continuation logic

## Debug Tooling (Already Implemented)

The `--debug` flag on `oxabl check` is available for investigating individual failures:

```bash
# Single file debug with preprocessing
cargo run --release --bin oxabl check <file> --preprocess -I ~/Code/pcna-erp --debug

# Single file debug without preprocessing
cargo run --release --bin oxabl check <file> --debug
```

Output shows:
- Last 10 AST statements parsed before first error (truncated at 200 chars)
- All errors with line/col and surrounding source context (5 lines above/below)
