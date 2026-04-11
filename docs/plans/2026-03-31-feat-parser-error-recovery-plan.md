---
title: "Parser Error Recovery"
type: feat
status: completed
date: 2026-03-31
deepened: 2026-03-31
---

# Parser Error Recovery

## Overview

Add error recovery to the oxabl parser so it can continue parsing after encountering errors, accumulating diagnostics instead of stopping at the first failure. This is essential for formatter and linter use cases where partial parse results are more useful than no results.

## Problem Statement

Currently, `parse_statement()` returns `Result<Statement, ParseError>` and callers bail on the first error. For a linter that needs to check variable usage across an entire file, stopping at the first syntax error means the rest of the file is invisible. Real-world ABL files often have minor issues that shouldn't prevent analysis of the rest of the file.

## Proposed Solution

### Core Changes

#### 1. Accumulate errors on the Parser struct

Reuse the existing `ParseError` type (already has `message: String` and `span: Span`). No new `ParseDiagnostic` or `Severity` enum needed — the parser only produces errors, not warnings. Warnings are a linter concern.

```rust
pub struct Parser<'a> {
    tokens: &'a [Token],
    source: &'a str,
    current: usize,
    errors: Vec<ParseError>,  // NEW: accumulated parse errors
}
```

#### 2. Add `Program` top-level node (in `oxabl_ast`)

```rust
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<ParseError>,
}
```

**Crate boundary note:** `ParseError` lives in `oxabl_parser`. To avoid `oxabl_ast` depending on the parser, either:
- (a) Move `ParseError` to `oxabl_common` (cleanest — both crates can reference it), or
- (b) Keep `Program` in `oxabl_parser` and have downstream consumers depend on the parser crate

Option (a) is recommended.

#### 3. Add `synchronize()` to Parser

Skip tokens until we hit a statement boundary:
- A `.` (period) — ABL's statement terminator. Consume it.
- A statement-starting keyword (`DO`, `IF`, `FOR`, `DEFINE`, `RUN`, `DISPLAY`, `MESSAGE`, `PROCEDURE`, `CASE`, `REPEAT`, `FIND`, `RETURN`, `LEAVE`, `NEXT`, `ASSIGN`, `END`)
  - Note: `VAR` is also a common statement starter but is lexed as `Kind::Identifier`, not its own variant. Consider adding it to sync set via text check.. Do NOT consume — let the next `parse_statement` call handle it.

```rust
fn synchronize(&mut self) {
    while !self.at_end() {
        if self.peek().kind == Kind::Period {
            self.advance(); // consume the period
            return;
        }
        if matches!(self.peek().kind,
            Kind::Do | Kind::KwIf | Kind::KwFor | Kind::Define |
            Kind::Run | Kind::Display | Kind::Message | Kind::Procedure |
            Kind::Case | Kind::Repeat | Kind::Find | Kind::KwReturn |
            Kind::Leave | Kind::Next | Kind::End
        ) {
            return; // don't consume — it starts the next statement
        }
        self.advance();
    }
}
```

#### 4. Change `parse_statements()` to accumulate errors

```rust
pub fn parse_statements(&mut self) -> Program {
    let mut statements = Vec::new();
    while !self.at_end() {
        match self.parse_statement() {
            Ok(stmt) => statements.push(stmt),
            Err(err) => {
                self.errors.push(err);
                self.synchronize();
            }
        }
    }
    Program {
        statements,
        errors: std::mem::take(&mut self.errors),
    }
}
```

#### 5. Add fuel/progress guarantee

Every parsing loop must guarantee forward progress. Add a fuel counter to catch infinite loops during development:

```rust
fn check_progress(&mut self, last_pos: &mut usize) {
    debug_assert!(
        self.current > *last_pos,
        "Parser made no progress at position {}",
        self.current
    );
    *last_pos = self.current;
}
```

### Granularity Decision

**Per-statement recovery** (v1): If a statement fails, skip to the next period and continue. Simple, predictable, handles the majority of real-world cases. ABL's period-terminated statements make this natural.

**Per-block recovery** (future): If a block is malformed (missing `END.`), try to infer block boundaries from indentation or keyword nesting. Much more complex, defer.

## Acceptance Criteria

- [ ] Parser can parse a file with syntax errors and return partial results
- [ ] All errors are collected in `Program.errors`
- [ ] Valid statements before and after an error are still parsed correctly
- [ ] `synchronize()` correctly finds the next statement boundary
- [ ] Existing tests continue to pass (refactor to use `Program` wrapper)

## Test Cases

- [ ] Missing `END.` — parser recovers and parses subsequent statements
- [ ] Missing `:` after block header — parser skips to next period
- [ ] Unmatched parentheses in expression — parser skips statement
- [ ] Unknown keyword in statement position — parser skips to next period
- [ ] Multiple errors in one file — all are collected
- [ ] Valid file — `errors` is empty, all statements parsed
- [ ] Parser never panics on arbitrary input (property-based test with `proptest`)

## Risks

- **Test migration:** Changing the return type of `parse_statements()` from `Vec<Statement>` to `Program` touches every test. Plan for a mechanical refactor pass.
- **Cascade errors:** `synchronize()` can produce cascade errors (one real error causes multiple diagnostics). Consider suppressing errors that occur at the synchronization point itself.
- **`finish_expression` fragility:** `statements.rs` has a `finish_expression()` that duplicates precedence logic from `expressions.rs`. If error recovery adds fallback paths, both must be updated in lockstep. Consider consolidating before this work.

## Research Insights

**The matklad approach (rust-analyzer):** Uses event-based parsing that decouples parsing from tree construction via an event stream (`Open`, `Close`, `Advance`). This is the gold standard for IDE-grade error recovery but is a significant architectural rewrite. The synchronization approach above is a pragmatic intermediate step that can evolve toward event-based parsing if needed.

**Anchor/Recovery sets:** Production parsers pass "recovery sets" (tokens that can follow the current construct) through parsing functions. When an unexpected token is encountered, skip until finding a token in the recovery set. For v1, the global statement-boundary set is sufficient; per-construct recovery sets can be added incrementally.

**Property-based testing:** Use `proptest` to fuzz the parser with random input. The key property: the parser must never panic on any input, always returning `Ok(Program)` with errors collected. This is the single most valuable test for error recovery.

## Dependencies

- Should be done after RUN/DISPLAY/MESSAGE are implemented so we don't have to retrofit recovery into in-flight features
- No external dependencies (except `proptest` as a dev-dependency for fuzz testing)
