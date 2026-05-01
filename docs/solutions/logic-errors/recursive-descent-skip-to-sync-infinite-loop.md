---
title: "Recursive-descent `skip_to_sync()` infinite loop on its own sync token"
category: logic-errors
date: 2026-04-17
tags: [parser, recursive-descent, error-recovery, infinite-loop, oxabl_schema]
related_prs: ["#49"]
component: oxabl_schema
---

# Recursive-descent `skip_to_sync()` infinite loop on its own sync token

## Problem

While building the `.df` parser (`crates/oxabl_schema/src/parser.rs`),
`cargo test -p oxabl_schema` hung indefinitely after one specific test
printed `ok`. Under default parallel execution the hang looked like "some
test never finishes" with no indication which one.

**Symptom:**
- `cargo test` never returns
- Most tests print `ok`; at least one parser test causes the runner to
  stall
- Reproduces deterministically under `-- --test-threads=1` on the first
  test whose input contains a top-level `UPDATE`, `DROP`, or `RENAME`
  directive (e.g. `skips_update_drop_rename`)

## Root Cause

The parser's error-recovery / skip-directive primitive was:

```rust
fn skip_to_next_directive(&mut self) {
    while self.idx < self.tokens.len() && !self.is_at_next_directive() {
        self.idx += 1;
    }
}

fn is_at_next_directive(&self) -> bool {
    match self.tokens.get(self.idx) {
        Some(t) if t.kind == TokKind::Word && t.line_start => {
            DIRECTIVE_KEYWORDS.contains(&self.word_text(t).to_ascii_uppercase().as_str())
        }
        _ => self.at_eof(),
    }
}
```

And the caller in `parse_dump()`:

```rust
match word.as_str() {
    "ADD" => self.parse_add(),
    "UPDATE" | "CHANGE" | "DROP" | "RENAME" => self.skip_to_next_directive(),
    _ => { self.emit_unexpected(); self.skip_to_next_directive(); }
}
```

The bug is in the interaction between the two. When `parse_dump()` peeks
and sees `UPDATE` (a top-level keyword at `line_start=true`), it dispatches
to `skip_to_next_directive()` **without advancing past the `UPDATE`
token**. On entry, the skip function's very first check
(`is_at_next_directive()`) sees *the same* `UPDATE` token and returns
`true` — so the `while` body never executes. The skip returns. Control
goes back to the `parse_dump()` loop, which peeks again and sees the same
`UPDATE` token. Infinite loop.

This is the classic recursive-descent error-recovery trap: **skip-to-sync
hangs when the current token *is* the sync token.**

## Solution

Advance past the current token before handing off to the skip primitive:

```rust
match word.as_str() {
    "ADD" => self.parse_add(),
    "UPDATE" | "CHANGE" | "DROP" | "RENAME" => {
        // Consume the starting keyword so skip doesn't immediately
        // re-match on its own `line_start` and stall.
        self.idx += 1;
        self.skip_to_next_directive();
    }
    _ => {
        self.emit_unexpected();
        self.idx += 1;
        self.skip_to_next_directive();
    }
}
```

Audit every call site that hands off to `skip_to_sync()` and make sure
*either* (a) the caller already consumed tokens past the sync trigger, or
(b) the first thing the skip does is `self.idx += 1`. Keep it at the call
site, not inside the skip — because some call sites (e.g. `parse_add` on
an unknown kind like `ADD FOO`) are already past the sync token via
earlier consumption, and double-advancing there would skip a legitimate
token.

### Diagnosing the hang

Run `cargo test -p <crate> -- --test-threads=1`. The last line printed
before the hang is the test *immediately before* the hanging test in
source-declaration order (not the hanging test itself — the runner prints
`ok` as each completes). The hanging test is the next one in the file.

## Prevention

- **Every recursive-descent loop must advance `self.idx` on every
  iteration.** If a branch delegates to a helper, prove the helper
  advances at least once in every reachable path.
- **Skip-to-sync primitives are dangerous when called on their own sync
  token.** Three options, pick one and document it:
  1. Caller consumes the sync-triggering token before calling (chosen
     here — keeps the skip primitive simple).
  2. Skip always advances by at least one before checking (surprising
     behavior; rejected).
  3. Skip takes a `min_advance: usize` parameter and asserts (too much
     ceremony for a single call site).
- **Serial test execution is the diagnostic.** Default Rust test
  parallelism hides which test is hanging. When you hit a hang, re-run
  with `--test-threads=1` first — before chasing the bug.
- **Put `debug_assert!(self.idx > start_idx)` at the end of every
  hand-written parser loop** that claims to make progress. Cheap,
  catches this class of bug at the first iteration under debug test
  runs.

## Related

- Plan: `docs/plans/2026-04-16-004-feat-semantic-layer-v1-plan.md`
  (Phase 2). Same pattern will recur in the Phase 3 declare pass's
  recovery on `.` boundaries.
- Existing parser uses period-boundary synchronization
  (`oxabl_parser::parse_program`) — same pitfall shape. Worth an audit
  when convenient.

## Compounding Principle

Error recovery in hand-written parsers is a high-risk surface because
it's the one place where "the input doesn't match any rule" is the
*expected* state. Every new recursive-descent parser in oxabl (Phase 3
declare pass, Phase 4a resolve, Phase 4b check) will hit some variant of
this bug unless we bake the `debug_assert!(idx > start)` guard into the
walker pattern from the start. Do it once in a shared helper.
