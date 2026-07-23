---
title: Formatter reindent corrupts (and bails on) lines that begin inside a multi-line token
date: 2026-07-23
category: docs/solutions/logic-errors
module: oxabl_formatter
problem_type: logic_error
component: tooling
symptoms:
  - "oxabl format leaves a whole file unchanged, reporting \"formatting would have altered the token stream; left unchanged\" (FormatBail::SemanticGuardTripped)"
  - "The file contains a string literal or an {include}/preprocessor reference that spans physical lines"
  - "The bail only happens when the enclosing statement is under-/over-indented (its continuation line would actually shift); an already-correctly-indented file formats fine"
root_cause: logic_error
resolution_type: code_fix
severity: medium
tags: [formatter, reindent, multi-line-token, semantic-guard, string-literal, include-reference]
---

# Formatter reindent corrupts (and bails on) lines that begin inside a multi-line token

## Problem

The `oxabl_formatter` layout engine reindents line-by-line: it strips each physical line's leading whitespace and rewrites it to the line's structural block depth. That is correct for code and trivia, but wrong for any physical line that *begins inside* a multi-line token whose interior bytes are significant — a string literal, or an `{include}`/preprocessor reference that wraps across lines. The whitespace it rewrites is *inside* the token, so the edit changes the token's bytes. The re-lex semantic-preservation guard catches the drift and refuses to emit, so the whole file bails unchanged. No corruption reaches disk, but common real-world ABL (e.g. `{include}` `&args` spread across lines) becomes unformattable.

## Symptoms

- `oxabl format <file>` reports `formatting would have altered the token stream; left unchanged` and leaves the file byte-for-byte unchanged.
- Reproduces on a multi-line string literal or a multi-line `{include}`/preprocessor reference inside an under-indented block.
- Does *not* reproduce when the enclosing statement is already at the correct depth (nothing shifts, so the guard never trips).

## What Didn't Work

- Nothing was "tried and failed" here — the guard was doing its job. The trap is misreading the bail as a guard bug rather than a coverage gap in the reindenter: the guard is the *backstop*, and the real fix belongs upstream, in what the printer chooses to reindent.

## Solution

Leave any physical line that begins inside a multi-line token verbatim, detected from token spans, and protect such lines from blank-line normalization too:

- **Detect** interior lines from the whole-source token stream. A line `l` begins inside token `t` iff `t.start < line_starts[l] < t.end` — i.e. `l` is strictly after the token's first line (`sl = line_index(t.start)`) and no later than its last (`el = line_index(t.end - 1)`), so the protected range is `sl+1..=el`. Skip `Kind::Comment` (trivia the guard ignores — comments keep their existing delta-preserving reindent) and guard `t.end == 0` against underflow.
- **Emit verbatim** via a `protected` flag on the line IR: prepend the line's original leading whitespace to its content and push at indent 0, so flushing reproduces the source bytes exactly.
- **Protect blanks:** blank-line normalization must treat a `protected` line as never-droppable, because a *blank* physical line inside a multi-line string literal is a significant byte of the string's value — clamping/dropping it would change the string and re-trip the guard.

Files: `crates/oxabl_formatter/src/printer.rs` (detection + verbatim emit), `crates/oxabl_formatter/src/ir.rs` (the `protected` flag + `push_protected`), `crates/oxabl_formatter/src/blanks.rs` (`is_droppable_blank`), backstopped by `crates/oxabl_formatter/src/guard.rs`. Shipped in PR #96.

## Why This Works

The keyword-recasing and `end_with_type` passes never touch a protected line — a multi-line string/include is a single token with no transformable keyword sub-tokens in its interior — so reconstructing the full original line at emit time is safe and order-independent. Because only lines *before* the opener (real indentation) get reindented and the token's interior bytes are emitted untouched, the re-lexed token stream matches the input and the guard passes. Idempotency holds: on a second pass a protected line already has zero leading whitespace to strip, so it re-emits identically.

## Prevention

- **Invariant for this layout-only engine:** any line-granular transform must treat the interior of a multi-line *significant* token as verbatim. When adding a new line-level transform (recasing, blank rules, future placement rules), ask "could this edit land inside a multi-line token's bytes?" and gate it on the same protected-line set.
- **The guard is the backstop, not the fix.** A `SemanticGuardTripped` bail on a shape the formatter *should* handle is a signal to widen what the printer emits verbatim — not to weaken the guard. The guard turning "would mangle" into "fails safe (bails unchanged)" is the safety property that makes this class of bug non-destructive.
- **Regression coverage:** guard-pass + idempotency tests exist for the multi-line string and multi-line include shapes under *both* the preserving (`default_base`) and strict (`oestandards`) presets, plus an interior-blank-line test that fails without the blank-normalization change. See `crates/oxabl_formatter/tests/{formatting,idempotency}.rs`.
- **Known residual (fails safe):** line endings are still normalized to the file's dominant ending at flush, so a multi-line token whose interior newline differs from the dominant ending (e.g. an interior `\r\n` in a mostly-`\n` file) still trips the guard and bails — unchanged, not corrupted.

## Related Issues

- GH issue #95 (root cause + repros), PR #96 (fix).
- `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md` — another lexer/formatter hot-path learning in the same toolchain.
