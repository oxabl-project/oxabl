---
title: "fix: Preprocessor erases multiline include references because name parser treats \\n as part of the name"
type: fix
status: completed
date: 2026-04-16
---

# fix: Preprocessor erases multiline include references

## Overview

When an include reference puts the include name on its own line and the first
argument on the next line (a very common style in the pcna-erp corpus), the
preprocessor silently erases the whole include. The outer code then looks
syntactically broken and the parser reports a confusing error far from the
root cause.

Example call site (from `PRET010000`):

```abl
If sv-currency ne local-currency THEN
    {ms/currexch.i
    &currency = "sv-currency"
    &date     = "today"
    &exchange = "c-exchange" }
ELSE
    ASSIGN c-exchange = 1.
```

Include body (`ms/currexch.i`) — expected to expand to ~15 lines of ABL:

```abl
If {&currency} gt " "             and
   {&currency} ne local-currency
then do:
   Find last currency-rate where ...
   ...
   {&exchange} = currency-rate.curr-exch-rate.
   ...
End.
Else
      {&exchange} = 1.
```

After preprocessing, the include expansion is empty. The parser then sees
`IF … THEN␤␤ELSE ASSIGN …` and reports `Unexpected token KwElse`:

```
Error 1: 2112:14 — Unexpected token KwElse

  2110 |              If sv-currency ne local-currency THEN
  2111 |
  2112 |              ELSE
  2113 |                  ASSIGN c-exchange = 1.
```

## Root Cause

Two interacting defects in `crates/oxabl_preprocessor/src/preprocessor.rs`:

### 1. `parse_include_name` splits on the wrong character set

```rust
// preprocessor.rs:876
fn parse_include_name(inner: &str) -> String {
    let trimmed = inner.trim();
    if let Some(stripped) = trimmed.strip_prefix('"') { ... }
    // Unquoted — first token (space or & delimited)
    trimmed.split([' ', '&']).next().unwrap_or("").to_string()
}
```

The split pattern is `[' ', '&']` — only space and `&`. `\n`, `\r`, and `\t`
are *not* treated as delimiters. When the source is

```
{ms/currexch.i
                 &currency = "sv-currency"
```

the inner content (after outer `.trim()`) is

```
ms/currexch.i\n                 &currency = "sv-currency"\n …
```

`.split([' ', '&']).next()` returns `"ms/currexch.i\n"` — the newline is
included in the returned name. That value is then passed to
`self.fs.resolve_include(…)`, which cannot match a real path with an embedded
newline, so resolution fails.

This diverges from the sibling helper `find_args_start` (preprocessor.rs:999),
which correctly uses `is_ascii_whitespace()` to locate the name/arg boundary.
Argument parsing works; name extraction doesn't. The drift between the two
helpers is the underlying bug.

### 2. Include-resolution errors are silently discarded

```rust
// preprocessor.rs:55
if ctx.diagnostics.iter().any(|d| matches!(d.severity, Severity::Error))
    && tree.is_empty()
{
    return Err(ctx.diagnostics);
}
…
// Attach non-fatal diagnostics even on success — caller can inspect them.
// For now, we discard them since PreprocessedFile doesn't have a diagnostics field.
// Future: add a diagnostics field to PreprocessedFile.
let _ = ctx.diagnostics;
```

`expand_include` does emit a `PREPROC004 include file not found` diagnostic
when resolution fails (preprocessor.rs:622-628). But because the outer tree
contains plenty of other nodes, the guard above keeps `process` on the `Ok`
path and the entire diagnostic vec is dropped with `let _`.

Net effect of (1) + (2): a malformed-name lookup fails invisibly, the include
expands to zero children, `expand_include` returns an empty `Vec`, the
call-site branch at preprocessor.rs:385

```rust
if !children.is_empty() {
    nodes.push(SpanNode::Include { … });
}
```

skips the push, and the whole `{ms/currexch.i …}` span becomes blank in the
preprocessed text. The user sees only the downstream parser error.

## Proposed Solution

### Primary fix — consistent whitespace handling in `parse_include_name`

Treat all ASCII whitespace as a name terminator, matching the semantics
already used by `find_args_start`. A straightforward rewrite of
`parse_include_name`:

```rust
// crates/oxabl_preprocessor/src/preprocessor.rs
fn parse_include_name(inner: &str) -> String {
    let trimmed = inner.trim();
    let bytes = trimmed.as_bytes();

    if let Some(stripped) = trimmed.strip_prefix('"') {
        if let Some(end_quote) = stripped.find('"') {
            return stripped[..end_quote].to_string();
        }
    }

    // Unquoted — name ends at first ASCII whitespace or `&`.
    let end = bytes
        .iter()
        .position(|b| b.is_ascii_whitespace() || *b == b'&')
        .unwrap_or(bytes.len());
    trimmed[..end].to_string()
}
```

This aligns `parse_include_name` with `find_args_start` (which already
terminates on whitespace or `&`) so the two helpers can never disagree on
where the name ends.

Optional follow-up (non-blocking, out of scope for this PR): extract a single
shared helper `include_name_len(inner: &str) -> usize` and have both call
sites use it, eliminating future drift risk. Left as a TODO so this change
stays a minimal fix.

### Secondary fix — surface include-resolution errors

The silent-drop of preprocessor diagnostics (preprocessor.rs:55-68) is a
pre-existing hazard; this bug is the proof it matters. Resolve the existing
TODO by adding a `diagnostics: Vec<Diagnostic>` field to `PreprocessedFile`
and returning it alongside the tree, so `parse_file_with_preprocess`
(`crates/oxabl/src/main.rs:272`) can report `PREPROC004`/`PREPROC005` as
warnings even when parsing continues.

Minimum change for this PR:

1. Add `pub diagnostics: Vec<Diagnostic>` to `PreprocessedFile`.
2. Thread `ctx.diagnostics` into the constructed `PreprocessedFile` instead of
   discarding with `let _`.
3. In `parse_file_with_preprocess`, before the parser runs, print each
   preprocessor error diagnostic using the same `ParseError` formatter so the
   user sees e.g. `[preprocess] include file not found: 'ms/currexch.i'` at
   the correct line/col.

This is the high-value part of the change: without it, any future name/path
resolution regression will be equally invisible.

### Non-goals

- No changes to include-path resolution behaviour (out of scope).
- No changes to how arguments are parsed (`parse_include_args` already
  handles multi-line input correctly — covered by
  `parse_include_args_multiline` at preprocessor.rs:1610).
- No change to the parser's error message for `IF … THEN␤ELSE …`. That
  error is the correct ABL diagnosis for an empty THEN branch; the real fix
  is to stop producing the empty branch in the first place.

## Acceptance Criteria

- [ ] `parse_include_name("file.i\n&arg=v")` returns `"file.i"`.
- [ ] `parse_include_name("file.i\t&arg=v")` returns `"file.i"`.
- [ ] `parse_include_name("file.i\r\n&arg=v")` returns `"file.i"`.
- [ ] New end-to-end test: an include reference with the name on one line and
      args on the next expands the include body correctly (model the
      `ms/currexch.i` shape).
- [ ] All existing `oxabl_preprocessor` tests pass unchanged.
- [ ] `cargo fmt --check` and `cargo clippy -D warnings` pass.
- [ ] Running `oxabl check --preprocess` against `PRET010000` no longer
      reports `Unexpected token KwElse` for this construct. The include
      expands to its body.
- [ ] (Secondary fix) When `oxabl check --preprocess` runs against a file
      referencing a missing include, the user sees a `[preprocess]` warning
      or error line identifying the missing include — no silent erasure.

## Technical Approach

### Files to modify

| File | Change |
|------|--------|
| `crates/oxabl_preprocessor/src/preprocessor.rs` | Rewrite `parse_include_name` to terminate on any ASCII whitespace or `&`. Add tests. |
| `crates/oxabl_preprocessor/src/preprocessor.rs` | (Secondary) Thread diagnostics into `PreprocessedFile` instead of `let _`. |
| `crates/oxabl_preprocessor/src/span_tree.rs` (or wherever `PreprocessedFile` is defined) | (Secondary) Add `diagnostics` field + accessor. |
| `crates/oxabl/src/main.rs` | (Secondary) Emit preprocess diagnostics as warnings in `parse_file_with_preprocess` and in the debug path. |

### New tests (in `crates/oxabl_preprocessor/src/preprocessor.rs`)

```rust
#[test]
fn parse_include_name_newline_terminator() {
    assert_eq!(parse_include_name("file.i\n&arg=v"), "file.i");
    assert_eq!(parse_include_name("path/file.i\n&arg=v"), "path/file.i");
}

#[test]
fn parse_include_name_tab_terminator() {
    assert_eq!(parse_include_name("file.i\t&arg=v"), "file.i");
    assert_eq!(parse_include_name("file.i\targ1"), "file.i");
}

#[test]
fn parse_include_name_crlf_terminator() {
    assert_eq!(parse_include_name("file.i\r\n&arg=v"), "file.i");
}

#[test]
fn expand_include_multiline_args_name_on_own_line() {
    let fs = make_fs(&[(
        "/inc/currexch.i",
        "If {&currency} gt \" \" then {&exchange} = 1.\nElse {&exchange} = -1.",
    )]);
    let pp = Preprocessor::new(&fs, &[PathBuf::from("/inc")]);
    let source = "\
If x ne y THEN
    {currexch.i
    &currency = \"sv-currency\"
    &exchange = \"c-exchange\" }
ELSE
    ASSIGN c-exchange = 1.";
    let result = pp.process(FileId::new(1), source).unwrap();
    let text = result.to_text();
    assert!(text.contains("If sv-currency gt"),
            "expected expanded include body, got:\n{text}");
    assert!(text.contains("c-exchange = 1"));
}
```

### Corpus verification

The `refine-oxabl-parser` workflow / pcna-erp corpus should be re-run after
the fix to confirm:

- Files using the `{inc.i␤&arg=...}` shape stop failing to preprocess.
- No regressions in existing passing files (expected: none — this is a
  strict expansion of the accepted input surface).

## System-Wide Impact

- **Interaction graph**: `process_source` → `find_matching_brace` →
  `parse_include_name` → `fs.resolve_include` → `expand_include` →
  recursive `process_source`. The fix is isolated to the name-parsing step;
  all downstream layers are unchanged.
- **Error propagation**: today, `PREPROC004` is emitted but dropped. The
  secondary fix makes this path visible. Callers (`parse_file_with_preprocess`,
  `run_debug_parse`) must be prepared to render preprocessor warnings
  alongside parser diagnostics.
- **State lifecycle risks**: none — the fix changes pure function behaviour
  (name parsing) and the plumbing of a diagnostics vec. No persistent state.
- **API surface parity**: `parse_include_name` is `pub(crate)`-level (private
  to the crate). Call sites are only within `preprocessor.rs`. Safe to change.
- **Integration test scenarios**:
  1. Name-on-own-line + multiline named args → body expands.
  2. Name followed by tab + named args → body expands.
  3. Missing include → user sees a `[preprocess]` diagnostic, not just a
     downstream parser error.
  4. Existing single-line `{file.i arg1 arg2}` usage → unchanged.
  5. Existing quoted-name `{"path/file.i"}` usage → unchanged.

## Dependencies & Risks

- **Risk (low):** the split-pattern change could theoretically accept a name
  that was previously rejected. In practice this only means a multi-line
  include that was silently erased now expands — the intended behaviour.
  Mitigation: run the full corpus via `refine-oxabl-parser` and compare pass
  rate before/after.
- **Risk (low):** surfacing `PREPROC004` warnings will make previously
  invisible misconfigurations loud (e.g., missing include paths). This is
  desirable, but worth flagging in the PR description so reviewers expect
  more diagnostic output on corpus runs.
- **Dependencies:** none — both fixes are internal to the `oxabl_preprocessor`
  and `oxabl` crates. No new dependencies.

## Sources & References

### Internal

- `parse_include_name` — `crates/oxabl_preprocessor/src/preprocessor.rs:876`
- `find_args_start` — `crates/oxabl_preprocessor/src/preprocessor.rs:999` (the
  correct-by-accident reference implementation for name termination)
- `expand_include` entry point — `crates/oxabl_preprocessor/src/preprocessor.rs:608`
- Caller that drops empty includes — `crates/oxabl_preprocessor/src/preprocessor.rs:385`
- Silent-diagnostic guard — `crates/oxabl_preprocessor/src/preprocessor.rs:55-68`
- `parse_file_with_preprocess` (CLI integration) — `crates/oxabl/src/main.rs:272`

### Related prior plans

- `docs/plans/2026-04-13-002-feat-preprocessor-include-arguments-plan.md` — introduced include arguments; did not anticipate name-on-own-line formatting.
- `docs/plans/2026-04-15-001-fix-skip-i-files-and-preserve-undefined-preproc-refs-plan.md` — hardened the "silently erase" behaviour for undefined `{&var}` references; this plan extends the same principle to misparsed include names (via secondary diagnostic surfacing).
