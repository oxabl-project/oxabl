---
title: "fix: allow '.' in preprocessor macro names for filename-based include-once guards (#74)"
type: fix
status: draft
date: 2026-07-19
origin: GitHub #74
branch: fix/dot-in-preproc-macro-names
---

# fix: allow '.' in preprocessor macro names (#74)

## Context

`&GLOBAL-DEFINE` / `&SCOPED-DEFINE` stop scanning the macro name at `.`, so a
filename-based include-once guard like `&GLOBAL-DEFINE guarded.i true` stores
the name as just `guarded` instead of `guarded.i`. A later `DEFINED(guarded.i)`
looks up the full `guarded.i` and finds no match, so the guard fails silently.
Any include reached through two paths re-expands, producing a cascade of
SEM0001 false positives.

ABL's preprocessor allows `.` and `-` in macro names. The filename-as-guard
idiom (`{guarded.i}` guards with `guarded.i`) is universal in WebSpeed/ADM.

## Root cause (confirmed)

`parse_define_body()` at `crates/oxabl_preprocessor/src/preprocessor.rs:967-974`
and `parse_undefine_body()` at lines 1064-1070 accept only `[a-zA-Z0-9_-]`.
The `.` character terminates the name scan.

The `replace_defined()` function in `condition.rs:486-490` correctly reads
through `.` (stops only at `)` or space), so `DEFINED(guarded.i)` captures the
full name — but the define was stored under the truncated name, so the lookup
always fails.

The `VarTable` has no character restrictions — it's purely the parser side.

## Ownership

| Layer | Action |
|-------|--------|
| **Preprocessor** | Own the fix — expand name charset in define/undefine |
| Condition parser | No change (replace_defined already handles `.`) |
| Lexer | No change |
| Semantic / Lint | No change (cascade fix) |

## Approach

### Slice A — Allow `.` in define/undefine macro names

Add `b'.'` to the character set in `parse_define_body()` and
`parse_undefine_body()`:

```rust
// parse_define_body — line 968-971
while i < rest.len()
    && (rest.as_bytes()[i].is_ascii_alphanumeric()
        || rest.as_bytes()[i] == b'-'
        || rest.as_bytes()[i] == b'_'
        || rest.as_bytes()[i] == b'.')
```

Same for `parse_undefine_body`.

### Slice B — Tests

Preprocessor unit tests:

1. `&GLOBAL-DEFINE` with `.` in name → `DEFINED()` sees it.
2. `&SCOPED-DEFINE` with `.` in name → `DEFINED()` sees it.
3. `&UNDEFINE` with `.` in name → name is removed.
4. `{&foo.i}` expansion (reference path, not just DEFINED).
5. Leading dot: `&SCOPED-DEFINE .foo val` → `DEFINED(.foo)` = TRUE.
6. Trailing dot: `&SCOPED-DEFINE foo. val` → `DEFINED(foo.)` = TRUE.
7. Double dot: `&SCOPED-DEFINE foo..i val` → `DEFINED(foo..i)` = TRUE.
8. Hyphen still works (regression).
9. Plain name still works (regression).
10. Include-once guard integration: `guarded.i` file reached twice via direct and transitive include → body expands once.
11. Define value starting with `.` (e.g. `&DEFINE X .field`) — `.` in value not consumed as name.

## Non-goals

- Full ABL identifier-completeness (only `.` and `-` are relevant to real corpus
  filenames; ABL allows more but fixes follow corpus demand).
- Lexer changes (the lexer's preproc keyword scanning already handles `.` in
  literal include names, but preproc directive keywords don't contain `.`).

## Risks

| Risk | Mitigation |
|------|------------|
| Define value starting with `.` parsed as part of name | Define value always has a whitespace separator between name and value. A `.` in the value won't be consumed because the name scanner stops at space. |
| Hyphenated names with dots (e.g. `my-guard.i`) | Already supported: hyphen is in the charset, adding `.` extends it. |

## Success criteria

1. `&GLOBAL-DEFINE foo.i true` → `DEFINED(foo.i)` = TRUE.
2. `&SCOPED-DEFINE bar.i x` → `DEFINED(bar.i)` = TRUE.
3. `&UNDEFINE foo.i` → `DEFINED(foo.i)` = FALSE.
4. Filename-based include-once guard works (no SEM0001 cascade).
5. Plain names, hyphen-only names still work (regression).
6. `cargo fmt` / clippy `-D warnings` / `cargo test --workspace` green.

## Related

- #74 (this issue)
- #69 merged prior — SEM0001 cascade that this issue is a root cause of
