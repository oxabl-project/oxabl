---
title: "fix: expand positional {N} inside &SCOPED/GLOBAL-DEFINE values (#65)"
type: fix
status: done
date: 2026-07-18
origin: GitHub #65 round-3 corpus A/B
branch: fix/inline-preproc-if-expression
---

# fix: expand positional `{N}` inside define values (#65 round 4)

## Context

Round-3 closed `PROCEDURE … IN SUPER:`. Corpus PARSE001 jumped further
(9 → 308) because more WebSpeed code now reaches `{get}`/`{set}` expansion.

Minimal repro (real `$DLC/tty/set` shape):

```abl
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
{set DataSourceEvents "evtList" TARGET-PROCEDURE}
```

Candidate expands roughly to:

```
DYNAMIC-FUNC('setDataSourceEvents':U IN {3},evtList)
```

`{3}` left literal → `Expected ')' after function arguments`.

Root cause: `set` does:

```
&IF "{3}":U = "":U &THEN &SCOPED-DEFINE ADMHdl TARGET-PROCEDURE
&ELSE &SCOPED-DEFINE ADMHdl {3} &ENDIF
…
DYNAMIC-FUNC("set{1}":U IN {&ADMHdl}, {2})
```

Round-1 expanded `{N}` in `&IF` conditions and string literals. It does **not**
expand `{N}` when storing `&SCOPED-DEFINE` / `&GLOBAL-DEFINE` **values**, so
`ADMHdl` is stored as the literal characters `{3}`. `{&ADMHdl}` then splices
`{3}` into output as a synthetic chunk (not re-scanned for positional expansion).

## Goal

Expand positional include args inside define values at define time, so
`{&ADMHdl}` resolves to the real handle (e.g. `TARGET-PROCEDURE`).

## Approach

### 1. Define-time positional expansion

In `DirectiveKind::ScopedDefine` / `GlobalDefine` handlers, when emitting and
`!positional_args.is_empty()`:

```rust
let value = expand_positional_refs(value, positional_args);
self.vars.define(name, &value);
// global: also global_vars.define
```

Top-level defines keep literal `{N}` (body-scanner parity).

### 2. `DYNAMIC-FUNC` abbreviation (companion, same commit)

Real ADE `set`/`get` emit `DYNAMIC-FUNC(...)`. Lexer only exact-matched
`dynamic-function`, so the call fell through to ordinary function-call parsing
and rejected the `IN handle` clause. Override:

```toml
[[override]]
name = "DYNAMIC FUNCTION"
min_abbreviation = "DYNAMIC-FUNC"
```

(regenerate `kind.rs` / atoms via `oxabl_codegen`.)

### Tests

1. Include: `&SCOPED-DEFINE H {3}` then `{&H}` with 3-arg include → third arg
2. `&ELSE &SCOPED-DEFINE ADMHdl {3}` after true/false `{3}` empty check (set shape)
3. Simplified `set` stub end-to-end: no literal `{3}` in output; handle present
4. Missing `{3}` → empty define value (2-arg set form uses TARGET-PROCEDURE branch)
5. Guard: define without `{N}` unchanged; multi-line `~` define still works

### Verify

```bash
cargo test -p oxabl_preprocessor
cargo clippy -p oxabl_preprocessor -- -D warnings
# smoke with simplified set stub on -I
```

### Non-goals

- Re-scan synthetic chunks from `{&var}` for nested positionals (define-time
  expansion makes this unnecessary for the set/get path)
- Full real ADE `get`/`set` fidelity (buffer-field xp{Prop} fast path, etc.)
- Re-scope corpus criterion 7

## Success

- set-shaped stub expands `IN TARGET-PROCEDURE` / `IN hProc`, not `IN {3}`
- PREPROC tests green; downstream A/B PARSE001 drops (target ≤ baseline 9)

## Risks

1. Top-level define with literal `{1}` text becomes empty — rare; matches
   include-arg empty semantics and is acceptable.
2. Named args already inject via `vars.define` at include entry — orthogonal.
