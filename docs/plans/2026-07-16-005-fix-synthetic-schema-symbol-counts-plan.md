---
title: "fix: Synthetic schema symbols accumulate read/write counts"
type: fix
status: ready
date: 2026-07-16
origin: GitHub #60
branch: fix/synthetic-schema-symbol-counts
depends_on: ["feat/semantic-schema-resolution (merged)"]
---

# fix: Synthetic schema symbols accumulate read/write counts

## Problem Statement

Schema-backed field resolution (`field_resolution` in
`crates/oxabl_semantic/src/resolve.rs`) records
`Resolution::Resolved(fsym)` for a schema-validated field but never calls
`bump_count(fsym, ...)`. The field-access arm of `resolve_field_access`
discards the access mode with `let _ = mode;`.

Synthesized default-buffer symbols **do** get counts bumped; synthesized
`Field` symbols do not.

**Consequence:** a field referenced (or assigned) any number of times dumps as
`read_count: 0, write_count: 0` in `oxabl analyze` output. Misleading for
consumers of the analyze surface, and a latent trap if any future rule keys off
field reference counts.

Not a lint bug today: LINT0002 filters to `Variable | Parameter` kinds, and
`synthetic_schema_symbols_not_reported` pins that synthetics stay out of
unused-variable.

## Goals

- Thread the real `AccessMode` through field resolution and bump counts on
  synthetic field symbols (Option 1 from #60 — preferred).
- Keep LINT0002 behavior unchanged (synthetics still not reported as unused).
- Preserve analyze dump fidelity for schema-derived field symbols.

## Non-Goals

- Changing synthetic symbol identity / naming (schema plan Open Question 1).
- New lint rules based on field counts.
- Bumping counts for `Unresolved` field refs (nothing to bump).

## Design

### Site

```
resolve_field_access
  match qresolved / schema fallback
    → field_resolution(qsym, field) → Resolution
    → let _ = mode;   // BUG: discards AccessMode

field_resolution(qsym, field)
  → synth_field_symbol(...) → Resolution::Resolved(fsym)
  → never bump_count
```

### Change

1. **`field_resolution` gains `mode: AccessMode`** so both call sites stay
   consistent.
2. On `Resolution::Resolved(fsym)` for a synthetic field, call
   `self.bump_count(fsym, mode)`.
3. **Remove `let _ = mode;`** at the resolved-qualifier arm. Also bump on the
   bare-table fallback path (today it returns early without bumping the field).
4. Default-buffer symbols already bump with `AccessMode::Read` for the
   qualifier. Field access mode may still be `Write` (assignment target) —
   that applies to the **field** symbol, not the buffer. Do not change buffer
   bumping.

### Edge cases

| Case | Expected |
|------|----------|
| `MESSAGE Customer.Name.` | field synth: `read_count += 1` |
| `ASSIGN Customer.Name = "x".` | field synth: `write_count += 1` (matching assignment-path mode) |
| Invalid field `Customer.NoSuch` | `NotInScope` — no bump |
| Temp-table buffer without schema link | `External` — no field synth, no bump |
| Deduped synth (same field twice) | same `SymbolId`; counts accumulate |

### LINT0002 invariant

`unused_variable` continues to filter by `SymbolKind::Variable | Parameter`.
Regression: a heavily-referenced synthetic field still does **not** appear as
unused.

## Implementation Steps

1. Thread `AccessMode` into `field_resolution`; call `bump_count` on resolved
   field symbols.
2. Fix both call sites in `resolve_field_access` (resolved + bare-table).
3. Tests:
   - Field with one read → `read_count == 1`.
   - Field as assignment target → `write_count == 1`.
   - Two references to same field → counts sum; single synth symbol.
   - LINT0002 still silent on synthetics.
4. `cargo test -p oxabl_semantic -p oxabl_lint -p oxabl_analyze`
5. No dedicated bench — correctness fix on existing accumulator machinery.

## Risk

**Low.** Accumulator/flush machinery already exists for buffers and locals.
Main risk is missing a call site if a third field-resolution path is added
later — grep for `field_resolution` when reviewing.

## Effort

~0.5 day.