---
title: "feat: ABL Preprocessor (oxabl_preproc crate)"
type: feat
status: active
date: 2026-04-12
---

# ABL Preprocessor (`oxabl_preproc`)

## Decision

After analysing all remaining corpus failures, the right solution is a preprocessing pass that runs *before* the parser — not a series of parser-level hacks for each pattern.

Two narrower fixes were considered and explicitly deferred:

- **`DynamicInclude` AST node** for `{{&var} args}` — would absorb the crash but produces an opaque black-box node with no analysis value. Superseded by the preprocessor, which resolves `{&var}` to its defined value and produces a real include reference.
- **Preproc-aware `parse_if_statement()`** — would allow 2 else branches when split across `&IF...&ENDIF`. Superseded by the preprocessor, which flattens the token stream so the parser never sees the split.

Doing those fixes first and then adding a preprocessor would mean ripping them out later. Build the preprocessor first.

---

## Problem

The parser operates on a raw token stream that includes unexpanded `&IF`/`&ELSE`/`&ENDIF` boundaries. This causes structural ambiguity the grammar cannot resolve:

| File | Error | Root cause |
|---|---|---|
| `oecrhead.i` | `LeftBrace` | `{{&var} args}` — dynamic include, filename is a preproc variable |
| `UPSReady.cls` | `KwElse` | ABL `IF...ELSE` split across `&IF...&ENDIF` |
| `oe100fr.p` | `PreprocElse` | DO block opened in one `&IF` branch, `END.` in a separate `&IF` block |
| `oe_deco_price_line.p` | `PreprocElse` | `OR` right-hand operand split across `&IF/&ELSE` |
| `check-vas-item.i` | `PreprocElse` | METHOD vs FUNCTION header conditionalized; body shared after `&ENDIF` |
| `perfectly_packaged_common.i` | `PreprocElseif` | Same three-way `&IF/&ELSEIF/&ELSE` pattern |

None of these are grammar problems. They are all structural ambiguities that dissolve once you know which `&IF` branches are active.

---

## Proposed Solution

A new `oxabl_preproc` crate that runs between the lexer and the parser:

```
oxabl_lexer  →  oxabl_preproc  →  oxabl_parser
```

The preprocessor takes a token stream and a `PreprocessContext`, evaluates `&IF` conditions where possible, and emits a flattened token stream with non-taken branches removed.

### `PreprocessContext`

Carries the define values known at the call site — particularly important for `.i` include files, where variables like `{&classDef}` are set by the *consumer* before including the file:

```rust
pub struct PreprocessContext {
    /// Define values established before this file was included.
    /// Key: interned define name. Value: raw value bytes.
    pub defines: HashMap<InternedString, Vec<u8>>,
}
```

The language server can populate this from workspace knowledge (prior `&SCOPED-DEFINE` / `&GLOBAL-DEFINE` statements in the including file). For standalone file parsing (e.g. `oxabl check`), start with an empty context.

### What the Preprocessor Evaluates

**Phase 1 (cover the corpus failures):**

- `&SCOPED-DEFINE name value` / `&GLOBAL-DEFINE name value` → populate symbol table
- `&UNDEFINE name` → remove from symbol table
- `&IF DEFINED(name)` → true if name is in symbol table
- `&IF "{&var}" EQ "literal"` / `"{&var}" NE "literal"` → string equality on known values
- `&IF "{&var}" GT 0` etc. → simple numeric comparison on known values (for `DEFINED()` which returns an integer)

**Phase 2 (future):**

- `DBTYPE("db")` function calls
- More complex boolean expressions in `&IF` conditions
- Nested `&IF` inside values

### Undefined Variable Handling

When a variable referenced in a `&IF` condition is not in context (not yet defined, or set by a consuming file we don't have):

- **Pessimistic inclusion**: treat all branches as potentially active and emit tokens from all of them. This is what the current parser already does implicitly — so behaviour for unknown variables is unchanged.
- Mark the `PreprocIf` node as `condition_unresolved: true` so the language server can distinguish "we evaluated this" from "we guessed."

### Token Stream Output

The flattened stream is a `Vec<Token>` with the same `Kind` enum — no new token types needed. Preproc tokens (`PreprocIf`, `PreprocElse`, `PreprocEndif`, `PreprocDefine`, `PreprocEnd`, `Preprop`) are either:

- **Consumed** (when the condition resolved and a branch was selected)
- **Passed through** (when the condition is unresolved — pessimistic mode)

The existing `PreprocIf<T>` AST nodes and `parse_preprocif_statement()` in the parser remain valid for the pass-through case. The preprocessor and parser are composable, not mutually exclusive.

---

## Workspace Structure

```
crates/
  oxabl_preproc/
    src/
      lib.rs          — public API: PreprocessContext, preprocess(tokens, source, ctx) -> Vec<Token>
      evaluator.rs    — &IF condition evaluation
      symbol_table.rs — define/undefine tracking
    Cargo.toml
```

`oxabl_parser` gains `oxabl_preproc` as an optional dependency. The `oxabl` top-level crate wires them together.

---

## Acceptance Criteria

### Phase 1

- [ ] `oxabl_preproc` crate created with `preprocess()` public API
- [ ] `PreprocessContext` with `HashMap<InternedString, Vec<u8>>` defines
- [ ] Symbol table updated from `PreprocDefine` tokens in stream order
- [ ] `&IF DEFINED(name)` evaluation correct for defined and undefined names
- [ ] `&IF "{&var}" EQ/NE "literal"` evaluation correct for known values
- [ ] Undefined variable → pessimistic (all branches passed through, `condition_unresolved` flagged)
- [ ] Corpus check improves: `oe100fr.p`, `oe_deco_price_line.p`, `check-vas-item.i`, `perfectly_packaged_common.i` all pass
- [ ] `oecrhead.i` and `UPSReady.cls` pass (these also resolve once `{&include-prog}` and `DBTYPE()` are handled, or via pessimistic passthrough)
- [ ] All existing 428 parser tests still pass (preprocessor is additive, not breaking)
- [ ] `cargo fmt`, `cargo clippy`, `cargo test` clean

### Phase 2 (future)

- [ ] `DBTYPE("db")` function evaluation
- [ ] Language server integration: `PreprocessContext` populated from workspace symbol table

---

## Sources

- Corpus failure analysis: [docs/solutions/corpus-remaining-failures.md](../solutions/corpus-remaining-failures.md)
- Prior preprocessor design decisions: [docs/brainstorms/2026-04-04-parser-gaps-brainstorm.md](../brainstorms/2026-04-04-parser-gaps-brainstorm.md)
- Prior preprocessor AST implementation: [docs/plans/2026-04-04-006-feat-parser-gaps-db-preproc-streams-plan.md](2026-04-04-006-feat-parser-gaps-db-preproc-streams-plan.md)
