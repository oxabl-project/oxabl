---
title: "fix: bind FUNCTION signature parameters into function scope (#68)"
type: fix
status: active
date: 2026-07-18
origin: GitHub #68
branch: fix/function-signature-params-lint0001
---

# fix: bind FUNCTION signature parameters into function scope (#68)

## Context

Downstream consumer reports `LINT0001` (undefined-symbol) for **every reference
to a user-defined FUNCTION's own parameters**. PROCEDURE body parameters
(`DEFINE INPUT PARAMETER …`) and METHOD signature parameters resolve correctly.
The gap is FUNCTION-specific.

Minimal repro:

```abl
FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER):
  RETURN sValue + "x".
END FUNCTION.
```

```
oxabl analyze --preprocess func_params.p
# LINT0001 undefined symbol `sValue`
```

Local confirmation on master (`abffad8`): analyze emits one LINT0001 for
`sValue`; the function scope has **zero** bindings; only the function symbol
`getval` is declared in the file scope.

## Root cause (confirmed)

`parse_function` in `crates/oxabl_parser/src/parser/statements.rs` **skips** the
parenthesized parameter list instead of parsing it:

```rust
// Optional parameter list in parentheses
if self.check(Kind::LeftParen) {
    self.advance();
    // Skip parameter declarations inside parens for now
    // Full parameter parsing would need its own implementation
    // Parameters are typically re-declared in the body with DEFINE INPUT PARAMETER
    let mut depth = 1;
    while depth > 0 && !self.at_end() { … }
}
```

So signature params never enter the AST. Semantic `declare_pass` already opens a
`ScopeKind::Function` and walks `body` — it correctly declares
`DefineParameter` when present (METHOD does this via a separate `parameters`
field). With params dropped at parse time, every body reference is
`not_in_scope` → LINT0001.

The AST already documents the intended shape:

```rust
/// Function body statements (parameters are parsed as DEFINE PARAMETER).
body: Vec<Statement>,
```

And `parse_parenthesized_params` (used by METHOD/CONSTRUCTOR) already produces
exactly those `DefineParameter` statements.

## Ownership

| Layer | Action |
|-------|--------|
| **Parser** | Own the fix — stop skipping; parse params |
| **AST** | No shape change — prepend `DefineParameter` stmts onto `body` (matches existing comment) |
| **Semantic** | No change required — existing `walk_block(body, fn_scope)` + `declare_parameter` |
| **Lint** | No change — LINT0001 correctly reports unresolved refs |

## Approach

### Slice A — Parse FUNCTION signature params into body

In `parse_function`:

1. When `(` is present, call `self.parse_parenthesized_params()?` instead of the
   paren-skip loop.
2. For full definitions (body after `:`/`.`), **prepend** the param statements
   to `body` before returning the `Function` node.
3. For prototypes (`FORWARD` / `IN …` / `MAP TO …`), parse params for fidelity
   then discard (prototype body is empty; params unused for #68). Prefer
   discard to avoid inventing scope for prototype-only symbols that would
   collide with the later definition's params (see #69 non-goal note).
4. Update `parse_parenthesized_params` doc comment to mention FUNCTION.

### Slice A′ — TABLE-HANDLE in parenthesized params (Fable amendment)

Add a `TABLE-HANDLE` branch mirroring `DATASET-HANDLE` in
`parse_parenthesized_params`. `Kind::TableHandle` already exists; without this
branch, `TABLE-HANDLE x` falls through to `parse_identifier` then fails on
"Expected AS or LIKE". The old skip loop tolerated it for FUNCTION; strict
parsing newly exposes the gap (also latent for METHOD).

### Slice B — Tests

Parser:

- Update `parse_function_with_params` to assert body starts with two
  `DefineParameter` statements then `Return`.
- Empty param list: `FUNCTION f RETURNS INT (): …` still parses.
- Prototype forms keep empty body:
  - `FUNCTION f RETURNS CHAR (INPUT a AS CHAR) FORWARD.`
  - `FUNCTION f RETURNS CHAR (INPUT a AS CHAR) IN SUPER.`
- TABLE-HANDLE param parses (METHOD or FUNCTION).

Semantic / lint (e2e):

- Fixture: FUNCTION with signature params referenced in body → **0** LINT0001.
- Fixture: multi-param + local var → params and local clean; unknown name still
  fires LINT0001 (negative).
- Optional unit: declare_pass on Function body-with-DefineParameter binds in
  Function scope (mirrors existing method param test).

### Non-goals

- **#69** FUNCTION FORWARD/IN SUPER + definition double-declare (SEM0001) —
  separate issue; do not change declare collision logic here.
- Adding a separate `parameters: Vec<Statement>` field on `Function` (symmetry
  with Method) — unnecessary for the fix; AST comment already says params live
  in body. Can refactor later if desired.
- PROCEDURE signature forms that already use body `DEFINE PARAMETER`.
- Changing LINT0001 itself.
- Exempting Function INPUT params from LINT0002 (see side effects).

## Downstream smoke

```bash
# clean
cat > /tmp/func_params.p << 'EOF'
FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER):
  RETURN sValue + "x".
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/func_params.p
# expect: diagnostics [] (no LINT0001)

# multi-param + local
cat > /tmp/func_params2.p << 'EOF'
FUNCTION calc RETURNS INTEGER (INPUT a AS INTEGER, INPUT b AS INTEGER):
  DEFINE VARIABLE r AS INTEGER NO-UNDO.
  r = a + b.
  RETURN r.
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/func_params2.p
# expect: no LINT0001 for a/b/r

# still fires for truly undefined
cat > /tmp/func_undef.p << 'EOF'
FUNCTION f RETURNS INTEGER (INPUT a AS INTEGER):
  RETURN a + missingName.
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/func_undef.p
# expect: LINT0001 only for missingName
```

## Side effects (call out to consumer)

- **LINT0002 unused-variable**: Parameters become real symbols. Unused INPUT
  params in FUNCTION bodies may newly fire LINT0002 (same as METHOD). OUTPUT /
  INPUT-OUTPUT remain exempt. Expect LINT0001↓ and possible small LINT0002↑.
- **PARSE001**: Strict param parsing may fail on exotic signature soup the old
  skip loop accepted. Track PARSE001 in corpus A/B; only add rewind-and-skip
  fallback if corpus shows real breakage beyond TABLE-HANDLE.

## Risks

| Risk | Mitigation |
|------|------------|
| Prepending params changes body length / statement order for consumers | Only additive prefix of synthetic DefineParameter; mirrors METHOD semantics |
| Prototype params discarded after parse | Same as today (skipped); no new symbols |
| Nested parens in param defaults | ABL function params don't have defaults; existing parser handles TABLE/BUFFER |
| Collision if body also has `DEFINE INPUT PARAMETER` with same name | Pre-existing ABL pattern; SEM0001 would correctly fire — out of scope |
| Strictness regression on unmodeled signature forms | TABLE-HANDLE branch + corpus PARSE001 gate |

## Success criteria

1. Minimal repro: **0** LINT0001 for own parameters.
2. Negative: still LINT0001 for unbound names in function body.
3. `cargo fmt`, `cargo clippy --workspace --all-targets -- -D warnings`,
   `cargo test --workspace` green.
4. Downstream corpus A/B: material drop in LINT0001 on FUNCTION-heavy modules;
   PARSE001 must not regress vs pin baseline (consumer re-run).

## Related

- #68 (this issue)
- #69 SEM0001 FORWARD/IN SUPER (next)
- #58 LINT0001 residual language gaps (built-ins/schema; not function params)
