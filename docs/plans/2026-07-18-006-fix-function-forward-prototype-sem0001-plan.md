---
title: "fix: reconcile FUNCTION FORWARD/IN SUPER prototype + definition (SEM0001) (#69)"
type: fix
status: active (partially superseded — see plan 007 for multi-prototype reconciliation)
date: 2026-07-18
origin: GitHub #69
branch: fix/function-forward-prototype-sem0001
---

# fix: reconcile FUNCTION FORWARD/IN SUPER prototype + definition (SEM0001) (#69)

## Context

Downstream consumer reports `SEM0001` ("already declared in this scope") for
every user-defined `FUNCTION` that is **forward-declared then defined** in the
same compilation unit — the dominant WebSpeed/ADM2 pattern:

```abl
FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER) FORWARD.

FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER):
  RETURN sValue.
END FUNCTION.
```

Same with `IN SUPER.` instead of `FORWARD.`.

A prototype **without** a local definition (`FUNCTION … IN hHandle.`) is
correctly single-declared — no SEM0001. The bug is only the
prototype-plus-definition pairing counted as two independent declarations.

Local confirmation on the #68 tip: `func_fwd.p` and `in_super.p` each emit one
SEM0001 on the definition's name; `external_only.p` is clean for SEM0001.

Stacks on #68 (`fix/function-signature-params-lint0001`): after #68, prototypes
still have **empty** `body`; full definitions have signature params (+ stmts)
in `body`, so emptiness remains a reliable prototype signal.

## Root cause (confirmed)

`declare_pass` treats every `StatementKind::Function` as a fresh insert into
`NamespaceId::Functions`. Both the prototype and the later definition call
`declare()`; the second hits the same-scope duplicate check and emits SEM0001.

No concept of "forward / incomplete" function symbols exists today.

## Ownership

| Layer | Action |
|-------|--------|
| **Semantic declare** | Own the fix — reconcile Function re-declarations |
| Parser / AST | No change (empty body already marks prototypes after #68) |
| Lint | No change |

## Approach

### Slice A — `SymbolFlags::PROTOTYPE`

Add `const PROTOTYPE = 1 << 16` on `SymbolFlags`. Set it when declaring a
`Function` whose `body` is empty (FORWARD / IN … / MAP TO, and any empty-body
form). Dump serializer in `oxabl_analyze` lists the new flag.

### Slice B — Function re-declaration reconciliation in `declare()`

When a same-scope same-namespace insert would collide and **both** prior and
incoming kinds are `SymbolKind::Function` **and** the incoming statement is
`StatementKind::Function` (not Method — Methods share `SymbolKind::Function` /
`NamespaceId::Functions` but never set PROTOTYPE):

| Prior | Incoming | Action |
|-------|----------|--------|
| PROTOTYPE | definition (non-empty body) | **Merge**: clear PROTOTYPE, set `declaration`/`name_span`/`data_type` from definition; return prior `SymbolId` (no SEM0001) |
| definition | PROTOTYPE | **Ignore** prototype; return prior (no SEM0001) |
| PROTOTYPE | PROTOTYPE | **Merge** (idempotent — plan 007 supersedes original SEM0001 intent) |
| definition | definition | **SEM0001** (true duplicate) |
| any (Method/Function) | Method | **SEM0001** (unchanged — Methods do not set/consume PROTOTYPE) |

Gate on `StatementKind::Function` for PROTOTYPE set/merge so two same-name
METHODs still SEM0001.

The Function walker still opens a `ScopeKind::Function` and walks `body` for
the definition statement after a successful merge return — parameters and
locals bind as today (#68).

### Slice C — Tests

Semantic unit:

1. FORWARD + definition → 0 SEM0001, one Function symbol, declaration node is
   the definition.
2. IN SUPER + definition → 0 SEM0001.
3. Prototype only (`IN h`) → 0 SEM0001, one symbol (unchanged).
4. MAP TO prototype only → 0 SEM0001, one symbol.
5. Two full definitions of same name → still SEM0001.
6. Two FORWARD prototypes of same name → merge (0 SEM0001 — plan 007 changed this).
7. Definition then FORWARD (reverse order) → 0 SEM0001, keep definition.
8. FORWARD + IN SUPER + definition → 0 SEM0001 (plan 007).
9. Two same-name METHODs in a class → still SEM0001 (guard shared kind).

E2e / analyze smoke (optional in lint tests or semantic):

```abl
FUNCTION getVal … FORWARD.
FUNCTION getVal … : RETURN sValue. END FUNCTION.
```
→ no SEM0001; with #68, no LINT0001 on `sValue`.

### Non-goals

- PROCEDURE forward forms (not reported).
- Overload resolution by signature (ABL user functions are not overloaded by
  param types in the same unit — name is the key).
- METHOD … FORWARD. reconciliation — parser supports empty-body method
  prototypes; same false-positive shape may exist for class methods. Follow-up
  candidate, not in #69 scope. Guard test keeps method duplicates as SEM0001.
- #68 param binding (already on base branch of this stack).

## Downstream smoke

```bash
cat > /tmp/func_fwd.p << 'EOF'
FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER) FORWARD.

FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER):
  RETURN sValue.
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/func_fwd.p
# expect: no SEM0001; no LINT0001 for sValue

cat > /tmp/in_super.p << 'EOF'
FUNCTION getVal RETURNS CHARACTER (INPUT s AS CHARACTER) IN SUPER.

FUNCTION getVal RETURNS CHARACTER (INPUT s AS CHARACTER):
  RETURN s.
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/in_super.p
# expect: no SEM0001

# still flags true duplicates
cat > /tmp/func_dup.p << 'EOF'
FUNCTION f RETURNS INTEGER:
  RETURN 1.
END FUNCTION.
FUNCTION f RETURNS INTEGER:
  RETURN 2.
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/func_dup.p
# expect: SEM0001
```

## Risks

| Risk | Mitigation |
|------|------------|
| Empty-body full definition treated as prototype | Rare; completing it with a second non-empty def merges cleanly; two empty defs would SEM0001 only if both empty as two prototypes — accept |
| Prototype flag leaks into dumps | Document in flag list; additive string only; bump symbols dump version only if repo convention requires it |
| Stack depends on #68 | PR targets / stacks on #68 branch; empty-body signal holds on master too for FORWARD/IN (no params in body either way) |

## Success criteria

1. FORWARD + definition: **0** SEM0001.
2. IN SUPER + definition: **0** SEM0001.
3. True duplicate definitions: still SEM0001.
4. External-only prototype: still clean.
5. With #68: function body param refs clean (LINT0001).
6. `cargo fmt` / clippy `-D warnings` / `cargo test --workspace` green.
7. Downstream corpus: large drop in SEM0001 on WebSpeed/ADM modules.

## Related

- #69 (this issue)
- #68 FUNCTION signature params (stack base)
- #65 PROCEDURE … IN SUPER companion (parser only)
