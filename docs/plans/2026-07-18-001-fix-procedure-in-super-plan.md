---
title: "fix: parse PROCEDURE/FUNCTION … IN SUPER: prototypes (#65 follow-up)"
type: fix
status: done
date: 2026-07-18
origin: GitHub #65 comment (round-2 corpus A/B)
branch: fix/inline-preproc-if-expression
---

# fix: parse `PROCEDURE`/`FUNCTION … IN SUPER:` prototypes (#65 follow-up)

## Context

#65 preprocessor work is complete (PREPROC002 596→0 on the 9-module sample).
Corpus PARSE001 remains +48 vs baseline, but the consumer showed this is a
**pre-existing parser gap revealed by better expansion**, not a preproc
regression:

```abl
PROCEDURE assignDBRow IN SUPER:
  DEFINE INPUT PARAMETER phRowObjUpd AS HANDLE.
END PROCEDURE.
```

```
parse error: Expected ':' after procedure name   # same on baseline and 1aedb17
```

ADM2/web2 prototype includes (`smrtprto.i`, `qryprto.i`, …) are full of this
form. Once `get`/`set`/`fn`/`fnarg` expand, those includes reach the parser and
fail.

FUNCTION already accepts `IN SUPER` (skips to statement end as external ref).
PROCEDURE does not.

## Goal

Accept ABL super-procedure / external-procedure prototype headers so ADM2
prototype includes parse without error, clearing the corpus PARSE001 climb
enough to merge #65.

## Non-goals

- Full semantic model of super-procedure dispatch / cross-file binding
- New AST fields for `IN` target (v1: elide like existing EXTERNAL skip)
- Missing positional-arg token preservation in code position (separate,
  low-volume note from consumer — defer)
- Opening a separate GH issue (optional; can land on same branch as #65 gate)

## Root cause

`parse_procedure` after the name (and optional EXTERNAL / PRIVATE) requires
`:` or `.` immediately. Token `IN` (`Kind::KwIn`) fails with
“Expected ':' after procedure name”.

`SUPER` is an identifier (not a dedicated Kind). `THIS-PROCEDURE` is
`Kind::ThisProcedure`. Handle form is typically an identifier.

## Approach

### 1. `parse_procedure` — optional `IN` clause before body opener

After EXTERNAL + access-modifier handling, before the `:` / `.` check:

```
IN ( SUPER | THIS-PROCEDURE | <identifier-or-can_be_identifier> )
```

Consume tokens only (mirror EXTERNAL: no AST field). Then existing body
parse (including param DEFINEs and `END PROCEDURE.`) continues unchanged.

Accept both:

- `PROCEDURE name IN SUPER:` + body + `END PROCEDURE.`
- `PROCEDURE name IN SUPER.` (period-only header; empty body)

Also cover `IN THIS-PROCEDURE` and `IN hProc` (identifier handle).

### 2. FUNCTION

Already handles `if self.check(Kind::KwIn) { skip_to_statement_end(); … }`.
Verify `FUNCTION name RETURNS type IN SUPER.` still green; add a regression
test if missing. No change unless a body form is required (unlikely for
FUNCTION external refs).

### 3. Tests (`oxabl_parser`)

- `PROCEDURE assignDBRow IN SUPER:` + DEFINE INPUT PARAM + END PROCEDURE.
- `PROCEDURE x IN SUPER.` (period form)
- `PROCEDURE x IN THIS-PROCEDURE:` + empty body + END.
- `PROCEDURE x IN hSuper:` + END PROCEDURE.
- Smoke: FUNCTION … IN SUPER. still parses
- Guard: normal `PROCEDURE x:` without IN still works

### 4. AST / semantic

No `StatementKind::Procedure` shape change → no declare/resolve/check updates,
no `ast-invariants.md` change.

### 5. Verify

```bash
cargo test -p oxabl_parser -- procedure
cargo test -p oxabl_parser
cargo clippy -p oxabl_parser -- -D warnings
# local smoke
oxabl check /tmp/in_super.p   # exit 0
```

Downstream: pin branch head, re-run 9-module A/B; expect PARSE001 back near
baseline ≤9 and PREPROC002 stay at 0.

## Risks

1. **Over-accepting `IN`** after procedure name — low; `IN` is not valid in
   other procedure-header positions we already support.
2. **Complex handle expressions** (`IN ACTIVE-WINDOW` etc.) — `can_be_identifier`
   covers many system handles; true expression forms rare in ADM2 prototypes.
   Extend later if corpus shows them.
3. **Scope of #65** — this is parser, not preproc; still the practical gate for
   closing the issue’s corpus criterion. Land on same branch for one pin.

## Success criteria

- Minimal IN SUPER repro parses (exit 0, no “Expected ':' after procedure name”)
- Unit tests above green
- Full `oxabl_parser` suite green
- Downstream A/B: PARSE001 ≤ baseline; PREPROC002 remains 0

## Implementation order

1. Add tests (expect fail)
2. Patch `parse_procedure` IN clause
3. Green tests + clippy
4. Commit + push branch
5. Comment on #65 with pin + smoke + A/B ask
