---
title: "chore: close #65 — rescope corpus gate; track xp-property separately"
type: chore
status: done
date: 2026-07-18
origin: GitHub #65 round-4 A/B recommendation
branch: fix/inline-preproc-if-expression
---

# Close #65: preprocessor scope complete; rescope criterion 7

## Consumer finding (round-4 A/B)

| Signal | baseline | round-4 |
|--------|----------|---------|
| PREPROC002 | 596 | **0** |
| PARSE001 (deduped) | 9 | **306** |

Round-4 fixes (define-value `{N}`, `DYNAMIC-FUNC`) verified. Remaining PARSE001 is
the ADM2 **xp-property `BUFFER-FIELD` fast path** inside real `$DLC/tty/get`/`set`
when `DEFINED(xp{1})` is true — grouped ASSIGN, unquoted comma-lists as values,
xp-assign gating. Explicit **non-goal** of earlier #65 plans.

Dedup skew: baseline errors concentrate in shared includes (~9 files); post-fix
errors appear in per-file expanded bodies (~300 files × 1). Not a regression of
the four surface fixes.

## Decision (consumer vote + maintainer execution)

**Option 1 — close #65** with re-scoped success criteria:

Done when:
- PREPROC002 unclosed-`&IF` cleared on ADM2/WebSpeed corpus (596→0)
- Mid-line `&IF` / directive-body / `fn`/`fnarg` / define-value positionals work
- `PROCEDURE … IN SUPER:` parses (revealed companion)
- `DYNAMIC-FUNC` abbreviation works
- No return of original `Unexpected token Then` / empty mid-line `&IF` abort

**Not** done under #65:
- Full xp-property BUFFER-FIELD / xp-assign expansion fidelity → **new issue**

## Actions

1. Open follow-up issue: ADM2 get/set xp-property BUFFER-FIELD fast path
2. Comment on #65 summarizing rounds 1–4 and re-scoped criterion
3. Close #65
4. Optional: tiny HANDOFF/plan note that #65 is closed; pin `da7fa4d` or later
5. **No code change** this round unless a 1-line doc-only commit is wanted

## Non-goals this round

- Implement xp-property ASSIGN/comma-list parsing
- Force PARSE001 ≤ 9 on whole tree
