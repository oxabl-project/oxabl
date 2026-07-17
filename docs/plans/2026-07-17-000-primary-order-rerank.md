---
title: "Primary order re-rank after #64 + #58 corpus re-run"
type: status
status: active
date: 2026-07-17
---

# Primary order re-rank (2026-07-17)

## Ranking

| Rank | Issue | Why |
|------|-------|-----|
| **1** | **#64** undefined `{&macro}` → empty | Preprocessor bug; dominant shared-global LINT0001 driver; declarations never enter AST |
| **2** | **#58 residuals** A–E | Language coverage after builtins/schema/SHARED; ~39% of unique LINT0001 |
| 3 | #60 synthetic field counts | Small correctness; analyze dump fidelity |
| 4 | #55 Phase B (virtual spans + renderer) | Unblocks multi-include diagnostic UX |
| 5 | #57 rule registry | Ecosystem; not blocking accuracy |
| 6 | #56 dependency fidelity | Build-tool track; after lint signal is good |
| 7 | #55 rest / perf | Parallel when accuracy plateaus |

## #58 status

**Done:** builtin functions + abbrevs, schema fields, include paths + PREPROC007,
within-file SHARED flags.

**This cycle:** residual comment items (handles, property SET param, QUERY
handle syntax, static class receivers, no/SUBSTR/NO-LOCK).

**Later:** ambient cross-scope (~61% unique) — needs multi-file or ambient API.

## Execution this session

1. Implement #64 (`fix/undefined-preproc-macro-empty`).
2. Implement #58 residuals (`feat/lint0001-residual-gaps`).
3. Review, test, commit (no GPG).
