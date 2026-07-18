---
title: "Status: open issues, #58 progress, recommended next focus"
type: status
status: active
date: 2026-07-16
---

# Status: open issues & recommended next focus

Snapshot after exploration on 2026-07-16 (master ≈ include-path merge; SHARED
feature mid-merge in working tree — **do not stomp that merge**).

## Open GitHub issues

| # | Title | Ready to execute? | Plan |
|---|-------|-------------------|------|
| **60** | Synthetic schema symbols never accumulate R/W counts | **Yes** — clear fix site | `2026-07-16-005-fix-synthetic-schema-symbol-counts-plan.md` |
| **58** | LINT0001 FPs (built-ins, schema, shared/include) | **Mostly shipped** — close or narrow | See below |
| **57** | Custom lint rule registry / selection | **Yes** | `2026-07-16-006-feat-lint-rule-registry-plan.md` |
| **56** | Dependency fidelity + XREF harness | **Yes, phased** | `2026-07-16-008-feat-dependency-fidelity-xref-plan.md` |
| **55** | Public API improvements | **Yes, phased** | `2026-07-16-007-feat-public-api-improvements-plan.md` |

Perf roadmap (not a GH issue): `2026-07-16-009-perf-lexer-parser-semantic-throughput-plan.md`.

## #58 progress (LINT0001 accuracy)

| Work item | Status |
|-----------|--------|
| Built-in function registry | **Merged** |
| Built-in abbreviations (AVAIL, …) | **Merged** (#59) |
| Schema-backed field/table resolution | **Merged** |
| Include-path config + loud PREPROC007 | **Merged** (#62) |
| Within-file SHARED flags on var/tt/buffer | **In flight** (merge conflict on `resolve.rs` / `CLAUDE.md` in this tree) |
| Cross-file SHARED | Deferred (needs workspace symbol index) |
| System handles as always-defined (`SESSION`, `ERROR-STATUS`, …) | Likely residual FP class — not yet a dedicated plan |
| Widget/attribute names, OO methods | Still External / Unknown — intentional for v1 |

**Recommendation:** once SHARED merge lands, re-run corpus lint counts, update
#58 with remaining FP classes, and either close #58 as "v1 accuracy done" or
split residuals into new issues (system handles, cross-file SHARED).

## Recommended product focus (next)

You said lint + semantic analysis is the next focus. Agree. Suggested order:

### 1. Finish accuracy plumbing (days, not weeks)

1. Land SHARED merge cleanly (in progress — leave to that session).
2. Ship **#60** (half day) — dump fidelity; cheap.
3. Optional residual: system-handle registry for LINT0001 (similar to builtins).
4. Re-measure corpus: findings should collapse from ~12.8M toward signal.

### 2. Make lint a product command + fast enough

1. **`oxabl lint`** over directories (today `check` is parse-only serial;
   `analyze` is single-file full pipeline).
2. Parallelize per-file analysis (rayon).
3. Single shared lint AST walk (perf plan P0.2).
4. **#57** rule registry so domain rules and `--disable` land cleanly.

### 3. Unblock real multi-include consumers (#55 Phase B)

Virtual span resolution + diagnostic renderer. Without this, semantic/lint on
include-heavy ERP code points at wrong locations — blocks adoption even when
resolution is correct.

### 4. Dependency fidelity for build tools (#56 + #55 Phase C)

After lint is usable: skipped-branch includes, dynamic flags, then optional
XREF harness.

### 5. Throughput investments once baseline measured

See perf plan: O(1) child scope, schema field hash, dense count vec, then PHF
keyword match if CodSpeed still shows it. Avoid declare+resolve merge and arena
AST until profiles demand them.

## Performance thesis (short)

| Layer | State | Next lever |
|-------|-------|------------|
| Lexer | Strong (stack fold, prealloc) | PHF keyword match; drop string `to_string` before intern |
| Parser | Feature-complete, alloc-heavy | Identifier/`String` design; not the product bottleneck vs 8 AST walks |
| Semantic | 3–4 walks + linear field scan | Scope map O(1); field hash index; kill class-upgrade walk |
| Lint | 3 redundant AST walks | **Merge walks**; then registry |
| CLI | Serial parse-only `check` | **Parallel full pipeline `lint`** — biggest wall-clock win |

Lexer/parser micro-opts matter for CodSpeed pride and long-term; **serial file
orchestration + multi-walk lint** dominate whole-corpus lint time today.

## Permissions / AFK notes

No further permission needed for:

- Reading code, writing docs/plans (done).
- Implementing #60, #57, perf P1.x on clean branches off master.

Ask before:

- Force-push, merge --abort of the in-progress SHARED merge, or resolving those
  conflicts if another session owns them.
- Publishing crates to crates.io (#55 Phase A).
- Adding OpenEdge-dependent CI jobs (#56 Phase 4).
- Large AST redesigns (Identifier atoms, arena AST).

## Plan index (this session)

| File | Topic |
|------|-------|
| `docs/plans/2026-07-16-005-fix-synthetic-schema-symbol-counts-plan.md` | #60 |
| `docs/plans/2026-07-16-006-feat-lint-rule-registry-plan.md` | #57 |
| `docs/plans/2026-07-16-007-feat-public-api-improvements-plan.md` | #55 |
| `docs/plans/2026-07-16-008-feat-dependency-fidelity-xref-plan.md` | #56 |
| `docs/plans/2026-07-16-009-perf-lexer-parser-semantic-throughput-plan.md` | Perf |
| `docs/plans/2026-07-16-010-status-open-issues-and-next-focus.md` | This file |
