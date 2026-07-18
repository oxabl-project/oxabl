---
title: "perf: Lexer / parser / semantic / lint throughput roadmap"
type: perf
status: ready
date: 2026-07-16
branch: perf/<area>-<change>  # one focused PR per item
related:
  - docs/plans/2026-04-11-001-perf-parser-lexer-regression-fixes-plan.md
  - docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md
---

# perf: Throughput roadmap (lexer, parser, semantic, lint)

## Context

Oxabl is past "can we parse the corpus?" and into **whole-codebase lint +
semantic analysis**. Accuracy work (LINT0001 FPs, schema, includes) is landing;
the next force multiplier is **throughput** so 10k-file corpora finish in
seconds, not minutes.

This plan synthesizes:

- Deepseek-v4-pro read-only exploration of lexer + semantic/lint (2026-07-16)
- Prior CodSpeed work (token Vec prealloc, stack keyword fold, AssignPair SmallVec)
- Local review of hot paths under active tree (including mid-flight SHARED merge)

**Principle:** measure with CodSpeed, ship one change per PR, prefer structural
wins (fewer AST walks, better indexes) over micro-opts until profiles demand
them.

---

## Already done — do not re-litigate

| Win | Approx effect |
|-----|----------------|
| `match_keyword` stack `[u8; 64]` ASCII fold (no `to_lowercase`) | ~20% lexer |
| `tokenize` `Vec::with_capacity(len/5)` | ~18–24% recovered realloc |
| Parser dispatches on `Kind` not strings | ~8% parse |
| `AssignPair` `SmallVec<[_; 4]>` | expressions bench |
| `BindingMap` SmallVec≤8 spill to FxHashMap | semantic scopes |
| Dense `NodeIndexVec` / `SymbolId` arenas | semantic side tables |
| `fold_atom` stack buffer | schema + resolve |

---

## Priority ranking (impact × confidence / effort)

### P0 — Whole-codebase product path (lint focus)

#### 0.1 Batch `check` / future `lint` command: parallelize files

**Today:** `oxabl check` walks files **serially** and only **parses** (no
semantic/lint). `oxabl analyze` is single-file full pipeline.

**Work:**

1. Add `oxabl lint` (or extend `check`) that runs preprocess → parse →
   `analyze_file` → `lint_file` per file.
2. `rayon` (or equivalent) over independent files; share one loaded `Schema`
   and include-path config.
3. Progress bar + aggregated diagnostics; exit code non-zero on errors.

**Why first:** 10k × per-file cost is wall-clock dominated by **serial
orchestration**, not micro-ops inside one file. 8 cores ≈ linear speedup for
independent files.

**Risk:** medium (CLI UX, diagnostic ordering, panic isolation already partially
present via `catch_unwind` on tokenize).

**Effort:** 1–2d.

#### 0.2 Single shared lint AST walk

**Today:** `lint_file` runs 4 rules; 3 re-walk the full AST with near-duplicate
`walk_statement` dispatchers (`undefined_symbol`, `unknown_table_or_field`,
`type_mismatch_assignment`). `unused_variable` correctly scans symbols only.

**Work:** one `LintWalker` (or registry-driven visitor from #57) producing all
walk-based diagnostics in a single pass.

**Gain:** ~40–60% of lint-only time (eliminates 2 redundant full walks).

**Risk:** low. Rules are side-table driven and stateless across scopes.

**Effort:** 1d. Coordinate with #57 so `Rule` can be either "walk-based
callback" or "table-scan only".

---

### P1 — Semantic pass structure

#### 1.1 O(1) `find_child_scope`

**Today:** linear scan of all scopes per scope-introducing statement in both
resolve and check (`resolve.rs` / `check.rs`).

**Work:** side map `NodeId → ScopeId` filled at declare time, or store child
scope id when pushing.

**Gain:** 5–15% on deep/nested files.

**Risk:** low.

**Effort:** 0.5d.

#### 1.2 Eliminate `collect_class_upgrades` pre-walk

**Today:** full AST walk before resolve only to upgrade `DataType::Class` types.

**Work:** defer `(SymbolId, class_name, scope)` during declare; resolve after
Types namespace is populated.

**Gain:** one fewer full walk per file (~5–10% of resolve path).

**Risk:** low.

**Effort:** 0.5–1d.

#### 1.3 Dense `Vec` for read/write count accumulators

**Today:** `FxHashMap<SymbolId, (u32, u32)>` in `ResolveWalker`.

**Work:** `Vec<(u32,u32)>` indexed by dense `SymbolId`.

**Gain:** small but free; cleaner hot path for every reference.

**Risk:** low. **Also the right substrate for #60** (field count bumps).

**Effort:** 0.25d.

#### 1.4 Schema `Table::get_field` hash index

**Today:** linear scan of fields (`schema.rs`).

**Work:** `FxHashMap<OxablAtom, usize>` per table (lazy or load-time).

**Gain:** 5–20% on schema-heavy resolve (ERP tables with 50–100+ fields).

**Risk:** low.

**Effort:** 0.5d.

#### 1.5 (Later) Merge declare + resolve single walk

**Gain:** large (~35–50% semantic) but **highest risk** — only after 1.1–1.4
and good benches. Scope ordering invariants are subtle.

**Effort:** 2–3d.

---

### P2 — Lexer

#### 2.1 PHF for `match_keyword` (open from 2026-04-11 plan Fix 4)

**Today:** length-bucketed match with sequential byte equality; was ~19.7% of
`full_program` in prior flamegraphs.

**Work:** codegen emits `phf::Map` (or `phf_shared` perfect hash) after stack
ASCII fold.

**Gain:** estimated 5–10% full pipeline; larger on keyword-dense fixtures.

**Risk:** low–medium (codegen + dep). Measure carefully — modern LLVM sometimes
optimizes big matches well; **require CodSpeed before/after**.

**Effort:** 1–2d.

#### 2.2 String literal: `OxablAtom::from(&str)` without `to_string()`

**Site:** `lib.rs` StringLiteral arm — `source[...].to_string()` then intern.

**Work:** `OxablAtom::from(&self.source[start+1..content_end])`.

**Gain:** moderate on string-heavy files; free correctness-wise.

**Risk:** very low.

**Effort:** 0.25d.

#### 2.3 Byte cursor / cheaper `peek` (optional)

**Today:** `peek()` clones `Chars` per call in identifier loops.

**Work:** maintain `pos: usize` over `source.as_bytes()` for ASCII hot path.

**Gain:** 3–7% keywords; medium risk (UTF-8 in strings/comments).

**Effort:** 1–2d. Do only if flamegraph still shows iterator cost after 2.1–2.2.

#### 2.4 `u32` token offsets (optional)

Shrink `Token` for cache locality in parser. ABL sources ≪ 4GB.

**Effort:** 0.5d; touches many types — batch carefully.

---

### P3 — Parser / AST memory (throughput via alloc pressure)

#### 3.1 Identifier / name storage

**Today:** `Identifier { name: String }` — heap per identifier; resolve
re-folds via `fold_atom(&name)`.

**Directions (pick one later):**

- Store `span` only + resolve from source slice (zero name alloc at parse), or
- Store pre-folded `OxablAtom` at parse/declare time.

**Gain:** large alloc reduction; helps semantic too.

**Risk:** medium (API / Display / tests). Design doc before coding.

#### 3.2 Expression `Box` tax

Every binary op is two `Box<Expression>`. Arena/bump allocation or
index-based AST (oxc-style) is a **major** rewrite — defer until profiles show
AST alloc dominant vs walks.

#### 3.3 Reduce `Token::clone` / `to_string` in statement parsers

Inventory shows many clones when building names. Prefer span-based slices.

---

## Measurement plan

| Layer | Existing | Add |
|-------|----------|-----|
| Lexer | `lexer_bench` fixtures | micro: `match_keyword` only; string-heavy repeat |
| Parser | `parser_bench` | keep full_program as gate |
| Semantic | declare/resolve/check/analyze_file + schema_heavy | large synthetic file; 20×40 field schema; lint_file group |
| Product | none | `oxabl lint` wall-clock on N-file corpus (manual or CI nightly) |

Always: CodSpeed on PR; one optimization per PR for attribution.

---

## Suggested implementation sequence (lint-first)

```
Week 1
  ├─ #60 field counts (correctness, tiny)
  ├─ P1.1 find_child_scope
  ├─ P1.3 dense count vec
  └─ P1.4 schema field index

Week 2
  ├─ P0.2 single lint walk  (+ optionally #57 registry)
  ├─ P1.2 kill class-upgrade pre-walk
  └─ P0.1 parallel oxabl lint CLI

Week 3+
  ├─ P2.2 string atom no to_string
  ├─ P2.1 PHF match_keyword (measure!)
  └─ P3.1 Identifier design spike
```

---

## What not to do yet

- SIMD keyword matching (PHF first).
- Merging declare+resolve before O(1) scope lookup and benches.
- Arena AST rewrite without a dedicated design + multi-week budget.
- Premature cross-file index optimization before cross-file semantics exist.
- Optimizing `oxabl check` parse-only path as if it were lint — product gap
  first (P0.1).

---

## Success criteria

- CodSpeed: no unexplained regressions; targeted benches show expected wins.
- Wall-clock: full semantic+lint of a ~10k-file corpus on 8 cores under a
  developer-acceptable budget (target: **under 30s** cold, better warm — refine
  once baseline measured).
- Lint remains correctness-first: throughput work must not reintroduce LINT0001
  false positives.
