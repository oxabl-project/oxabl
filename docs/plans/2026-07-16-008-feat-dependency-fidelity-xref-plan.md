---
title: "feat: Dependency-extraction fidelity + XREF-diff harness"
type: feat
status: ready
date: 2026-07-16
origin: GitHub #56
related: ["GitHub #55 Phase C", "docs/plans/2026-07-16-003-feat-analysis-include-path-config-plan.md"]
branch: feat/dependency-fidelity-phase-N
---

# feat: Dependency-extraction fidelity + XREF-diff harness (#56)

## Framing

When parse-derived dependencies drive **build invalidation**, failure modes are
asymmetric:

- **Over-report** → wasteful rebuild (safe).
- **Under-report** → missed rebuild → stale artifacts (dangerous, silent).

Guiding principle: **anything static analysis cannot resolve must be explicit
and conservative** — a loud "unresolvable / dynamic" signal, never silent
omission.

## What's already in place

| Area | Status |
|------|--------|
| Include expansion + PROPATH first-match-wins | Implemented (`FileSystem::resolve_include`) |
| Loud unresolvable include (PREPROC007) + `oxabl.toml` paths | Shipped (#62 / plan 003) |
| Schema-backed table/field resolution (single-file) | Shipped |
| Dead-branch / skipped include enumeration | **Missing** (#55.25) |
| Dynamic construct flags | **Missing** |
| Table/buffer dependency surface | **Partial** (AST + semantic symbols exist; no dedicated query API) |
| XREF-diff harness | **Missing** |

## Goals

- **G1.** Document and test PROPATH + preprocessor fidelity vs AVM docs (and
  corpus notes where AVM behavior is tribal).
- **G2.** Expose dynamic/unresolvable sites as first-class API signals.
- **G3.** Expose referenced tables/buffers (static vs dynamic).
- **G4.** Optional in-repo XREF-diff harness gated on OpenEdge install.

## Non-Goals

- Replacing the OpenEdge compiler.
- Perfect static resolution of `RUN VALUE(...)` (impossible — flag dynamic).
- Full cross-file SHARED graph (separate roadmap).

---

## Phase 1 — Document + pin resolution fidelity (~1–2d)

### PROPATH

- Document order: CLI `-I` vs `oxabl.toml` merge order (already defined in
  `resolved_include_paths`) vs AVM PROPATH.
- Tests: two files same name on different path entries → first wins; absolute
  vs relative; cwd anchoring.
- Fix only if a test finds divergence.

### Preprocessor conditionals

- Document define precedence (`&GLOBAL` vs `&SCOPED`), undefined name handling,
  builtin preproc values we support vs gap list.
- Targeted tests for branch selection that changes which include is expanded.

**Deliverable:** `docs/design/dependency-fidelity.md` + tests under
`oxabl_preprocessor` / `oxabl_workspace`.

---

## Phase 2 — Dynamic / unresolvable surface (~2–3d)

AST or side-table signals (prefer **side table / query API** over reshaping
every node):

```rust
pub enum DependencyKind {
    Include { path: PathBuf, static: bool },
    Run { target: RunTarget },       // static name vs VALUE
    Table { name: OxablAtom, static: bool },
    Buffer { name: OxablAtom, table: Option<OxablAtom> },
}

pub struct DependencySet {
    pub resolved: Vec<Dependency>,
    pub unresolvable: Vec<UnresolvableDep>, // dynamic or missing
}
```

Minimum constructs to flag:

- `RUN VALUE(...)` / dynamic `RUN`
- Dynamic buffer/query (`CREATE BUFFER`, `CREATE QUERY`, handle-based)
- Dynamic / unresolved includes (wire to PREPROC007 + structured list)
- Optional: `DYNAMIC-FUNCTION`, `NEW` of external class (already
  `UnresolvedReason::External` in semantic)

**API entry:** e.g. `oxabl_analyze::dependencies(program, sem, preproc) ->
DependencySet` or a dedicated small crate module under `oxabl` umbrella.

**Overlaps #55 Phase C** — implement skipped_includes there, consume here.

---

## Phase 3 — Table/buffer dependency enumeration (~1–2d)

Using existing semantic model:

- Collect symbols with `SymbolKind::Buffer` / table links / `table_id`.
- Collect field-access / record-phrase references from `references` side table.
- Schema CRC is **not** available without compiler — document that oxabl can
  list *names* and leave CRC comparison to the build tool or XREF.

Static vs dynamic:

- Static: `FOR EACH Customer`, `DEFINE BUFFER b FOR Customer`, schema-resolved
  bare `Customer.Name`.
- Dynamic: handle-based buffer ops → `unresolvable` / `dynamic` entries.

---

## Phase 4 — XREF-diff harness (~3–5d, optional CI)

Dev tool (feature-gated or `[[bin]]` under `tools/`):

1. Input: corpus root + PROPATH + optional `.df`.
2. Run OpenEdge compiler with XREF; parse include + table refs from XREF.
3. Run oxabl dependency extraction on the same inputs.
4. Diff sets → report under-reported (bugs / missing conservative flags) and
   over-reported (noise).

**Requirements:**

- OpenEdge install optional — harness no-ops or skips with clear message.
- Golden fixtures checked in for *parsed XREF samples* so CI without OE can
  still test the XREF parser and diff logic.
- Never require OE for normal `cargo test`.

**Value:** continuous fidelity metric + free test-case generator.

---

## Implementation order

1. Phase 1 docs/tests (cheap, prevents silent wrong PROPATH assumptions).
2. Phase 2 + #55 Phase C together (API for unresolvable/dynamic + skipped
   includes).
3. Phase 3 table/buffer surface (builds on schema resolve).
4. Phase 4 harness when OE access is available to the author.

## Testing

- Unit: each dynamic construct produces an `unresolvable` entry, not silence.
- Integration: multi-include file with one missing include → dependency set
  lists it; symbols from it are not silently "undefined" without PREPROC007.
- Harness: at least one checked-in XREF fixture with known include set.

## Risk

- **Phase 2 API design** can ossify — keep `DependencySet` additive and version
  the analyze JSON section if exposed there.
- **XREF format** varies slightly across OE versions — pin documented version.
- Do not block lint work on Phase 4; harness is fidelity insurance, not a
  product dependency.

## Effort

| Phase | Effort |
|-------|--------|
| 1 | 1–2d |
| 2 | 2–3d |
| 3 | 1–2d |
| 4 | 3–5d |
| **Total** | **~7–12d** |
