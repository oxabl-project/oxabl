---
title: "feat: Semantic v1.1 — Schema Fields, Inheritance, Cross-File, Flow, and Validation"
type: feat
status: draft
date: 2026-04-25
origin: docs/plans/2026-04-16-004-feat-semantic-layer-v1-plan.md
---

# feat: Semantic v1.1 — Schema Fields, Inheritance, Cross-File, Flow, and Validation

## Enhancement Summary

**Deepened on:** 2026-04-25
**Agents consulted:** architecture-strategist, code-simplicity-reviewer, performance-oracle, pattern-recognition-specialist.

### Key improvements folded in

1. **Schema-backed field lookup** wires `Schema::get_field` into the resolve walker so `LINT0003` actually fires and `bCust.CustNum` types as the field's primitive instead of `Unknown`.
2. **Class INHERITS/IMPLEMENTS chain** is modeled as a `ClassHierarchy` side structure; `assignable(Class(a), Class(b))` walks the chain instead of matching by identity.
3. **`NodeId` on `Identifier` and `RunTarget::Literal`** gives every statement-level reference site a stable side-table key, enabling the cross-file sketch's `RUN "other.p"` resolution.
4. **Cross-file resolution** implements the `Workspace::resolve_cross_file -> CrossFileResolutions` side table from the v1 design sketch, limited to single-workspace files.
5. **Flow analysis** implements `CfgBuilder::build` and a definite-assignment dataflow pass, producing `LINT0005` (read-before-assignment).
6. **Corpus audit** ships a `corpus_lint_audit` binary that runs the four rules across a sampled pcna-erp subset and emits TSV, validating "meaningfully few false positives" empirically.
7. **Per-rule lint benches** add `crates/oxabl_lint/benches/lint_bench.rs` mirroring the per-pass semantic benches.
8. **Exact-JSON goldens** replace shape-based property tests with byte-level golden files once the parser's NodeId minter is deterministic under feature growth.

### Simplifications accepted

- Cross-file is **single-workspace only**; no external procedure library resolution (`PL` files) or AppServer proxy stubs.
- Flow analysis is **definite-assignment only** in v1.1; unreachable-code and NO-UNDO enforcement are deferred to v1.2.
- Exact-JSON goldens require a **deterministic NodeId allocator** — if the parser's feature-growth churn makes this intractable, goldens stay shape-based and this item is postponed.
- Corpus audit TSV is **human-readable, not machine-consumed** — no JSON schema, no CI gate on precision numbers. The TSV is a development artifact committed for transparency.

## Overview

v1 shipped a four-rule proof-point linter over a single-file semantic model with
side tables. v1.1 hardens that foundation by:

- Making the schema actually useful for field-level typing and diagnostics.
- Filling in the OO-ABL type-system gaps (inheritance, interfaces).
- Extending the model to cross-file references within a workspace.
- Adding the first dataflow pass (definite assignment).
- Validating rule quality empirically against the pcna-erp corpus.

Every item in this plan is additive — no v1 public type is removed or renamed.
The only AST change is adding an `id: NodeId` field to `Identifier` and
`RunTarget::Literal`, which is backward-compatible for consumers that don't read
the field.

---

## Problem Statement

v1's semantic layer is structurally sound but has four categories of
"known-unimplemented" that block real-world usefulness:

1. **Schema is decorative.** The `.df` parser loads; the schema model exists;
  but `resolve_field_access` emits `External` instead of querying the schema
  when a buffer qualifier is resolved. `LINT0003` is a no-op.
2. **OO-ABL types are flat.** `CLASS Sub INHERITS Super` parses, but the
  semantic layer stores the inherits clause as raw `Identifier` text. Assignment
  compatibility between `Sub` and `Super` fails because `assignable` only checks
  `SymbolId` identity.
3. **Statement-level identifiers are invisible.** `DELETE bCust`, `RUN "other.p"`,
  `DISPLAY STREAM s` — these reference symbols but have no `NodeId`, so they
  don't appear in the `references` side table. Cross-file resolution and
  reference-counting are incomplete.
4. **No validation at scale.** The four lint rules have unit tests but have
  never been run against the pcna-erp corpus. False-positive rate is unknown.

v1.1 closes these gaps while preserving the v1 architectural contract:
side-table-only, no AST mutation, no HIR lowering, Salsa-ready.

---

## Proposed Solution

Eight work items, sequenced by dependency. Each is a self-contained PR with
green CI.

```
Phase 1 ──► Phase 2 ──► Phase 3 ──► Phase 4
 (schema)    (inherit)   (NodeId)    (cross-file)
    │                                     │
    └─────────────────────────────────────┘
                    │
                    ▼
Phase 5 ──► Phase 6 ──► Phase 7
 (flow)     (audit)     (benches + goldens)
```

**Phase dependencies:**
- Phase 3 (NodeId on Identifier) blocks Phase 4 (cross-file) because
  `CrossFileResolutions` needs a `NodeId` key for every reference site.
- Phase 1 and Phase 2 are independent and can be developed in parallel.
- Phase 5 (flow) is independent of 1-4.
- Phase 6 (audit) benefits from 1-4 being done but can start in parallel
  using the v1 rule set as a baseline.
- Phase 7 (benches + goldens) is independent tooling.

---

## Technical Approach

### Phase 1 — Schema-backed field lookup

**Goal:** `bCust.CustNum` resolves to the schema field's type when schema is
loaded; `LINT0003` fires on unknown fields; `LINT0004` uses the field's real
primitive type instead of `Unknown`.

#### Current state

`resolve_field_access` (crates/oxabl_semantic/src/resolve.rs:1722-1806) resolves
the qualifier (`bCust`) to a buffer symbol, then emits:

```rust
// v1 placeholder: no schema-backed lookup
UnresolvedReason::External
```

The `Symbol` struct has no linkage from a `Buffer`/`TempTable` symbol to its
underlying `TableId`.

#### Changes

1. **`Symbol` gains `target_table: Option<TableId>`**

   Populated during declare pass:
   - `DEFINE BUFFER b FOR customer` → `target_table = Some(customer_table_id)`
   - `DEFINE TEMP-TABLE tt` → `target_table = None` (no schema table)
   - Implicit buffer from `FOR EACH customer` → `target_table = Some(customer_table_id)`

2. **`resolve_field_access` queries the schema**

   After resolving the qualifier to a buffer symbol:
   ```rust
   if let Some(table_id) = sym.target_table {
       if let Some(field) = ctx.schema.get_field(table_id, field_name) {
           // Record field resolution
           let field_sym = ...; // SymbolId for the field
           references.insert(field_access_node_id, Resolution::Resolved(field_sym));
           types.insert(field_access_node_id, field.data_type.into());
       } else {
           references.insert(field_access_node_id,
               Resolution::Unresolved { name: field_name, reason: UnresolvedReason::NotInScope });
       }
   }
   ```

3. **`LINT0003` updated**

   The rule currently fires on `NoSchema` (schema absent). It now also fires on
   `NotInScope` field references when `schema_loaded == true`.

4. **`LINT0004` precision improvement**

   Field-access expressions now carry the field's real `ResolvedType::Primitive`
   instead of `Unknown`, so assignments like `i = bCust.CustNum` (where `i` is
   `INTEGER` and `CustNum` is `INTEGER`) don't trigger a false-positive
   type-mismatch.

#### Tests

- Schema loaded + known field → `Resolved` + correct primitive type.
- Schema loaded + unknown field → `LINT0003` fires.
- Schema absent + any field access → `LINT0003` silent (R7).
- Buffer-qualified field (`bCust.CustNum`) vs bare table-qualified
  (`Customer.CustNum` where `Customer` is an implicit buffer).
- Field type propagates into `LINT0004` (no false positive on `INTEGER = INTEGER`).

Estimated effort: 2 days.

---

### Phase 2 — Class INHERITS/IMPLEMENTS chain

**Goal:** `assignable(from, to)` returns `true` when `from` is a subclass of `to`
or implements `to`'s interface.

#### Current state

`ResolvedType::Class(SymbolId)` stores the class symbol. `assignable` compares
`SymbolId` for equality only. `StatementKind::Class` stores `inherits` and
`implements` as raw `Identifier`/`Vec<Identifier>`, never resolved.

#### Changes

1. **New `ClassHierarchy` side structure**

   ```rust
   pub struct ClassHierarchy {
       /// parent class, if any
       pub inherits: IndexVec<SymbolId, Option<SymbolId>>,
       /// implemented interfaces
       pub implements: IndexVec<SymbolId, Vec<SymbolId>>,
   }
   ```

   Stored on `Semantic` as `pub class_hierarchy: ClassHierarchy`.

2. **Declare/resolve pass populates it**

   After the declare pass builds all class symbols, a second mini-pass resolves
   `inherits` and `implements` identifiers against the `Types` namespace:
   - `INHERITS Super` where `Super` is local → `inherits[class_sym] = Some(super_sym)`
   - `INHERITS ExternalClass` → `inherits[class_sym] = None` (cross-file, deferred)
   - `IMPLEMENTS Interface1, Interface2` → similar

3. **`assignable` walks the chain**

   ```rust
   fn assignable(from: &ResolvedType, to: &ResolvedType) -> bool {
       match (from, to) {
           (Class(a), Class(b)) => {
               a == b || class_hierarchy.is_subclass_of(*a, *b)
           }
           // ... existing primitive rules
       }
   }
   ```

   `is_subclass_of` walks `inherits` links transitively (single-file only;
   cross-file parent classes remain `None` and assignment falls back to identity).

4. **`is_narrowing_warning` for downcasts**

   `Super = Sub` is valid (widening). `Sub = Super` is valid but emits a
   `LINT0004 Warning` (narrowing — runtime `CAST` may fail).

#### Tests

- Single-file inheritance: `Sub INHERITS Super` → `Sub` assignable to `Super`.
- Interface implementation: `Class IMPLEMENTS IFace` → `Class` assignable to `IFace`.
- Transitive: `A INHERITS B INHERITS C` → `A` assignable to `C`.
- Cross-file parent: `Sub INHERITS External` → `Sub = Sub` OK, `Sub = External`
  silent (External type unknown).
- Downcast warning: `Sub = Super` emits `LINT0004 Warning`.

Estimated effort: 2 days.

---

### Phase 3 — NodeId on Identifier and RunTarget::Literal

**Goal:** Every identifier reference site has a `NodeId` in the `references`
side table, including statement-level identifiers and `RUN "other.p"` targets.

#### Current state

```rust
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

pub enum RunTarget {
    Literal(String),
    Dynamic(Expression),
}
```

Neither carries a `NodeId`. In `resolve.rs`, the RUN handler notes:
```rust
RunTarget::Literal(_) => {
    // External procedure name — no NodeId to bind
}
```

#### Changes

1. **`Identifier` gains `id: NodeId`**

   ```rust
   pub struct Identifier {
       pub id: NodeId,
       pub span: Span,
       pub name: String,
   }
   ```

   The parser's `NodeIdAllocator` assigns an ID at every identifier construction
   site. This is a non-breaking addition — consumers that don't read `id` are
   unaffected.

2. **`RunTarget::Literal` gains `NodeId`**

   ```rust
   pub enum RunTarget {
       Literal { path: String, id: NodeId },
       Dynamic(Expression),
   }
   ```

   The parser assigns a `NodeId` when parsing the string literal in `RUN "x"`.

3. **Resolve pass records references for statement-level identifiers**

   Every site that currently bumps `read_count`/`write_count` without recording
   a `references` entry now records one:
   - `DELETE bCust` → identifier NodeId → buffer symbol
   - `DISPLAY STREAM s` → stream identifier NodeId → stream symbol
   - `RUN "other.p"` → RunTarget Literal NodeId → `Unresolved { reason: External }`

4. **Tests updated**

   Existing tests that construct `Identifier` by hand need `id: NodeId::DUMMY`
   added. No semantic behavior changes for v1 rules.

#### Tests

- `DELETE buffer_name` → references entry exists at identifier NodeId.
- `RUN "proc.p"` → references entry exists at RunTarget NodeId, reason External.
- `DISPLAY STREAM s` → references entry exists at identifier NodeId.
- Read/write counts still correct (no double-counting from new entries).

Estimated effort: 1.5 days.

---

### Phase 4 — Cross-file resolution

**Goal:** Implement the `CrossFileResolutions` side table from the v1 design
sketch, limited to files within the same workspace.

#### Current state

The design sketch at `docs/design/semantic-v1-cross-file-sketch.md` specifies:

```rust
pub struct CrossFileResolutions {
    pub resolved_external: FxHashMap<(FileId, NodeId), CrossFileSymbol>,
}
```

No implementation exists.

#### Changes

1. **New `oxabl_workspace_semantic` crate (or module in `oxabl_workspace`)**

   Keeps cross-file logic out of `oxabl_semantic` (which remains single-file).
   `oxabl_workspace` already depends on `oxabl_semantic`; adding cross-file
   resolution there is a natural extension.

2. **`Workspace::resolve_cross_file`**

   ```rust
   impl Workspace {
       pub fn resolve_cross_file(
           &self,
           files: &FxHashMap<FileId, Semantic>,
       ) -> CrossFileResolutions {
           // Walk each file's External references:
           // 1. USING imports → match short name against workspace's class index
           // 2. RUN "name.p" → match against workspace file names
           // 3. NEW ClassName → match against workspace class index
       }
   }
   ```

   The workspace maintains a name index built from all loaded `Semantic`s:
   `FxHashMap<OxablAtom, Vec<(FileId, SymbolId)>>` for class/procedure names.

3. **`AnalysisContext` gains `cross_file: Option<&CrossFileResolutions>`**

   Lint rules consult it via `effective_resolution()` (from the design sketch):
   ```rust
   pub fn effective_resolution<'a>(
       sem: &'a Semantic,
       xfr: Option<&'a CrossFileResolutions>,
       file: FileId,
       node: NodeId,
   ) -> EffectiveResolution { ... }
   ```

4. **`LINT0001` updated**

   Previously skipped all `External` references. Now:
   - Consult `CrossFileResolutions`
   - If resolved → no diagnostic
   - If still unresolved → emit `LINT0001` with message "undefined symbol (cross-file unresolved)"

5. **`LINT0004` updated**

   Cross-file resolved symbols contribute real types, reducing `Unknown`-based
   skips.

#### Tests

- `USING pkg.Class. NEW Class()` → resolved to declaring file's class symbol.
- `RUN "other.p"` where `other.p` is in workspace → resolved.
- `RUN "missing.p"` → `LINT0001` fires.
- Cross-file class assignment: `Super = Sub` where `Sub` is in another file →
  `assignable` uses `ClassHierarchy` from both files (merged).

#### Limitations (documented)

- Single-workspace only. External `PL` libraries and AppServer proxies remain
  `External`.
- `RUN VALUE(x)` (dynamic) remains `External` — we can't resolve runtime values.
- Schema revision mismatch across files is checked but not auto-reconciled.

Estimated effort: 3 days.

---

### Phase 5 — Flow analysis (definite assignment)

**Goal:** Build a CFG over the AST and run a forward dataflow pass to detect
read-before-assignment. New rule: `LINT0005`.

#### Current state

The design sketch at `docs/design/semantic-v1-flow-analysis-sketch.md` specifies:

```rust
pub struct Cfg {
    pub blocks: Vec<BasicBlock>,
    pub entry: FxHashMap<NodeId, BlockId>,
    pub block_of_stmt: NodeIndexVec<BlockId>,
}
```

No implementation exists.

#### Changes

1. **New `oxabl_semantic/src/cfg.rs`**

   `CfgBuilder::build(program: &[Statement], sem: &Semantic) -> Cfg`

   Straightforward recursive walk matching on `StatementKind`:
   - `Block(body)` → sequence
   - `If { then, else }` → split + join
   - `Do` / `Repeat` / `ForEach` → loop with back-edge
   - `Leave` / `Next` / `Return` / `Throw` → jump
   - `Catch` / `Finally` → extra predecessors from throw sites

   Every `Statement` and `Expression` already carries a `NodeId`; the CFG
   indexes into the AST by `NodeId` without modifying it.

2. **New `oxabl_semantic/src/dataflow.rs`**

   `definite_assignment(cfg: &Cfg, sem: &Semantic) -> DefiniteAssignment`

   Forward dataflow, meet = intersection, gen = writes in block:
   ```
   in[b]  = ∩ out[p] for p ∈ predecessors(b)
   out[b] = in[b] ∪ writes_in(b)
   ```

   Converges via worklist algorithm.

3. **New `LINT0005` in `oxabl_lint`**

   `read_before_assignment.rs`

   ```rust
   for each resolved read ref r at NodeId n in block b:
       let sym = sem.references[n] === Resolved(s) => s;
       if sym ∉ must_assigned_at_entry[b]:
           emit LINT0005 at n
   ```

   Severity: `Warning` (ABL permits read-before-assignment at runtime, returning
   `?` or the initial value).

4. **Skip list**

   - Variables with `INITIAL` value (considered assigned at declaration).
   - `INPUT` parameters (assigned by caller).
   - `SHARED` / `NEW SHARED` variables (assigned in another file).
   - Builtins (`SESSION`, `ERROR-STATUS`, etc.).
   - `CATCH` variables (assigned by the exception mechanism).

#### Tests

- Simple read-before-write: `DEF VAR i AS INT. MESSAGE i.` → `LINT0005`.
- Assignment then read: `DEF VAR i AS INT. i = 1. MESSAGE i.` → no diagnostic.
- Branching: `IF x THEN i = 1. MESSAGE i.` → `LINT0005` (not assigned on all paths).
- Loop: `DO WHILE FALSE: i = 1. END. MESSAGE i.` → `LINT0005` (loop may not execute).
- `INITIAL` value: `DEF VAR i AS INT INIT 1. MESSAGE i.` → no diagnostic.
- `INPUT` parameter: `DEF INPUT PARAM p AS INT. MESSAGE p.` → no diagnostic.

Estimated effort: 3 days.

---

### Phase 6 — Corpus lint audit

**Goal:** Run the lint rules against a sampled pcna-erp subset, emit TSV, and
validate precision ≥ 0.9 per rule.

#### Current state

No corpus-level lint tooling exists. `oxabl check` only parses; it does not run
semantic analysis or lint.

#### Changes

1. **New binary: `crates/oxabl_analyze/src/bin/corpus_lint_audit.rs`**

   Or a new subcommand on `oxabl`: `oxabl audit <dir>`.

   ```rust
   pub fn audit_corpus(
       workspace: &Workspace,
       rules: &[LintRule],
       output: &mut dyn Write,
   ) -> Result<AuditReport>;
   ```

2. **TSV format**

   ```
   file	rule	severity	line	message	category
   src/sales/order.p	LINT0001	Error	42	undefined symbol 'x'	true_positive
   src/sales/order.p	LINT0002	Warning	55	unused variable 'y'	false_positive
   ```

   The `category` column is manually adjudicated during review.

3. **Sampling strategy**

   - 100 files randomly sampled from pcna-erp (or stratified by directory to
     ensure coverage of sales, inventory, finance, etc.).
   - All four rules run on each file.
   - Parse failures are recorded separately but do not count against precision.

4. **Precision target**

   Per-rule precision = true_positives / (true_positives + false_positives).
   Target: ≥ 0.9 for each of LINT0001–LINT0005.

   If a rule falls below 0.9, the plan author and pcna-erp maintainer jointly
   decide whether to:
   - Add skip-list entries (rule bug)
   - Document the limitation (semantic gap)
   - Defer the rule to v1.2

#### Tests

- Binary runs without panic on the sampled subset.
- TSV output is parseable and contains all required columns.
- Manual audit of 20-file pilot produces a precision estimate before the full 100-file run.

Estimated effort: 2 days (including manual audit time).

---

### Phase 7 — Per-rule lint benches + exact-JSON goldens

#### 7a — Per-rule lint benches

**Goal:** `cargo bench -p oxabl_lint --bench lint_bench` measures each rule
individually.

**Changes:**
- New `crates/oxabl_lint/benches/lint_bench.rs`
- One Criterion group per rule (`undefined_symbol`, `unused_variable`, etc.)
- Fixtures: tiny (1 rule, 10 lines), medium (mixed rules, 200 lines), large
  (dense violations, 1000 lines)
- Baseline measurement: end-to-end `lint_file` vs per-rule call

**Tests:** Bench compiles and runs. No functional changes.

Estimated effort: 0.5 days.

#### 7b — Exact-JSON goldens

**Goal:** Replace shape-based property tests with byte-level exact-JSON golden
files for stable output validation.

**Prerequisite:** Parser NodeId allocator must be deterministic under feature
growth. Currently, adding a new statement kind can shift NodeIds because the
allocator is invoked in parser match arms that may be reordered.

**Approach:**
1. **Spike:** Verify that NodeIds are deterministic across parser changes by:
   - Recording NodeId assignments for a canonical fixture
   - Adding a new statement kind (noop parser branch)
   - Checking if existing NodeIds shift

2. **If deterministic:**
   - Add `tests/goldens/` directory with `.json` files
   - Test runner: `assert_eq!(actual_json, expected_json)` (byte-level)
   - Update script: `BLESS=1 cargo test -p oxabl_analyze` overwrites goldens
   - Start with 5 fixtures, grow to 20+ over time

3. **If NOT deterministic:**
   - Document the limitation in `docs/design/semantic-v1-limitations.md`
   - Keep shape-based tests
   - Consider a stable "semantic fingerprint" (hash of sorted symbols/references)
     as an alternative

**Tests:** Golden files are the tests. CI enforces byte-level match.

Estimated effort: 1 day (including spike).

---

## Implementation Phases

| Phase | Work item | Effort | Dependencies |
|-------|-----------|--------|--------------|
| 1 | Schema-backed field lookup | 2d | None |
| 2 | Class INHERITS/IMPLEMENTS | 2d | None |
| 3 | NodeId on Identifier / RunTarget | 1.5d | None |
| 4 | Cross-file resolution | 3d | Phase 3 |
| 5 | Flow analysis | 3d | None |
| 6 | Corpus audit | 2d | Phase 1–4 (benefits from) |
| 7a | Per-rule lint benches | 0.5d | None |
| 7b | Exact-JSON goldens | 1d | Spike result |
| **Total** | | **~15d** | |

Phases 1, 2, 3, 5, and 7a are parallelizable. Phases 4 and 6 are sequential.

---

## Acceptance Criteria

### Functional

- [ ] `bCust.CustNum` resolves to schema field type when schema loaded; `LINT0003`
  fires on unknown field; `LINT0004` no longer false-positives on matching primitives.
- [ ] `CLASS Sub INHERITS Super` → `Sub` assignable to `Super`; `Super = Sub`
  emits `LINT0004 Warning`.
- [ ] Every `Identifier` and `RunTarget::Literal` carries a `NodeId`; resolve pass
  records a `references` entry for statement-level identifier sites.
- [ ] `Workspace::resolve_cross_file` resolves `USING` + `NEW` and `RUN "name.p"`
  within the workspace; unresolved cross-file references emit `LINT0001`.
- [ ] `CfgBuilder::build` produces a valid CFG for all parser-supported statement
  kinds; definite-assignment pass emits `LINT0005` on read-before-write.
- [ ] `corpus_lint_audit` runs on 100-file pcna-erp sample and emits TSV.
- [ ] `cargo bench -p oxabl_lint` runs per-rule benches.
- [ ] Exact-JSON goldens pass byte-level diff (or documented limitation if
  NodeIds are not deterministic).

### Non-Functional

- [ ] `analyze_file` runtime regression < 10% from Phase 1–5 changes.
- [ ] `oxabl_semantic` crate remains `serde_json`-free.
- [ ] `cargo clippy -D warnings` clean on all changed crates.

### Quality Gates

- [ ] ≥ 10 new unit tests in `oxabl_semantic` for schema field lookup.
- [ ] ≥ 8 new unit tests for class hierarchy.
- [ ] ≥ 6 new unit tests for NodeId on Identifier.
- [ ] ≥ 8 new unit tests for cross-file resolution.
- [ ] ≥ 10 new unit tests for definite assignment.
- [ ] Per-rule precision ≥ 0.9 on 100-file manual audit sample.

---

## Success Metrics

- `LINT0003` true-positive rate on schema-loaded corpus ≥ 0.9 (previously 0,
  because rule was a no-op).
- `LINT0004` false-positive rate decreases by ≥ 30% on schema-loaded corpus
  (field types no longer `Unknown`).
- Cross-file resolution resolves ≥ 50% of `External` references in a
  multi-file workspace sample.
- `LINT0005` finds ≥ 1 real read-before-write bug in pcna-erp sample.

---

## Dependencies & Prerequisites

- v1 semantic layer is complete and green (confirmed: 933 tests).
- v1 design sketches for cross-file and flow analysis are reviewed and stable
  (confirmed: `docs/design/semantic-v1-cross-file-sketch.md` and
  `semantic-v1-flow-analysis-sketch.md`).
- pcna-erp corpus is accessible for sampling (confirmed: used in parser tests).
- `.df` schema loads successfully for pcna-erp (confirmed: `oxabl_schema`
  parses the corpus `.df` files).

## Risk Analysis & Mitigation

| Risk | Mitigation |
|------|------------|
| Schema field lookup requires Symbol changes that break existing tests | Additive-only: `target_table: Option<TableId>` with default `None`; no existing test constructs buffers with schema linkage. |
| Class hierarchy introduces cycle risk (`A INHERITS B INHERITS A`) | Detect cycles during hierarchy build; mark involved classes as `ResolvedType::Error`; emit `SEM0020` diagnostic. |
| NodeId on Identifier is a wide refactor across parser + AST | Mechanical: add field, update constructor call sites, update hand-constructed AST in tests. No semantic changes. |
| Cross-file resolution is slow on large workspaces | Workspace name index is built once; lookup is `FxHashMap` O(1). Benchmark and optimize if > 1000 files. |
| Flow analysis CFG builder misses ABL-specific control flow | Extensive fixture coverage for `ON ERROR`, `UNDO`, `RETRY`, `NEXT`, `LEAVE`. Document known gaps. |
| Corpus audit reveals systematic false positives | Joint review with pcna-erp maintainer; add skip-list entries or document limitations. |
| NodeId non-determinism blocks exact-JSON goldens | Spike first; if blocked, document limitation and keep shape-based tests. |

## Resource Requirements

Single engineer, sequential phases with parallelization noted. Total: ~15 days.

Gated by:
- Phase 1 + 2 code review (jointly, since independent)
- Phase 4 code review (depends on Phase 3)
- Phase 6 manual audit (requires pcna-erp maintainer time)

## Documentation Plan

- `docs/design/semantic-v1-limitations.md` — updated with v1.1 scope and known gaps.
- `crates/oxabl_lint/docs/rules/LINT0005.md` — new rule documentation.
- `README.md` — update Current Status to reflect v1.1 capabilities.
- `CLAUDE.md` — add notes on `ClassHierarchy`, `CrossFileResolutions`, and CFG.

## Sources & References

### Origin

- Parent plan: `docs/plans/2026-04-16-004-feat-semantic-layer-v1-plan.md`
- Cross-file sketch: `docs/design/semantic-v1-cross-file-sketch.md`
- Flow analysis sketch: `docs/design/semantic-v1-flow-analysis-sketch.md`

### Internal References

- Schema parser: `crates/oxabl_schema/src/`
- Semantic layer: `crates/oxabl_semantic/src/`
- Lint rules: `crates/oxabl_lint/src/rules/`
- Analyze dump: `crates/oxabl_analyze/src/`
- AST nodes: `crates/oxabl_ast/src/statement.rs`, `expression.rs`

### External References

- Oxc CFG builder: `crates/oxc_semantic/src/cfg/`
- Ruff definite-assignment: `crates/ruff_python_semantic/src/analyze/`
- rust-analyzer cross-crate resolution: `crates/hir_def/src/nameres/`
