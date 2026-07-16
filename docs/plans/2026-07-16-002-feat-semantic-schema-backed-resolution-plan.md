---
title: "feat: Schema-Backed Symbol Resolution (single-file field validation)"
type: feat
status: draft
date: 2026-07-16
origin: GitHub #58
related: [#56, docs/plans/2026-04-25-001-feat-semantic-v1-1-followups-plan.md]
branch: feat/semantic-schema-resolution
---

# feat: Schema-Backed Symbol Resolution

## Problem Statement

The `.df` schema is **decorative** today. `oxabl_schema` parses the file, the
`Schema` model is built, `AnalysisContext` carries a `schema: &Schema` and a
`schema_loaded` flag — and then nothing consumes it during resolution. The
resolve walker deliberately throws the schema away at the one point it matters.

Concretely, three false-positive / missed-diagnostic classes make up the bulk
of the #58 noise:

1. **Unqualified table names in a field-access qualifier are a live double
   false-positive.** A bare `Customer.Name` reference where `Customer` was never
   `DEFINE BUFFER`'d flows through `resolve_field_access`
   (crates/oxabl_semantic/src/resolve.rs:1749-1760): the qualifier is resolved
   directly via `self.tree.resolve(Buffers/Tables)` — `resolve_expr_ident` is
   **not** consulted — so a miss takes the `None` branch (resolve.rs:1796-1820)
   and emits `NotInScope` on **both** the qualifier and the field under a loaded
   schema. That is LINT0001 firing on `Customer` **and** LINT0003 on `Name` —
   the exact double FP this feature must eliminate. (Note: bare `FIND Customer` /
   `FOR EACH … OF Customer` is a *different* path — those buffer names go through
   `resolve_statement_ident`, resolve.rs:1164, which silently no-ops on a miss;
   they produce no FP today and are **not** covered by the schema fallback added
   here. See Goals/Non-Goals.)

2. **Valid qualified fields are never validated or typed.**
   `DEFINE BUFFER bCust FOR Customer. ... bCust.CustNum` resolves the qualifier
   `bCust` to a buffer symbol correctly, but the field is hard-coded to
   `Unresolved { reason: External }` (crates/oxabl_semantic/src/resolve.rs:1776-1786).
   The comment there is explicit: *"schema-backed field lookup requires knowing
   the target table for the buffer symbol. This indirection isn't cached on
   `Symbol` in v1."* Consequence: `bCust.CustNum` has type `Unknown`, so
   LINT0004 (type-mismatch-assignment) silently skips any assignment involving a
   field, and LINT0003 can never confirm the field is real.

3. **Invalid fields are never caught.** `bCust.NoSuchField` produces the exact
   same `External` resolution as a valid field. LINT0003
   (`unknown-table-or-field`) is a documented no-op: its own module doc says it
   *"is effectively a no-op until the schema-backed field lookup lands as a
   follow-up"* (crates/oxabl_lint/src/rules/unknown_table_or_field.rs). The rule
   ships purely to stabilize the diagnostic surface.

The wiring gap is closed by four connected changes: a table link on `Symbol`,
a rewritten field-access resolver, a schema fallback for bare identifiers, and
CLI `--schema` loading (the CLI currently hard-codes `Schema::empty()` at
crates/oxabl/src/main.rs:192).

---

## Goals / Non-Goals

### Goals

- Add a `table_id` link from `Buffer` / `TempTable` symbols to their backing
  schema table, populated at declare time for every buffer-introducing site.
- Rewrite `resolve_field_access` to perform real field lookup
  (buffer → `table_id` → `Schema::get_by_id` → `Table::get_field`) and record a
  concrete resolution for the field.
- Type a validated field with its schema primitive (`SchemaType` → `ResolvedType`)
  in the check pass, so LINT0004 stops skipping field assignments.
- Add a schema fallback in **two** places so bare table names resolve: the
  `None` branch of `resolve_field_access` (the field-access qualifier path — the
  headline `Customer.Name` case) **and** `resolve_expr_ident` (bare identifier
  used as a standalone reference). The field-access path is primary; without it
  the CRITICAL double FP above is not fixed.
- Make LINT0003 fire on unknown fields / unknown qualified tables under a loaded
  schema, without regressing its schema-absent silence. (LINT0003 *already*
  fires on `NoSchema|NotInScope` under `schema_loaded` — see Step 5; the
  behavior change is driven entirely by the new `resolve_field_access`
  resolutions, not by editing the rule's logic.)
- Wire `--schema <path>` into the CLI, scoped to the **`analyze` subcommand
  only**, mirroring `--include-path`. (The `check` subcommand is parse-only:
  `run_check`, main.rs:219, never calls `analyze_file`/`lint_file`, so a
  `--schema` flag there would be dead. Only `run_analyze` runs semantic
  analysis.)

### Non-Goals (each is a separate feature)

- **Class inheritance / interfaces** — INHERITS/IMPLEMENTS chain modeling
  (v1.1 plan Phase 2). Untouched here.
- **Cross-file resolution** — the `CrossFileResolutions` side table
  (v1.1 plan Phase 4, `docs/design/semantic-v1-cross-file-sketch.md`). This
  feature is strictly single-file.
- **Flow analysis** — CFG / definite-assignment / LINT0005 (v1.1 plan Phase 5).
- **XREF / corpus audit harness** — the empirical precision-measurement tooling
  (v1.1 plan Phase 6).
- **`NodeId` on `Identifier` / `RunTarget`** (v1.1 plan Phase 3). Not required:
  the field-access node already carries a `NodeId`.
- Field extent/array coercion refinement in LINT0004 — deferred.

---

## Design

### The central decision: how is a resolved field/table represented?

This is the decision that determines the blast radius of the feature. The
dossier lays out three options; each is a different answer to *"where does a
schema field live in the resolution model, and how does a type flow out of
it?"*

The current model (crates/oxabl_semantic/src/resolve.rs:48-64,
`docs/design/semantic-v1-cross-file-sketch.md`):

```rust
pub enum Resolution {
    Resolved(SymbolId),
    Unresolved { name: OxablAtom, reason: UnresolvedReason },
}
pub enum UnresolvedReason { NotInScope, External, NoSchema }
```

**Option A — synthesize field/table symbols, reuse `Resolved(SymbolId)`.**
On a successful lookup, mint a `Symbol` (kind `Field` for a field, `Buffer` for
a bare table) carrying the schema-derived `data_type`, insert it into the
`SymbolTable`, and record `Resolution::Resolved(new_id)` at the field-access
node. The field's type flows out through the *existing* path in check.rs
(`type_from_reference` → `Symbol::data_type`).

**Option B — add `Resolution::Field { table_id, .. }` / `Resolution::Table(table_id)`
variants.** Represent schema hits directly on the enum, bypassing the symbol
table. Type synthesis extracts `field.data_type` from the variant.

**Option C — keep `Unresolved`, add a `reason` like `FieldValidated`.** The
field stays `Unresolved` but the reason distinguishes "validated by schema" from
"never checked." A parallel `NodeIndexVec<Option<SchemaType>>` side table would
carry the type.

#### Decision: **Option A**, with two refinements (dedup cache + `NodeId::DUMMY` marking).

This is chosen for correctness and architectural fit, not for being the smallest
diff. The reasoning:

1. **It preserves the one invariant the whole model is built around.** The
   cross-file design sketch pins this explicitly
   (`docs/design/semantic-v1-cross-file-sketch.md`, "What cross-file does *not*
   touch"): *"`NodeIndexVec<Resolution>` side table: unchanged. No new variants
   on `Resolution`."* And: *`Resolved(SymbolId)` is the unique path to a type.*
   Option B breaks both — it is a public breaking change to `Resolution` that
   the next feature (cross-file) has already promised reviewers it will not make.
   Choosing B here spends a breaking-change budget that belongs to a later,
   larger feature, for a benefit (avoiding symbol synthesis) that is cosmetic.

2. **Type synthesis falls out for free.** check.rs `type_from_reference`
   (crates/oxabl_semantic/src/check.rs:547-560) already routes
   `Resolved(sym)` with `SymbolKind::Field` through `symbol.data_type`. A
   synthesized field symbol whose `data_type` is the schema primitive types
   correctly with **zero** new logic in `type_from_reference`. The *only*
   check.rs change needed is the `FieldAccess` arm, which today hard-returns
   `ResolvedType::Unknown` (crates/oxabl_semantic/src/check.rs:503-508) — it must
   instead call `self.type_from_reference(expr)`. Under Option B, both that arm
   *and* `type_from_reference` would need to grow new match arms.

3. **Lint rules are unchanged in shape.** LINT0001/0002/0004 all pattern-match
   `Resolution::Resolved` / `Unresolved`. Option A adds no variant, so none of
   them break or need a new arm. Only LINT0003 changes behavior (it must now
   fire on `NotInScope` field references), and that is a behavior change we
   *want*, independent of representation.

4. **Option C is disqualified** because it does not solve typing: LINT0004 still
   sees `Unknown` for fields unless we add a parallel side table, which violates
   the "`Resolved` is the unique path to a type" invariant and adds a second
   source of truth.

5. **Why not type the qualifier as the already-existing `ResolvedType::Table` /
   `Buffer`?** `ResolvedType::Table(SchemaRevision, TableId)` and
   `ResolvedType::Buffer(SymbolId)` **already exist** in the type enum
   (crates/oxabl_semantic/src/types.rs:21-23) and look like a ready-made home for
   a resolved qualifier. We deliberately do *not* route the field result through
   them. Those variants describe the *type of the qualifier expression*
   (`bCust`/`Customer` is table-typed); they carry no per-**field** type, so a
   `bCust.CustNum` node typed as `Table(..)` still gives LINT0004 nothing to
   check. The synthesized `Field` symbol is what carries the field's primitive
   `data_type`, and it lands on the field-access node's `Resolution` — exactly
   where `type_from_reference` already looks. We *may* additionally set the
   qualifier node's type to `Buffer`/`Table` (it is the qualifier's honest type),
   but that is orthogonal to fixing the field type and is not required by this
   feature. These variants existing does not change the Option A decision.

**Refinement 1 — on-demand synthesis with a dedup cache.** We do *not*
pre-synthesize all fields of a table (the 200-field-`Customer` bloat the dossier
worries about). We synthesize only fields actually referenced, and cache
`(TableId, field_atom) -> SymbolId` on the resolve walker so repeated
`bCust.CustNum` references across the file reuse one synthetic symbol. This caps
synthetic symbols at *distinct fields touched*, not *fields that exist*.

**Refinement 2 — mark synthetics.** Synthetic symbols get
`declaration: NodeId::DUMMY` and `name_span` = the field-access identifier's span
(so a future diagnostic can still point at the use site). This mirrors how
builtins are already marked (declaration = DUMMY, skipped by the type-mirroring
loop in `resolve_pass`). LINT0002 (unused-variable) already scopes itself to
user declaration sites; a test will pin that synthetic field symbols are never
enumerated by it.

#### `table_id` vs. target-table atom on `Symbol` (sub-decision)

The link itself is stored as `table_id: Option<oxabl_schema::TableId>` on
`Symbol` (per the dossier). The considered alternative — storing the folded
target-table `OxablAtom` and re-resolving to a `TableId` in the resolve pass —
is more robust against schema-revision drift but costs a `FxHashMap` name lookup
per buffer per resolve. We choose `TableId` because:

- Declare and resolve both run inside a single `analyze_file` call against **one**
  `ctx.schema` (crates/oxabl_semantic/src/lib.rs:83-88). Within that call the
  revision is fixed, so a `TableId` minted in declare is valid in resolve.
- `Schema::get_by_id(TableId)` is a dense-vector index; `Schema::table_id(atom)`
  is a hash lookup. Storing the id moves the hash lookup to declare-time (once
  per buffer) and makes every field access an O(1) vector index.

The atom alternative is recorded in Open Questions as the fallback if a future
feature ever reuses a `Symbol` across schema revisions.

### The `TableId` / `SchemaRevision` staleness invariant

`TableId` is dense and stable **only within one `SchemaRevision`**
(dossier §1). `Semantic` already carries `schema_revision: SchemaRevision`
(crates/oxabl_semantic/src/lib.rs:75). We formalize the invariant:

> A `Symbol::table_id` is meaningful only under the `Schema` whose
> `revision()` equals the `Semantic::schema_revision` captured when the symbol
> was declared. Consumers must never resolve a `table_id` against a `Schema`
> from a different revision.

Why the guard must be real: `Schema::get_by_id` is a bare `Vec` index
(crates/oxabl_schema/src/schema.rs:241) with no revision check. A stale
`TableId` does not panic — it **silently indexes the wrong table** (or, if out
of range, returns `None`). So a vacuous assert buys nothing; the tripwire has to
compare a *captured* revision against the *current* one.

A naive `debug_assert!(ctx.schema.revision() == ctx.schema.revision())` is
vacuous (it compares a value to itself). And `Symbol` carries no declare-time
revision to compare against — adding a per-symbol `SchemaRevision` was rejected
as overhead — while `SchemaRevision::new`/`bump` are `pub(crate)`, so tests
cannot mint revisions by hand and must build real schemas via the loader.

Enforcement, cheapest-sufficient first:

- Single-file analysis loads the schema once and threads the *same* `&Schema`
  through declare, resolve, and check. The invariant holds by construction.
- **Capture the declare-time revision in the declare pass output.** `declare_pass`
  already returns a tuple `(ScopeTree, SymbolTable, Vec<Diagnostic>)`; extend it
  to also carry `declare_revision: SchemaRevision` (= `ctx.schema.revision()` at
  declare time). `resolve_pass` receives it and, on the schema-lookup path,
  `debug_assert_eq!(declare_revision, self.ctx.schema.revision())`. Because the
  compared values come from two *different* points in time (declare vs. resolve),
  the assert is meaningful — it fires the moment a resolve runs against a schema
  whose revision differs from the one declare saw. Release builds pay nothing.
- A unit test (see Testing) builds **two** distinct schemas via
  `SchemaLoader::load_files` (hence two revisions), runs declare against one and
  resolve against the other, and asserts the guard panics (`#[should_panic]`,
  debug-only).

### Buffer → table linkage at every declaration site

There are three declare-pass sites that introduce a buffer/table symbol, plus
one resolve-time site. All must populate `table_id`:

| Site | Location | Table name source |
|------|----------|-------------------|
| `DEFINE BUFFER b FOR Customer` | resolve.rs:197-198 (`declare_simple`) | `BufferTarget::Table(Identifier)` (statement.rs:834-839) — **currently discarded** |
| `DEFINE BUFFER b FOR TEMP-TABLE tt` | resolve.rs:197-198 | `BufferTarget::TempTable` → `table_id = None` (temp-tables have no schema table) |
| `FOR EACH Customer:` implicit buffer | resolve.rs:388-401 (`declare`) | the `buffer: Identifier` is the table name |
| `DEFINE TEMP-TABLE tt` | temp-table declare (resolve.rs ~580) | `table_id = None`; its fields already get `SymbolKind::Field` symbols |

`FIND Customer WHERE ...` does **not** declare an implicit buffer in the declare
pass today (the `StatementKind::Find` handling lives only in the resolve pass at
resolve.rs:1158). Its buffer name is resolved through **`resolve_statement_ident`**
(resolve.rs:1164) over `[Buffers, Tables]`, which **silently no-ops on a miss** —
no diagnostic, no synthesized symbol. This produces no false positive today, and
this feature does **not** change it: the schema fallback added here lives in
`resolve_field_access` and `resolve_expr_ident`, neither of which is on the
`resolve_statement_ident` path. So bare `FIND Customer` / `FOR EACH … OF Customer`
remains a silent non-resolution (out of scope). The mechanism for "unqualified
table names resolve" applies specifically to the **field-access qualifier**
(`Customer.Name`, via the `resolve_field_access` `None`-arm fallback) and to
**standalone bare identifiers** (via `resolve_expr_ident`).

### Mechanics

`declare` (resolve.rs:628-681) grows one parameter, `table_id: Option<TableId>`,
threaded from each call site. `declare_simple` (resolve.rs:614-623) either grows
the same parameter or the `DefineBuffer` arm stops using it and calls `declare`
directly (it must, because only `DefineBuffer` needs the target). The `Symbol`
literal at resolve.rs:666-677 sets the new field.

`resolve_field_access` (resolve.rs:1738-1822) rewrite. **Both** match arms
change — this is the crux of the CRITICAL fix. The qualifier is resolved by
`self.tree.resolve(Buffers/Tables)` directly (resolve.rs:1757-1760); it does
**not** route through `resolve_expr_ident`, so the schema fallback added to
`resolve_expr_ident` (below) does *not* reach a field-access qualifier. The
fallback must therefore be duplicated into the `None` arm here, or the headline
`Customer.Name` case stays broken.

**`Some(qsym)` arm** — replacing the `External` placeholder at 1776-1793:

```rust
Some(qsym) => {
    self.references.insert(qualifier.id, Resolution::Resolved(qsym));
    self.bump_count(qsym, AccessMode::Read);
    let field_atom = fold_atom(&field.name);

    let resolution = match self.symbols.get(qsym).table_id {
        Some(tid) if self.ctx.schema_loaded => {
            match self.ctx.schema.get_by_id(tid).and_then(|t| t.get_field(&field_atom)) {
                Some(f) => {
                    let fsym = self.synth_field_symbol(tid, &field_atom, f, field);
                    Resolution::Resolved(fsym)         // valid field
                }
                None => Resolution::Unresolved {        // field not on table
                    name: field_atom, reason: UnresolvedReason::NotInScope,
                },
            }
        }
        // qualifier resolved but no schema link (temp-table buffer, or
        // schema absent) → preserve today's behavior.
        _ if self.ctx.schema_loaded => Resolution::Unresolved {
            name: field_atom, reason: UnresolvedReason::External,
        },
        _ => Resolution::Unresolved {
            name: field_atom, reason: UnresolvedReason::NoSchema,
        },
    };
    self.references.insert(expr_id, resolution);
}
```

**`None` arm** — the qualifier did not resolve to any local buffer/table symbol.
Today (resolve.rs:1796-1820) this emits `NotInScope` on the qualifier **and** the
field under a loaded schema (the double FP). Add the schema fallback here: if
`ctx.schema_loaded` and `ctx.schema.table_id(&qatom)` is `Some(tid)`, synthesize
a default-buffer symbol via the same dedup cache used by `resolve_expr_ident`,
record the qualifier as `Resolved`, and then run the *identical* field-lookup
block as the `Some` arm. Only when the qualifier is neither a local symbol nor a
schema table does it fall to `NotInScope` (schema loaded) / `NoSchema`:

```rust
None => {
    if self.ctx.schema_loaded {
        if let Some(tid) = self.ctx.schema.table_id(&qatom) {
            // bare `Customer.Name` — synthesize a default buffer for the table
            let bsym = self.synth_table_buffer_symbol(tid, qid); // dedup-cached
            self.references.insert(qualifier.id, Resolution::Resolved(bsym));
            self.bump_count(bsym, AccessMode::Read);
            let field_atom = fold_atom(&field.name);
            let resolution = match self.ctx.schema.get_by_id(tid)
                .and_then(|t| t.get_field(&field_atom)) {
                Some(f) => Resolution::Resolved(
                    self.synth_field_symbol(tid, &field_atom, f, field)),
                None => Resolution::Unresolved {
                    name: field_atom, reason: UnresolvedReason::NotInScope },
            };
            self.references.insert(expr_id, resolution);
            return;
        }
    }
    // …unchanged: qualifier NotInScope (schema loaded) / NoSchema on both nodes.
}
```

Consequence: `Customer.Name` with **no** `DEFINE BUFFER` and a loaded schema
resolves the qualifier (no LINT0001) **and** the field with a real type (no
LINT0003) — the two FPs collapse together. A regression test pins exactly this.

`synth_field_symbol` looks up / inserts into the `(TableId, atom) -> SymbolId`
cache, minting a `Symbol { kind: Field, data_type: Some(resolved_ty),
declaration: NodeId::DUMMY, table_id: None, .. }` on a miss, where `resolved_ty`
is the `SchemaType -> ResolvedType` conversion below.

`resolve_expr_ident` (resolve.rs:1671-1710) schema fallback: after the
namespace-chain lookup fails and before/after the builtin check, if
`ctx.schema_loaded` and `ctx.schema.table_id(&atom)` is `Some(tid)`, synthesize a
`Buffer`-kind symbol carrying `table_id = Some(tid)` (dedup-cached per table) and
record `Resolved`. This makes a bare `Customer` resolve as a default buffer, and
— because it carries `table_id` — a subsequent `Customer.Name` field access
flows through the same field-lookup path.

`SchemaType -> ResolvedType` conversion does **not** exist yet. Add a
`ResolvedType::from_schema_field(field: &Field) -> ResolvedType` next to
`from_data_type` in crates/oxabl_semantic/src/types.rs:65. It must:

- Map each `SchemaType` primitive 1:1 to the corresponding
  `ResolvedType::Primitive(PrimitiveTy::_)`.
- Map `SchemaType::Unknown(_)` → `ResolvedType::Unknown`.
- Map `SchemaType::Error` → **`ResolvedType::Error`** (not `Unknown`, not a
  scalar) — `Error` is the "prior error, suppress cascade" bottom in the type
  lattice (types.rs:31-33) and must not be collapsed into `Unknown`.
- Wrap `EXTENT` fields as `ResolvedType::Array { element, extent }`, mirroring
  `wrap_extent` (resolve.rs:598) exactly: `field.extent: Option<u32>` becomes the
  `extent` and the scalar conversion becomes the boxed `element`. Taking the
  whole `&Field` (rather than `&SchemaType`) is what lets the converter see
  `field.extent`. LINT0004 extent/array *coercion* refinement is still a
  non-goal, but the field's type must be correctly *represented* as an array so
  that later work has the right input.

### Every downstream consumer that must change

- **check.rs** — `FieldAccess` arm (check.rs:503-508): replace the hard
  `ResolvedType::Unknown` with `self.type_from_reference(expr)`.
  `type_from_reference` (check.rs:547-560) needs **no change** — `SymbolKind::Field`
  already routes to `symbol.data_type`.
- **LINT0003** (`unknown_table_or_field.rs`): **no rule-logic change.** The rule
  already fires on `Unresolved { reason: NoSchema | NotInScope }` for
  field-access nodes (unknown_table_or_field.rs:246-250) and already early-returns
  when `!schema_loaded`. It is a no-op *today* only because
  `resolve_field_access` never produces `NotInScope` for a valid-qualifier field
  (it hard-codes `External`, which the rule skips). Once the resolve rewrite
  emits `NotInScope` for genuinely-absent fields and `Resolved` for valid ones,
  the rule starts firing correctly with zero edits to its matching. The work here
  is **doc + tests only**: rewrite the stale module doc that calls it a no-op,
  and add the tests below.
- **`SymbolKind::Field` doc** (`symbol.rs:41-44`): the current comment —
  *"Schema-table fields are resolved through `oxabl_schema::Field` directly and
  don't get a symbol."* — becomes **false** under Option A: this feature
  synthesizes a `Field` symbol for every referenced schema field. Update the
  doc comment to say `Field` now covers both temp-table fields and synthesized
  schema-table fields (the latter with `declaration: NodeId::DUMMY` and a
  schema-derived `data_type`).
- **LINT0001** (`undefined_symbol.rs`): verify only. It skips `External`; fields
  now emit `NotInScope` (handled by LINT0003) or `Resolved`, and synthetic field
  symbols must not leak into its undefined-checks. No code change expected;
  add a regression test.
- **LINT0004** (`type_mismatch_assignment.rs`): verify only. It benefits
  automatically — field-access nodes now carry a real primitive type, so
  matching-type assignments (`i = bCust.CustNum`, both INTEGER) stop being
  skipped-as-Unknown. Add a regression test.
- **oxabl_analyze** (`lib.rs`): the `symbols` section now contains synthetic
  field/table symbols (declaration = DUMMY, schema-derived types). Bump the
  `symbols` section version 1 → 2 (lib.rs:60) and note in the module doc that
  symbols include schema-derived entries. `references_json` (lib.rs:232-255)
  needs no change — synthetics serialize as ordinary `Resolved(SymbolId)` rows.
- **CLI** (`crates/oxabl/src/main.rs`): `--schema` flag + loader wiring on the
  **`analyze` subcommand only** (`run_analyze` replaces `Schema::empty()` at
  main.rs:192). `run_check` (main.rs:219) is parse-only and gets no flag.

---

## Implementation Steps

Ordered; each group is independently compilable and testable.

### 1. `Symbol` gains `table_id`
- `crates/oxabl_semantic/src/symbol.rs:91-115` — add
  `pub table_id: Option<oxabl_schema::TableId>` to `Symbol` (doc-comment: set
  only for `Buffer` / `TempTable`, valid only under the owning `Semantic`'s
  `schema_revision`). `oxabl_schema` is already a dependency (dossier §2).
- Update the `SymbolKind::Field` doc comment (symbol.rs:41-44), which currently
  claims schema fields "don't get a symbol" — now false under Option A.
- Update the `Symbol { .. }` literal in `declare` (resolve.rs:666-677) and any
  other construction site to default `table_id: None`.
- `crates/oxabl_semantic/src/types.rs:65` — add
  `ResolvedType::from_schema_field(&Field)` (EXTENT → `Array`, `Error` → `Error`;
  see Mechanics).

### 2. Declare-pass population
- `declare` signature (resolve.rs:628) grows `table_id: Option<TableId>`;
  `declare_simple` (resolve.rs:614) either forwards `None` or the `DefineBuffer`
  arm bypasses it.
- `DefineBuffer` arm (resolve.rs:197-198): match `target: BufferTarget`
  (statement.rs:834-839); for `Table(id)` compute
  `ctx.schema.table_id(&fold_atom(&id.name))`, for `TempTable` pass `None`.
- `ForEach` arm (resolve.rs:388-401): compute `table_id` from the `buffer`
  identifier the same way.
- Temp-table declare: pass `None` (unchanged behavior, explicit).
- **Capture the declare-time revision.** Extend `declare_pass`'s return tuple
  with `declare_revision: SchemaRevision` (= `ctx.schema.revision()`), threaded
  into `analyze_file` (lib.rs:84) and handed to `resolve_pass` for the staleness
  guard (Step 3).

### 3. Resolve rewrite
- **Thread `&mut SymbolTable` into `ResolveWalker` (borrow-plumbing refactor,
  not a verify).** `ResolveWalker` today (resolve.rs:744-753) has **no** `symbols`
  field — not even `&SymbolTable`. `resolve_pass` holds the `&mut SymbolTable`
  and only touches it in the prologue/epilogue (count flush); the walker never
  sees it. Symbol *synthesis* must `insert` mid-walk, so the walker needs
  `symbols: &'a mut SymbolTable`. Add the field, thread the borrow through
  `ResolveWalker::new`, and reconcile it against the existing end-of-pass count
  flush (which currently takes the `&mut SymbolTable` from `resolve_pass`
  directly) so there is exactly one live mutable borrow. This is real lifetime
  work, not a no-op verification — the prior draft's claim that the walker
  "already has it" was wrong.
- Add the `(TableId, OxablAtom) -> SymbolId` synth cache field to `ResolveWalker`.
- Add `synth_field_symbol` and a `synth_table_buffer_symbol` helper (both
  dedup-cached; both `insert` into `self.symbols`).
- Rewrite `resolve_field_access` (resolve.rs:1738-1822) per Design — **both** the
  `Some(qsym)` arm and the `None` arm (the `None`-arm schema fallback is the
  CRITICAL fix for bare `Customer.Name`).
- Add the schema fallback to `resolve_expr_ident` (resolve.rs:1671-1710) for
  bare identifiers used as standalone references (distinct from the field-access
  qualifier path above).
- Add the `debug_assert_eq!(declare_revision, self.ctx.schema.revision())`
  tripwire on the schema-lookup path (Step 2 supplies `declare_revision`).

### 4. Check-pass typing
- `check.rs:503-508` `FieldAccess` arm → `self.type_from_reference(expr)` after
  checking the qualifier.

### 5. LINT0003 — doc + tests only
- `crates/oxabl_lint/src/rules/unknown_table_or_field.rs` — **no matching-logic
  change.** The rule already fires on `NoSchema | NotInScope` under a loaded
  schema (unknown_table_or_field.rs:246-250) and already early-returns when
  `!schema_loaded`. Once Step 3 emits `NotInScope`/`Resolved` for fields, the
  rule fires correctly on its own. Work: rewrite the stale module doc that calls
  it a no-op, and add the LINT0003 tests below.

### 6. CLI wiring (`analyze` only)
- `crates/oxabl/src/main.rs` — add `#[arg(long = "schema")] schema: Option<PathBuf>`
  to the `Analyze` subcommand struct only (mirror `include-path` at
  main.rs:39-40). **Do not** add it to `Check`: `run_check` (main.rs:219) is
  parse-only and never runs semantic analysis, so the flag would be dead.
- In `run_analyze` (main.rs:135): if `Some(path)`, call
  `SchemaLoader::load_files(&[path], &fs)`, print load diagnostics to stderr
  (non-fatal), and build the `AnalysisContext` with the loaded schema. Default
  remains `Schema::empty()` (replacing main.rs:192).
- **Set `schema_loaded = true` explicitly when `--schema` was passed.**
  `AnalysisContext::new` derives `schema_loaded` from `!schema.is_empty()`
  (lib.rs:62), so an intentionally-empty `.df` would read as *not* loaded. When
  the user supplied `--schema`, construct the context so `schema_loaded` is
  `true` regardless of table count (via a direct struct literal or a
  loaded-flavored constructor) — the doc at lib.rs:43-46 already promises this
  independence, but `new()` does not honor it.

### 7. Analyze version bump
- `crates/oxabl_analyze/src/lib.rs:60` — `symbols` section 1 → 2; update module
  doc (lib.rs:8-24) to note schema-derived symbols.

---

## Testing

### Existing tests to preserve (must stay green)
- **Schema** (`oxabl_schema`): parser/model tests — no changes expected.
- **Resolve** (resolve.rs in-file tests): `resolve_field_access_no_schema_loaded_is_no_schema`
  (resolve.rs:3233), `resolve_field_access_schema_loaded_unknown_qualifier_is_not_in_scope`
  (resolve.rs:3261), buffer/for-each declare tests (resolve.rs:2099-2112, 2385).
  **`resolve_field_access_resolves_local_buffer_qualifier` (resolve.rs:3280-3307)
  must be updated**: it currently asserts the field is `External`; under a
  loaded schema with `Customer.CustNum` it becomes `Resolved`. Preserve an
  `External` assertion only for the temp-table-buffer (no `table_id`) case.
- **LINT0003**: `skip_list_no_fire_when_schema_not_loaded`,
  `fires_on_unknown_qualifier_when_schema_loaded`, `no_fire_on_non_field_expressions`,
  `no_fire_on_local_variable_reference`. `no_fire_when_qualifier_resolves_to_buffer_under_schema`
  must be updated to use a *valid* field (still no-fire) vs. the new
  unknown-field test.

### New tests

**crates/oxabl_semantic/src/resolve.rs** (a `with-schema` test harness that
builds a small in-memory `Schema` with a `Customer(CustNum INTEGER, Name CHARACTER)`
table; the existing `run_full_with_schema_loaded` at resolve.rs:2899 sets a flag
only — extend it to accept a real `&Schema`):
- `declare_buffer_links_schema_table_id` — `DEFINE BUFFER bCust FOR Customer` →
  buffer symbol `table_id == Some(customer_id)`.
- `declare_buffer_for_missing_table_is_none` — `DEFINE BUFFER b FOR Ghost` →
  `table_id == None`.
- `declare_buffer_for_temp_table_is_none` — `FOR TEMP-TABLE tt` → `None`.
- `for_each_implicit_buffer_links_table_id` — `FOR EACH Customer:` → `table_id == Some`.
- `unqualified_table_name_resolves_via_schema` — bare `Customer` used as a
  standalone reference (through `resolve_expr_ident`) resolves to a synthesized
  buffer symbol with `table_id == Some`.
- `bare_field_access_qualifier_resolves_via_schema` — **(CRITICAL-fix guard)**
  `Customer.Name` with **no** `DEFINE BUFFER`, schema loaded: qualifier node
  `Resolved` (not `NotInScope`), field node `Resolved` + typed `Character`, and
  no LINT0001 on `Customer` / no LINT0003 on `Name`. This is the headline
  double-FP case and exercises the `None`-arm schema fallback in
  `resolve_field_access` specifically (the `resolve_expr_ident` fallback does not
  reach a field-access qualifier).
- `field_access_valid_field_resolves_and_types` — `bCust.CustNum` →
  `references[node] == Resolved(fsym)` **and** the check pass writes
  `types[node] == Primitive(Integer)`.
- `field_access_invalid_field_is_not_in_scope` — `bCust.BadField` →
  `Unresolved { reason: NotInScope }`.
- `field_access_no_schema_still_no_schema` — schema absent → `NoSchema`
  (regression).
- `field_access_qualifier_resolves_schema_absent_is_external` — temp-table buffer
  under a loaded schema (qualifier resolves, no `table_id`) → still `External`,
  LINT0003 silent (the "qualifier-resolves-but-schema-absent" invariant).
- `duplicate_field_access_reuses_one_synthetic_symbol` — two `bCust.CustNum`
  references → symbol table grows by exactly one synthetic symbol (dedup cache).
- `stale_table_id_revision_guard` — build **two** distinct schemas via
  `SchemaLoader::load_files` (two `.df` inputs → two revisions; `SchemaRevision`
  cannot be minted by hand — its ctor is `pub(crate)`), run `declare_pass`
  against schema A and `resolve_pass` against schema B, and assert the
  `debug_assert_eq!(declare_revision, ctx.schema.revision())` fires
  (`#[should_panic]`, debug-only). Rationale: `Schema::get_by_id` is a bare `Vec`
  index (schema.rs:241) — a stale `TableId` silently reads the wrong table, so
  the guard is the only thing standing between a revision mismatch and a
  silently-wrong resolution.

**crates/oxabl_lint/src/rules/unknown_table_or_field.rs**
- `fires_on_unknown_field_when_schema_loaded` — `DEFINE BUFFER bCust FOR Customer.
  bCust.BadField.` → LINT0003 fires.
- `no_fire_on_valid_field_when_schema_loaded` — `bCust.CustNum` → no diagnostic.
- `no_fire_on_temp_table_field` — `DEFINE TEMP-TABLE tt FIELD f AS INT. tt.f` →
  no LINT0003 (field resolved locally / qualifier has no schema link).
- `no_fire_on_bare_valid_qualifier_field` — `Customer.Name` with **no**
  `DEFINE BUFFER`, schema loaded → field `Resolved`, no LINT0003 (the CRITICAL
  fix, viewed from the lint side).
- `fires_on_bare_unknown_table_field` — `Ghost.Field`, schema loaded, no buffer
  → qualifier stays `NotInScope` (not a schema table), field `NotInScope` →
  LINT0003 fires on the field (and LINT0001 on the qualifier — unchanged).

**crates/oxabl_lint/src/rules/type_mismatch_assignment.rs** (LINT0004)
- `no_false_positive_on_matching_field_type` — `DEF VAR i AS INT. i = bCust.CustNum.`
  → no LINT0004 (field now types as INTEGER, not Unknown).

**crates/oxabl_lint/src/rules/undefined_symbol.rs** (LINT0001)
- `synthetic_field_symbols_not_reported` — regression that a resolved schema
  field never surfaces as undefined-symbol.

**crates/oxabl_analyze** — golden update: add/refresh a dump fixture built with a
loaded schema, asserting the `symbols` section version is `2` and that
schema-derived symbols appear. Update any existing golden whose `symbols`
version literal is now stale.

### Verification
Run `oxabl analyze --schema <fixture.df> <fixture.p>` end-to-end (the `verify`
skill) to confirm the CLI path threads a loaded schema and LINT0003 fires on a
crafted unknown-field fixture.

---

## Benchmark

Add a **new schema-loaded resolve benchmark** to
`crates/oxabl_semantic/benches/semantic_bench.rs`. It is this feature's own
regression guard and is distinct from the existing fixtures — there are only two,
`TINY` and `MEDIUM` (semantic_bench.rs:17,24; driven by `bench_fixture` at
lines 119-120; **no** `LARGE` fixture exists). Both run against `Schema::empty()`
and therefore exercise **none** of the new field-lookup code.

- Fixture: a small in-memory `Schema` (a `Customer` table + an `Item` table with
  ~10 fields each) plus a field-access-heavy program (nested `FOR EACH` with
  many `Customer.Field` / `Item.Field` references and a `FIND` — the
  `SCHEMA_HEAVY` shape sketched in dossier §9).
- Group `schema_resolve` with `declare` and `resolve` sub-benches, mirroring
  `bench_fixture`'s `iter_batched` structure so the schema-lookup cost is
  isolated to the resolve timing.
- Rationale: field resolution adds a `Schema::get_by_id` index + `Table::get_field`
  atom scan + a synth-cache probe **per field access**. Nothing else in the
  suite covers this hot path; a schema-empty bench would show a flat line
  regardless of regressions here.

**Lint bench:** there is currently **no** `oxabl_lint` bench (confirmed: no
`benches/` dir under `crates/oxabl_lint`). The v1.1 plan proposes per-rule lint
benches as its own Phase 7a item. **Defer** it there — LINT0003's added cost is
a single `reason`-match per field node and is dominated by the resolve-pass
lookup this feature already benches. Adding a lint bench harness here would be
scope creep against a feature that belongs to the v1.1 follow-up.

---

## Risks & Edge Cases

- **Schema revision invalidation.** A `table_id` outliving its schema is
  use-after-free-shaped. Mitigated by the single-`&Schema`-per-`analyze_file`
  construction, the `debug_assert!` tripwire, and the staleness unit test. The
  atom-storage fallback (Open Questions) is the escape hatch if incremental
  re-analysis ever reuses symbols.
- **Buffer whose target table isn't in the schema.** `DEFINE BUFFER b FOR Ghost`
  → `table_id = None`. Field access on it falls to the `External` branch (silent),
  exactly today's behavior — no new false positives. LINT0003 does *not* fire on
  the qualifier here because the buffer symbol resolved; whether an
  unknown-*table* buffer should itself be flagged is deferred (Open Questions).
- **Ambiguous unqualified names (variable vs. table).** `resolve_expr_ident`
  walks the namespace chain **first** and only falls back to the schema on a
  miss. A local variable / buffer named `Customer` always shadows the schema
  table — matches ABL scoping and preserves user-shadowing (the same ordering
  the builtin fallback already uses, resolve.rs:1687-1702).
- **Per-field lookup performance.** `Table::get_field` is an atom-equality scan
  over the field vector (dossier §10: zero-allocation, folded atoms). For
  wide tables this is linear in field count per access. Acceptable for v1
  correctness; if the schema bench shows regression, `Table` can gain an
  `FxHashMap<OxablAtom, usize>` field index in a follow-up (out of scope here).
- **Symbol-table growth.** Bounded by *distinct fields/tables referenced* via the
  dedup cache, not by schema size. A test pins single-symbol dedup.
- **Synthetic symbols leaking into rules.** LINT0002 (unused) and LINT0001
  (undefined) must ignore DUMMY-declaration synthetics; covered by regression
  tests.

---

## Rollout

- Branch: `feat/semantic-schema-resolution` off `master`.
- Single self-contained PR (or two: Symbol+declare+resolve+check as one,
  LINT0003+CLI+analyze as a follow-up) with green CI: `cargo check`, `cargo test`,
  `cargo fmt --check`, `cargo clippy -D warnings`.
- Benchmarked independently via the new `schema_resolve` group; CodSpeed CI
  compares against `master` baseline.
- Reviewed independently of the other #58 features (inheritance, cross-file,
  flow, XREF audit), which remain separate branches.
- No AST change → no `docs/design/ast-invariants.md` update required
  (dossier §10). Update README "Current Status" LINT0003 line and the LINT0003
  rule doc to reflect that the rule now fires.

---

## Open Questions

1. **Synthetic symbol naming in dumps.** Should synthetic field symbols print as
   `Customer:CustNum` (table-qualified) to distinguish them from user-declared
   fields in the `oxabl analyze` text/JSON output? Leaning yes for debuggability.
2. **`table_id` vs. target-table atom.** If a future incremental/cross-file
   feature reuses a `Symbol` across schema revisions, switch the link to a folded
   `OxablAtom` and resolve to `TableId` lazily. Decide when that feature lands,
   not now.
3. **Flag unknown-table buffers.** `DEFINE BUFFER b FOR Ghost` silently yields
   `table_id = None`. Should this itself be a LINT0003 diagnostic ("buffer target
   table not found")? Out of scope here; needs a product call on noise.
4. **`--schema` ergonomics.** Single `--schema <path>` for v1. Multi-file
   (`--schema a.df --schema b.df`) or directory auto-discovery
   (`--schema-dir`) is deferred; `SchemaLoader::load_files` already accepts a
   slice, so multi-flag is a trivial later addition.
5. **`FIND` / `FOR … OF` implicit buffers.** Confirmed the declare pass does not
   declare a `FIND` buffer (resolve-only at resolve.rs:1158), and the buffer name
   goes through `resolve_statement_ident` (resolve.rs:1164), which no-ops
   silently on a miss — **not** through `resolve_expr_ident`, so the schema
   fallback added by this feature does **not** cover it. Bare `FIND Customer`
   thus stays unresolved (no FP, no resolution). Extending the schema fallback to
   `resolve_statement_ident` (so a `FIND Customer` binds a default buffer and its
   subsequent `Customer.Field` accesses type) is a natural follow-up but is out
   of scope here; flagged so the next iteration picks it up.
