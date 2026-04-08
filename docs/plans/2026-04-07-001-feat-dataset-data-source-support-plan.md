---
title: "feat: Add dataset and data-source parsing support"
type: feat
status: active
date: 2026-04-07
origin: docs/brainstorms/2026-04-07-dataset-support-brainstorm.md
---

# feat: Add dataset and data-source parsing support

## Enhancement Summary

**Deepened on:** 2026-04-07
**Sections enhanced:** AST design, parser implementation, performance, tests
**Review agents used:** architecture-strategist, code-simplicity-reviewer, pattern-recognition-specialist, performance-oracle, software-architect

### Key Improvements
1. **CreateTarget redesign** — Collapsed Dataset/DataSource/TempTable into a single `Handle` variant with `CreateTargetKind` enum, preventing future breaking changes when adding CREATE QUERY/BUFFER/SERVER
2. **ParameterType simplification** — Extracted `HandlePassingOptions` struct to deduplicate append/bind/by_value across 4 variants; collapsed into fewer variants with a `HandleParamKind` discriminant
3. **XmlSerializeOptions fields** — Changed from `Option<String>` to `Option<Identifier>` for AST consistency and to avoid unnecessary heap allocations
4. **Naming convention alignment** — Documented `is_` prefix for OO modifiers vs bare names for ABL keywords; `serializable: Option<bool>` replaced with two bools matching codebase style
5. **Phase ordering fix** — Derive `Default` on `XmlSerializeOptions` to decouple AST changes (Phase 2f) from parser implementation (Phase 3), preventing compilation failures between phases

### New Considerations Discovered
- `CreateTarget::Record` naming is misleading for non-record CREATE variants (WIDGET-POOL, SERVER, etc.) — renamed to `Name`
- `DefineParameter` restructuring touches Method.parameters and Constructor.parameters match sites across the codebase — grep before starting
- `UseIndex.as_primary` vs `TempTableIndex.is_primary` inconsistency exists in current code — new code should use `is_` consistently

## Overview

Add full parsing support for ABL ProDataSet and DataSource constructs: DEFINE DATASET, DEFINE DATA-SOURCE, CREATE DATASET, CREATE DATA-SOURCE, and DEFINE PARAMETER variants for dataset/dataset-handle/table/table-handle/buffer. Also retrofit proper parsing of shared XML/serialize clauses (NAMESPACE-URI, XML-NODE-NAME, SERIALIZE-NAME, etc.) on existing DEFINE TEMP-TABLE and DEFINE BUFFER parsers, replacing the current skip-unknown-token hack.

## Problem Statement / Motivation

Datasets are fundamental to ABL — they group temp-tables with defined relationships and are heavily used in data exchange (JSON/XML serialization), web services, and application server communication. The parser currently cannot parse any dataset or data-source definitions, and the existing temp-table/buffer parsers silently discard XML/serialize clauses as unknown tokens (`statements.rs:771`). This work closes one of the last major gaps in statement parsing coverage.

(see brainstorm: `docs/brainstorms/2026-04-07-dataset-support-brainstorm.md`)

## Proposed Solution

### Phase 1: Lexer — Add missing keywords

Add to `resources/keyword_overrides.toml` and regenerate via `cargo run -p oxabl_codegen`:

**Shared XML/serialize keywords** (used by TEMP-TABLE, BUFFER, and DATASET):
- `NAMESPACE-URI` (Option)
- `NAMESPACE-PREFIX` (Option)
- `XML-NODE-NAME` (Option)
- `XML-NODE-TYPE` (Option)
- `SERIALIZE-NAME` (Option)
- `SERIALIZE-HIDDEN` (Option)

**Dataset-specific keywords:**
- `SERIALIZABLE` (Option)
- `NON-SERIALIZABLE` (Option)
- `REFERENCE-ONLY` (Option)
- `RELATION-FIELDS` (Option)
- `NESTED` (Option)
- `FOREIGN-KEY-HIDDEN` (Option)
- `NOT-ACTIVE` (Option)
- `RECURSIVE` (Option)
- `PARENT-ID-RELATION` (Statement)
- `PARENT-ID-FIELD` (Option)
- `PARENT-FIELDS-BEFORE` (Option)
- `PARENT-FIELDS-AFTER` (Option)

**Other:**
- `WIDGET-POOL` (Option) — for CREATE ... IN WIDGET-POOL

**Existing tokens confirmed present:** `Dataset`, `DataRelation`, `DatasetHandle`, `DataSource`, `Reposition`, `Query`, `Rowid`, `Keys`

**Abbreviations:** Most hyphenated compound keywords (NAMESPACE-URI, SERIALIZE-NAME, etc.) do NOT have abbreviations in ABL. Verify against `resources/abl_keyword_index.html` during implementation. SERIALIZABLE, NESTED, and RECURSIVE should be checked.

**Files:**
- `resources/keyword_overrides.toml` — add entries
- Run `cargo run -p oxabl_codegen` — regenerates `crates/oxabl_lexer/src/kind.rs` and `crates/oxabl_lexer/build.rs`

### Phase 2: AST — Add new types and extend existing ones

**File: `crates/oxabl_ast/src/statement.rs`**

#### 2a. Shared XML/serialize options struct

Extract a reusable struct for the clauses shared across TEMP-TABLE, BUFFER, and DATASET. All six fields are valid on all three consumers per the ABL grammar.

```rust
/// XML and serialization options shared by TEMP-TABLE, BUFFER, and DATASET definitions.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct XmlSerializeOptions {
    pub namespace_uri: Option<Identifier>,
    pub namespace_prefix: Option<Identifier>,
    pub xml_node_name: Option<Identifier>,
    pub xml_node_type: Option<Identifier>,
    pub serialize_name: Option<Identifier>,
    pub serialize_hidden: bool,
}
```

### Research Insights (Phase 2a)

**Why `Option<Identifier>` not `Option<String>`:** The existing AST uses `Identifier` (which carries both a `Span` and a `name: String`) for all named references. Using `Option<String>` would break AST consistency and lose source location information. Since these are string literal values from source, `Identifier` gives us both the value and the span for future diagnostics. (Performance oracle, pattern recognition)

**Why `#[derive(Default)]`:** This decouples Phase 2f (adding `xml_options` to existing AST nodes) from Phase 3 (implementing the parser). Existing test construction sites can use `xml_options: XmlSerializeOptions::default()` without immediately needing parser changes. Without this, all `DefineTempTable` and `DefineBuffer` tests break between phases. (Architecture strategist)

#### 2b. Add `DefineDataset` variant to `Statement` enum

```rust
Statement::DefineDataset {
    name: Identifier,
    access: Option<AccessModifier>,    // PRIVATE | PROTECTED
    is_static: bool,
    is_new_shared: bool,               // TODO: Retrofit on DefineTempTable, DefineBuffer, VariableDeclaration
    is_shared: bool,                   // TODO: Retrofit on DefineTempTable, DefineBuffer, VariableDeclaration
    serializable: bool,                // SERIALIZABLE keyword present
    non_serializable: bool,            // NON-SERIALIZABLE keyword present
    xml_options: XmlSerializeOptions,
    reference_only: bool,
    buffers: Vec<Identifier>,          // FOR buffer1, buffer2, ...
    data_relations: Vec<DataRelation>,
    parent_id_relations: Vec<ParentIdRelation>,
}
```

### Research Insights (Phase 2b)

**Why two bools instead of `Option<bool>`:** The existing AST exclusively uses plain `bool` for flags (`is_abstract`, `is_final`, `no_undo`, `no_error`). `Option<bool>` with 3 states (Some(true)/Some(false)/None) is inconsistent with codebase conventions and adds cognitive load. Two bools (`serializable` and `non_serializable`) match existing patterns. Both false = unspecified; at most one true = explicitly stated. (Code simplicity reviewer, pattern recognition)

**NEW SHARED / SHARED TODO comments:** These flags are scoped to DefineDataset/DefineDataSource only for this PR. The TODO comments make the gap discoverable for the follow-up retrofit. Do NOT create a shared `DefineModifiers` struct yet — wait until the retrofit PR where the actual field set stabilizes. (Software architect)

#### 2c. Supporting types for dataset relations

```rust
pub struct DataRelation {
    pub name: Option<Identifier>,
    pub parent_buffer: Identifier,
    pub child_buffer: Identifier,
    pub relation_fields: Vec<(Identifier, Identifier)>, // (parent_field, child_field) pairs
    pub reposition: bool,
    pub nested: bool,
    pub foreign_key_hidden: bool,
    pub not_active: bool,
    pub recursive: bool,
}

pub struct ParentIdRelation {
    pub name: Option<Identifier>,
    pub parent_buffer: Identifier,
    pub child_buffer: Identifier,
    pub id_field: Identifier,
    pub parent_fields_before: Vec<Identifier>,
    pub parent_fields_after: Vec<Identifier>,
}
```

#### 2d. Add `DefineDataSource` variant

```rust
Statement::DefineDataSource {
    name: Identifier,
    access: Option<AccessModifier>,
    is_static: bool,
    query: Option<Identifier>,          // FOR QUERY query-name
    source_buffers: Vec<DataSourceBuffer>,
}

pub struct DataSourceBuffer {
    pub name: Identifier,
    pub keys: Option<DataSourceKeys>,   // KEYS ( field1, field2 ) or KEYS ( ROWID )
}

pub enum DataSourceKeys {
    Fields(Vec<Identifier>),
    Rowid,
}
```

#### 2e. Extend `Statement::Create`

The current `Create` variant is `Create { buffer: Identifier, no_error: bool }`. Restructure to handle typed creation:

```rust
Statement::Create {
    target: CreateTarget,
    no_error: bool,
}

pub enum CreateTarget {
    /// CREATE buffer-name (existing behavior — also covers CREATE WIDGET-POOL, CREATE SERVER, etc.)
    Name(Identifier),
    /// CREATE DATASET/DATA-SOURCE/TEMP-TABLE handle [IN WIDGET-POOL pool]
    Handle {
        kind: CreateTargetKind,
        handle: Identifier,
        widget_pool: Option<Expression>,
    },
}

pub enum CreateTargetKind {
    Dataset,
    DataSource,
    TempTable,
}
```

### Research Insights (Phase 2e)

**Why `Name` not `Record`:** The fallback path handles any CREATE that takes a plain identifier — not just database records. `CREATE WIDGET-POOL`, `CREATE SERVER`, `CREATE SOCKET`, `CREATE BROWSE`, etc. all follow this pattern. "Record" is misleading. (Architecture strategist)

**Why collapse into `Handle` variant:** Dataset, DataSource, and TempTable CREATE statements have identical structure (keyword + handle + optional widget-pool). A single `Handle` variant with a `CreateTargetKind` discriminant means extending to CREATE QUERY, CREATE BUFFER, etc. later requires only adding a variant to `CreateTargetKind` — no structural change to the `CreateTarget` enum. (Software architect)

**Future extensibility:** When CREATE QUERY/BUFFER/SERVER support is added, just add variants to `CreateTargetKind`. The `Name` fallback continues to catch everything the parser doesn't specifically recognize.

#### 2f. Add XML/serialize options to existing AST nodes

Add `xml_options: XmlSerializeOptions` field to:
- `Statement::DefineTempTable` 
- `Statement::DefineBuffer`

#### 2g. Extend `Statement::DefineParameter`

Currently parameters are: `{ direction, name, data_type, no_undo }`. Add new variants for typed parameters:

```rust
Statement::DefineParameter {
    direction: ParameterDirection,
    param_type: ParameterType,
}

pub enum ParameterType {
    /// Standard variable parameter: DEFINE INPUT PARAMETER name AS type [NO-UNDO].
    Variable {
        name: Identifier,
        data_type: DataType,
        no_undo: bool,
    },
    /// Handle-based parameter: TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE
    Handle {
        kind: HandleParamKind,
        name: Identifier,
        passing: HandlePassingOptions,
    },
    /// Buffer parameter: DEFINE PARAMETER BUFFER buf FOR table
    Buffer {
        name: Identifier,
        target: Identifier,
    },
}

pub enum HandleParamKind {
    Table,          // DEFINE INPUT PARAMETER TABLE FOR tt-name
    TableHandle,    // DEFINE OUTPUT PARAMETER TABLE-HANDLE handle
    Dataset,        // DEFINE INPUT PARAMETER DATASET FOR ds-name
    DatasetHandle,  // DEFINE OUTPUT PARAMETER DATASET-HANDLE handle
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct HandlePassingOptions {
    pub append: bool,
    pub bind: bool,
    pub by_value: bool,
}
```

### Research Insights (Phase 2g)

**Why collapse 4 variants into `Handle`:** Table, TableHandle, Dataset, DatasetHandle all carry identical fields (`name`, `append`, `bind`, `by_value`). Four separate variants with duplicated fields is a textbook extraction candidate. A single `Handle` variant with a `HandleParamKind` discriminant eliminates 9 redundant field declarations and gives a single place to add future passing modes (e.g., BY-REFERENCE). (Code simplicity reviewer, software architect)

**Breaking change scope:** This restructuring changes every `DefineParameter` match site. Before starting Phase 2g, run `grep -rn "DefineParameter" crates/` to scope all affected locations — this includes `Method.parameters`, `Constructor.parameters`, `Function.parameters`, and all test assertions. (Architecture strategist)

### Phase 3: Parser — Shared XML/serialize clause helper

**File: `crates/oxabl_parser/src/parser/statements.rs`**

Extract a reusable method:

```rust
fn parse_xml_serialize_options(&mut self) -> XmlSerializeOptions {
    // Loop consuming NAMESPACE-URI, NAMESPACE-PREFIX, XML-NODE-NAME,
    // XML-NODE-TYPE, SERIALIZE-NAME (all take a string literal value),
    // and SERIALIZE-HIDDEN (flag, no value).
    // Returns when current token is not one of these kinds.
}
```

Retrofit `parse_define_temp_table()` and `parse_define_buffer()`:
- Call `parse_xml_serialize_options()` at the appropriate point in each parser
- Remove the skip-unknown-token hack from both methods
- Add `xml_options` to the returned AST nodes

**Clause ordering:** The ABL grammar places XML/serialize options before the main body (before FIELD/INDEX for temp-tables, before FOR for datasets). The parser should consume them in order, matching the grammar. Unknown tokens after XML options should still trigger an error or be skipped with the existing recovery pattern — but NAMESPACE-URI etc. will no longer be "unknown."

### Phase 4: Parser — DEFINE DATASET

**File: `crates/oxabl_parser/src/parser/statements.rs`**

#### 4a. Dispatch in `parse_define_statement()`

The existing dispatch already parses access modifiers and STATIC before routing. Add `Kind::Dataset` check after FRAME, before VARIABLE:

```rust
// After existing STATIC parsing and before VARIABLE...
if self.check(Kind::Dataset) {
    return self.parse_define_dataset(access, is_static);
}
```

Also add `Kind::DataSource`:
```rust
if self.check(Kind::DataSource) {
    return self.parse_define_data_source(access, is_static);
}
```

Also handle SERIALIZABLE/NON-SERIALIZABLE modifiers before DATASET:
```rust
let serializable = if self.check(Kind::Serializable) {
    self.advance();
    Some(true)
} else if self.check(Kind::NonSerializable) {
    self.advance();
    Some(false)
} else {
    None
};
```

The full modifier order is: `[NEW] [SHARED] | [PRIVATE|PROTECTED] [STATIC] [SERIALIZABLE|NON-SERIALIZABLE]`. Note PRIVATE/PROTECTED and STATIC can appear in either order (`DEFINE STATIC PRIVATE DATASET` is valid). The existing code handles access modifiers and STATIC for PROPERTY already — the same pattern applies.

**Note:** NEW SHARED / SHARED is NOT currently parsed by `parse_define_statement()` or represented in any AST node. This is a pre-existing gap across all DEFINE statements, not dataset-specific. For this work, add `Kind::New` + `Kind::Shared` / `Kind::Shared` handling to the dispatch and pass the flags into `DefineDataset` and `DefineDataSource`. Retrofitting NEW SHARED on TEMP-TABLE/BUFFER/VARIABLE is out of scope for this plan but should be a follow-up.

#### 4b. Implement `parse_define_dataset()`

Follow the temp-table loop pattern:

1. Advance past `DATASET`
2. Parse identifier (dataset name)
3. Call `parse_xml_serialize_options()` for namespace/XML/serialize clauses
4. Check for `REFERENCE-ONLY` flag
5. Expect `FOR`, parse comma-separated buffer identifiers
6. Enter loop: dispatch on `Kind::DataRelation` → `parse_data_relation()`, `Kind::ParentIdRelation` → `parse_parent_id_relation()`
7. Break on `Kind::Period` or statement-starting token
8. Expect period

#### 4c. Implement `parse_data_relation()`

```
DATA-RELATION [name] FOR parent, child
    RELATION-FIELDS (pf1, cf1 [, pfN, cfN]...)
    [REPOSITION] [NESTED [FOREIGN-KEY-HIDDEN]] [NOT-ACTIVE] [RECURSIVE]
```

1. Advance past `DATA-RELATION`
2. Optional: if next token is an identifier (not `FOR`), parse as relation name
3. Expect `FOR`, parse parent buffer identifier, expect comma, parse child buffer identifier
4. Expect `RELATION-FIELDS`, expect `(`, parse comma-separated field pairs, expect `)`
5. Parse optional flags in a loop: REPOSITION, NESTED (optionally followed by FOREIGN-KEY-HIDDEN), NOT-ACTIVE, RECURSIVE

#### 4d. Implement `parse_parent_id_relation()`

```
PARENT-ID-RELATION [name] FOR parent, child
    PARENT-ID-FIELD id-field
    [PARENT-FIELDS-BEFORE (f1 [, fN]...)]
    [PARENT-FIELDS-AFTER (f1 [, fN]...)]
```

1. Advance past `PARENT-ID-RELATION`
2. Optional relation name (same pattern as DATA-RELATION)
3. Expect `FOR`, parse parent and child buffer identifiers
4. Expect `PARENT-ID-FIELD`, parse field identifier
5. Optional `PARENT-FIELDS-BEFORE` → expect `(`, parse comma-separated fields, expect `)`
6. Optional `PARENT-FIELDS-AFTER` → same pattern

### Phase 5: Parser — DEFINE DATA-SOURCE

**File: `crates/oxabl_parser/src/parser/statements.rs`**

```
DEFINE [PRIVATE|PROTECTED] [STATIC] DATA-SOURCE name
    FOR [QUERY query-name]
    source-buffer [, source-buffer]...
```

Where each `source-buffer` is: `buffer-name [KEYS (field1 [, fieldN]... | ROWID)]`

1. Advance past `DATA-SOURCE`
2. Parse identifier (data-source name)
3. Expect `FOR`
4. Optional: if `QUERY`, advance and parse query name identifier
5. Parse comma-separated source buffer phrases, each with optional KEYS clause
6. Expect period

### Phase 6: Parser — CREATE DATASET / CREATE DATA-SOURCE / CREATE TEMP-TABLE

**File: `crates/oxabl_parser/src/parser/statements.rs`**

Restructure `parse_create_statement()` (currently at line 2182):

```rust
fn parse_create_statement(&mut self) -> ParseResult<Statement> {
    self.advance(); // consume CREATE
    
    // Check for typed CREATE variants (DATASET, DATA-SOURCE, TEMP-TABLE)
    let target = if let Some(kind) = self.match_create_target_kind() {
        self.advance(); // consume the type keyword
        let handle = self.parse_identifier()?;
        let widget_pool = self.parse_optional_widget_pool()?;
        CreateTarget::Handle { kind, handle, widget_pool }
    } else {
        let name = self.parse_identifier()?;
        CreateTarget::Name(name)
    };
    
    let no_error = self.parse_no_error();
    self.expect_kind(Kind::Period, "Expected '.' after CREATE statement")?;
    Ok(Statement::Create { target, no_error })
}

fn match_create_target_kind(&self) -> Option<CreateTargetKind> {
    match self.peek().kind {
        Kind::Dataset => Some(CreateTargetKind::Dataset),
        Kind::DataSource => Some(CreateTargetKind::DataSource),
        Kind::TempTable => Some(CreateTargetKind::TempTable),
        _ => None,
    }
}

fn parse_optional_widget_pool(&mut self) -> ParseResult<Option<Expression>> {
    if self.check(Kind::In) {
        self.advance(); // consume IN
        self.expect_kind(Kind::WidgetPool, "Expected WIDGET-POOL after IN")?;
        Ok(Some(self.parse_expression()?))
    } else {
        Ok(None)
    }
}
```

### Phase 7: Parser — DEFINE PARAMETER extensions

**File: `crates/oxabl_parser/src/parser/statements.rs`**

Extend `parse_define_parameter()` (currently at line 442). After consuming the direction and PARAMETER keyword, dispatch on the next token:

- `Kind::Table` → expect `FOR`, parse temp-table name → `Handle { kind: Table, ... }`
- `Kind::TableHandle` → parse handle name → `Handle { kind: TableHandle, ... }`
- `Kind::Dataset` → expect `FOR`, parse dataset name → `Handle { kind: Dataset, ... }`
- `Kind::DatasetHandle` → parse handle name → `Handle { kind: DatasetHandle, ... }`
- `Kind::Buffer` → parse buffer name, expect `FOR`, parse table name → `Buffer { ... }`
- Default → existing `name AS type` path → `Variable { ... }`

For all `Handle` variants, parse `HandlePassingOptions` after the name:
```rust
fn parse_handle_passing_options(&mut self) -> HandlePassingOptions {
    let mut opts = HandlePassingOptions::default();
    loop {
        match self.peek().kind {
            Kind::Append => { self.advance(); opts.append = true; }
            Kind::Bind => { self.advance(); opts.bind = true; }
            Kind::ByValue => { self.advance(); opts.by_value = true; }
            _ => break,
        }
    }
    opts
}
```

### Phase 8: Tests

**File: `crates/oxabl_parser/src/parser/tests.rs`**

Follow existing convention: `#[test]` functions named `parse_<construct>_<variant>()`.

**DEFINE DATASET tests:**
- `parse_define_dataset_basic` — minimal: `DEFINE DATASET ds FOR ttA.`
- `parse_define_dataset_multiple_buffers` — `DEFINE DATASET ds FOR ttA, ttB, ttC.`
- `parse_define_dataset_data_relation` — single relation with RELATION-FIELDS
- `parse_define_dataset_multiple_relations` — multiple DATA-RELATIONs
- `parse_define_dataset_relation_flags` — REPOSITION, NESTED, FOREIGN-KEY-HIDDEN, NOT-ACTIVE, RECURSIVE
- `parse_define_dataset_parent_id_relation` — with PARENT-ID-FIELD, PARENT-FIELDS-BEFORE/AFTER
- `parse_define_dataset_mixed_relations` — DATA-RELATION + PARENT-ID-RELATION together
- `parse_define_dataset_xml_serialize_options` — NAMESPACE-URI, XML-NODE-NAME, SERIALIZE-NAME, SERIALIZE-HIDDEN
- `parse_define_dataset_reference_only` — REFERENCE-ONLY flag
- `parse_define_dataset_modifiers` — NEW SHARED, SHARED, PRIVATE/PROTECTED STATIC SERIALIZABLE (merged — both test modifier dispatch)

**DEFINE DATA-SOURCE tests:**
- `parse_define_data_source_basic` — `DEFINE DATA-SOURCE ds FOR buf.`
- `parse_define_data_source_with_query` — `DEFINE DATA-SOURCE ds FOR QUERY q buf.`
- `parse_define_data_source_multiple_buffers` — multiple source-buffer-phrases
- `parse_define_data_source_with_keys` — `KEYS (field1, field2)`
- `parse_define_data_source_with_rowid_key` — `KEYS (ROWID)`
- `parse_define_data_source_access_static` — PRIVATE STATIC

**CREATE tests:**
- `parse_create_dataset` — basic + with IN WIDGET-POOL (two assertions in one test)
- `parse_create_data_source` — `CREATE DATA-SOURCE hDs.`
- `parse_create_temp_table` — `CREATE TEMP-TABLE hTt.`
- `parse_create_name` — existing behavior preserved: `CREATE Customer.`

**DEFINE PARAMETER tests:**
- `parse_define_parameter_dataset` — `DEFINE INPUT PARAMETER DATASET FOR dsName.`
- `parse_define_parameter_dataset_handle` — `DEFINE OUTPUT PARAMETER DATASET-HANDLE hDs.`
- `parse_define_parameter_table_for` — `DEFINE INPUT PARAMETER TABLE FOR ttName.`
- `parse_define_parameter_table_handle` — `DEFINE OUTPUT PARAMETER TABLE-HANDLE hTt.`
- `parse_define_parameter_buffer` — `DEFINE PARAMETER BUFFER bCust FOR Customer.`
- `parse_define_parameter_bind_append` — with BIND and APPEND flags

**Retrofit tests:**
- `parse_define_temp_table_xml_options` — verify NAMESPACE-URI, SERIALIZE-NAME etc. are now properly parsed (not skipped)
- `parse_define_buffer_xml_options` — same for DEFINE BUFFER

### Phase 9: Benchmarks

**Files:**
- `resources/bench_parser_datasets.abl` — new fixture file with diverse dataset/data-source definitions
- `crates/oxabl_parser/benches/parser_bench.rs` — register new fixture

Fixture content should include:
- Simple dataset with one relation
- Complex dataset with multiple relations, all flags, XML options
- Data-source definitions with QUERY and KEYS
- CREATE DATASET/DATA-SOURCE statements
- Mix of shared, static, and access-modified definitions

## Technical Considerations

**Performance:** All new keywords are dispatched via `Kind` enum matching (O(1)), per the established pattern. No runtime string comparisons. The shared `parse_xml_serialize_options()` helper uses the same `check(Kind::X)` pattern. Adding ~20 Kind variants to the existing ~524 has negligible impact on `match_keyword()`. The longest new keyword (`PARENT-FIELDS-BEFORE`, 20 bytes) fits well within the `[u8; 64]` stack buffer. (Performance oracle)

**Error recovery:** Dataset definitions can be long (multiple relations). The parser should follow the existing temp-table pattern: loop until `Kind::Period` or end, break on statement-starting tokens (indicating missing period), skip truly unknown tokens. DATA-RELATION and PARENT-ID-RELATION sub-parsers should not attempt cross-relation recovery — if a relation is malformed, error out and let the outer loop handle it.

**Clause ordering is strict:** The ABL grammar is positional — namespace/XML options come before FOR, DATA-RELATIONs come before PARENT-ID-RELATIONs. The parser enforces this ordering rather than accepting any permutation. This matches the compiler behavior.

**Modifier ordering is flexible:** PRIVATE/PROTECTED and STATIC can appear in either order (`DEFINE STATIC PRIVATE DATASET` is valid). The existing `parse_define_statement()` already handles this for PROPERTY — same pattern applies.

### Naming Conventions (from pattern recognition analysis)

Follow the existing dual convention for boolean fields:
- **`is_` prefix** for OO/structural modifiers: `is_static`, `is_abstract`, `is_primary`, `is_unique` — use for `is_static`, `is_new_shared`, `is_shared`
- **Bare name** for ABL keyword flags: `no_undo`, `no_error`, `preselect`, `validate` — use for `reference_only`, `serializable`, `non_serializable`, `reposition`, `nested`, `foreign_key_hidden`, `not_active`, `recursive`, `append`, `bind`, `by_value`

Use `Option<T>` for optional single values, `Vec<T>` (empty = absent) for repeatable clauses. Never use sentinel defaults.

**Inconsistency to avoid:** The existing code has `UseIndex.as_primary` alongside `TempTableIndex.is_primary` for the same concept. New code should consistently use `is_` for this type of modifier.

## Acceptance Criteria

- [ ] All new keywords added to `keyword_overrides.toml` and codegen runs cleanly
- [ ] `parse_define_dataset()` handles all documented clauses: modifiers, XML/serialize options, REFERENCE-ONLY, FOR buffers, DATA-RELATION (with RELATION-FIELDS, REPOSITION, NESTED, FOREIGN-KEY-HIDDEN, NOT-ACTIVE, RECURSIVE), PARENT-ID-RELATION (with PARENT-ID-FIELD, PARENT-FIELDS-BEFORE/AFTER)
- [ ] `parse_define_data_source()` handles QUERY and source-buffer-phrases with KEYS
- [ ] `parse_create_statement()` dispatches on DATASET, DATA-SOURCE, and TEMP-TABLE with optional IN WIDGET-POOL
- [ ] `parse_define_parameter()` handles TABLE, TABLE-HANDLE, DATASET, DATASET-HANDLE, and BUFFER parameter types
- [ ] DEFINE TEMP-TABLE and DEFINE BUFFER properly parse XML/serialize options (skip-unknown hack removed)
- [ ] All existing tests still pass (no regressions from AST restructuring)
- [ ] New tests cover all clause combinations listed in Phase 8
- [ ] Benchmark fixture added and discoverable by CodSpeed
- [ ] `cargo clippy -D warnings` passes
- [ ] `cargo fmt --check` passes

## Dependencies & Risks

**AST breaking changes:** Restructuring `Statement::Create` and `Statement::DefineParameter` will break existing test assertions and all match sites across the codebase. **Before starting Phase 2**, run `grep -rn "DefineParameter\|Statement::Create" crates/` to enumerate every affected location. This includes `Method.parameters`, `Constructor.parameters`, `Function.parameters`, and all test assertions. Risk is low — the changes are mechanical — but scope must be understood upfront. (Architecture strategist)

**Keyword count growth:** Adding ~20 new keywords increases the `Kind` enum and `match_keyword()` function. The codegen handles this automatically. The longest new keyword (`PARENT-FIELDS-BEFORE`, 20 bytes) fits well within the `[u8; 64]` stack buffer. Verify the compile-time assertion still passes. (Performance oracle)

**Shared clause parsing order:** The XML/serialize options helper must be called at the right point in each parser (temp-table, buffer, dataset). Getting the call site wrong could change parsing behavior for existing constructs. The retrofit tests guard against this. `XmlSerializeOptions::default()` on existing AST nodes during the transition further mitigates this risk. (Architecture strategist)

**Phase 2f/3 coupling:** Adding `xml_options: XmlSerializeOptions` to existing AST nodes (Phase 2f) would break all existing `DefineTempTable` and `DefineBuffer` test construction sites before the parser populates them (Phase 3). Mitigated by `#[derive(Default)]` on `XmlSerializeOptions` — existing sites use `xml_options: XmlSerializeOptions::default()`. (Architecture strategist)

## Sources & References

### Origin

- **Brainstorm document:** [docs/brainstorms/2026-04-07-dataset-support-brainstorm.md](docs/brainstorms/2026-04-07-dataset-support-brainstorm.md) — Key decisions: dataset methods as regular postfix calls, all documented clauses in scope, retrofit XML/serialize on temp-table/buffer.

### Internal References

- DEFINE dispatch: `crates/oxabl_parser/src/parser/statements.rs:266`
- TEMP-TABLE parser: `crates/oxabl_parser/src/parser/statements.rs:507`
- Skip-unknown hack (buffer): `crates/oxabl_parser/src/parser/statements.rs:771`
- Skip-unknown hack (temp-table): `crates/oxabl_parser/src/parser/statements.rs:717`
- CREATE parser: `crates/oxabl_parser/src/parser/statements.rs:2182`
- PARAMETER parser: `crates/oxabl_parser/src/parser/statements.rs:442`
- AST statements: `crates/oxabl_ast/src/statement.rs`
- Keyword overrides: `resources/keyword_overrides.toml`
- Performance learnings: `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md`

### External References

- ABL DATASET reference: `abl-context/logical_data_(datasets_temp-tables).txt` (lines 173-372)
- ABL DATA-SOURCE reference: same file (lines 384-474)
- ABL CREATE DATASET: same file (lines 1-37)
- ABL CREATE DATA-SOURCE: same file (lines 39-56)
