---
title: "Complete TEMP-TABLE and BUFFER Parsing"
type: feat
status: completed
date: 2026-04-02
deepened: 2026-04-02
---

# Complete TEMP-TABLE and BUFFER Parsing

## Enhancement Summary

**Deepened on:** 2026-04-02
**Agents used:** Architecture Strategist, Pattern Recognition Specialist, Performance Oracle, Code Simplicity Reviewer, Spec Flow Analyzer

### Key Improvements
1. **Consolidated from 9 phases to 6** — merged lexer+dispatch (1+2), merged field+table LIKE (3+4), cut BEFORE-TABLE/REFERENCE-ONLY (defer to DATASET plan)
2. **Introduced `FieldTypeSource` enum** — replaces `Option<DataType>` + `Option<QualifiedName>` to eliminate impossible states. Matches existing `RunTarget` pattern.
3. **Introduced `BufferTarget` enum** — replaces `table: Identifier` + `for_temp_table: bool`. Same impossible-state elimination.
4. **Deferred scope modifiers** — `DEFINE NEW SHARED` should be done as a cross-cutting sweep across all DEFINE types, not piecemeal for temp-tables only.
5. **Skip+discard FORMAT/LABEL/SERIALIZE** — Low/None linting value per our own feature table. Parse to avoid choking, but don't store in AST.

### New Considerations Discovered
- `TEMP-TABLE` contains a hyphen. Adding it as a keyword will change lexer behavior globally. Must verify with lexer tests that existing hyphenated identifiers still work.
- `BEFORE-TABLE` is also hyphenated — codegen pipeline must handle this. Verify existing hyphenated keyword patterns (`NO-UNDO`, `NO-LOCK`).
- The VARIABLE option loop currently uses string comparison for INITIAL/EXTENT. Phase 1 should also migrate that loop for consistency.
- `match` is preferred over `if/else if` for option-parsing loops — idiomatic Rust, compiler-enforced exhaustiveness.
- Phase 5's recovery should use a `can_start_statement()` helper rather than an inline keyword list.
- ABL allows `INITIAL [1, 2, 3]` array syntax for EXTENT fields — `initial_value` should be `Option<Vec<Expression>>` not `Option<Expression>`.
- Index flags (PRIMARY, UNIQUE, WORD-INDEX) can appear without `IS`/`AS` prefix and in any order. Current parser only handles them after `IS`.
- Field-level `LIKE field VALIDATE` is distinct from table-level `LIKE table VALIDATE`. Both need support.
- BUFFER also supports NAMESPACE-URI, SERIALIZE-NAME, etc. — must skip these to avoid parse errors on valid ABL.
- `AS CLASS ClassName` for object-typed fields needs verification against `parse_data_type()`.

---

## Overview

The current parser handles a minimal subset of `DEFINE TEMP-TABLE` and `DEFINE BUFFER` syntax. This plan covers all remaining ABL features for both statements, prioritized by real-world frequency and linting value. The goal is not full semantic analysis, but complete **syntactic parsing** so the parser doesn't choke on production ABL code.

## Current State

**What works today:**
- `DEFINE TEMP-TABLE name [NO-UNDO] FIELD name AS type ... INDEX name [IS] [PRIMARY] [UNIQUE] field ... .`
- `DEFINE BUFFER name FOR table.`
- Dispatch via `eq_ignore_ascii_case` string comparison (no Kind variants for TEMP-TABLE/BUFFER)
- Silent skip of unrecognized tokens in temp-table body
- 4 tests covering basic cases

**What's missing (by ABL spec):**

| Feature | Frequency in Production | Linting Value | Plan Status |
|---------|------------------------|---------------|-------------|
| `LIKE table-name` | Very High | High | Phase 2 |
| `FIELD ... LIKE field` | Very High | High | Phase 2 |
| Field `INITIAL` value | High | Medium | Phase 2 |
| Field `EXTENT n` (arrays) | High | High | Phase 2 |
| Field `FORMAT` string | High | Low | Skip+discard |
| Field `LABEL` / `COLUMN-LABEL` | High | Low | Skip+discard |
| Index `ASCENDING` / `DESCENDING` | Medium | Low | Phase 3 |
| `NEW [GLOBAL] SHARED` / `SHARED` | Medium | High | Deferred |
| `FOR TEMP-TABLE tableName` in BUFFER | Medium | High | Phase 4 |
| `BEFORE-TABLE name` | Medium | Medium | Deferred |
| `WORD-INDEX` on indexes | Low | Low | Phase 3 |
| `VALIDATE` with LIKE | Low | Low | Phase 2 |
| `USE-INDEX` with LIKE | Low | Low | Phase 2 |
| `REFERENCE-ONLY` | Low | Medium | Deferred |
| `PRESELECT` on BUFFER | Low | Low | Phase 4 |
| `LABEL` on BUFFER | Low | Low | Phase 4 |
| XML/Serialization attrs | Low | None | Skip+discard |
| `NAMESPACE-URI/PREFIX` | Very Low | None | Skip+discard |
| `PRIVATE/PROTECTED/STATIC` | Class context only | Medium | Deferred |

## Execution Plan

### Phase 1: Lexer + Dispatch Refactor

Add proper Kind variants and switch parser dispatch from string comparison to Kind checks in one atomic change.

#### 1a. Add Missing Kind Variants

**Add to `resources/keyword_overrides.toml` and regenerate:**

- [ ] `TEMP-TABLE` → `Kind::TempTable`
- [ ] `BUFFER` → `Kind::Buffer`
- [ ] `INITIAL` / `INIT` → `Kind::Initial` (abbreviations: `init`, `initi`, `initia`, `initial`)
- [ ] `EXTENT` → `Kind::Extent`
- [ ] `PRIMARY` → `Kind::Primary` (currently matched via text comparison)
- [ ] `VALIDATE` → `Kind::Validate`
- [ ] `BEFORE-TABLE` → `Kind::BeforeTable` (verify codegen handles hyphenated keywords — follow `NO-UNDO`, `NO-LOCK` pattern)
- [ ] `WORD-INDEX` → `Kind::WordIndex`
- [ ] `PRESELECT` → `Kind::Preselect`

**Already have Kind variants (no action needed):**
`Like`, `LikeSequential`, `Format`, `Label`, `ColumnLabel`, `Ascending`, `Descending`, `Shared`, `Global`, `Unique`, `Field`, `Index`, `NoUndo`, `Is`, `KwAs`, `KwFor`, `UseIndex`

**Steps:**
1. Add entries to `resources/keyword_overrides.toml`
2. Run `cargo run -p oxabl_codegen`
3. Verify with `cargo check -p oxabl_lexer`
4. Update `is_callable_kind()` in `crates/oxabl_lexer/src/callable.rs` to include new variants where needed

#### 1b. Refactor Parser Dispatch

**File:** `crates/oxabl_parser/src/parser/statements.rs`

- [ ] In `parse_define_statement()`, replace `eq_ignore_ascii_case("temp-table")` with `self.check(Kind::TempTable)`
- [ ] Replace `eq_ignore_ascii_case("buffer")` with `self.check(Kind::Buffer)`
- [ ] Replace `eq_ignore_ascii_case("variable")` / `eq_ignore_ascii_case("var")` with Kind checks
- [ ] Remove the `is_callable_kind` guard — use specific Kind checks instead
- [ ] Migrate the VARIABLE option loop (lines 209-239) from `eq_ignore_ascii_case("initial")` / `eq_ignore_ascii_case("extent")` to `Kind::Initial` / `Kind::Extent`

#### 1c. Lexer Tests

- [ ] Verify `TEMP-TABLE` lexes as `Kind::TempTable` (not `Kind::Identifier`)
- [ ] Verify `temp-table` (lowercase) also maps correctly
- [ ] Verify existing hyphenated identifiers (e.g., `my-variable`) still lex as `Kind::Identifier`
- [ ] Verify `BEFORE-TABLE` lexes correctly

### Research Insights (Phase 1)

**Hyphenated keyword risk (Architecture Strategist):** When `TEMP-TABLE` becomes a keyword, the lexer will match it as `Kind::TempTable` instead of `Kind::Identifier`. This is correct for DEFINE contexts but could theoretically break if `TEMP-TABLE` appears as an identifier elsewhere. The lexer test suite should catch any regressions. Existing hyphenated keywords (`NO-UNDO`, `NO-LOCK`, `NO-ERROR`) demonstrate the codegen handles this correctly.

**Variable loop consistency (Pattern Recognition):** The VARIABLE option loop uses string comparison for INITIAL/EXTENT today. Adding `Kind::Initial` and `Kind::Extent` without migrating the VARIABLE loop creates an inconsistency where new code uses Kind while old code uses strings for the same keywords. Fix both in the same commit.

---

### Phase 2: Field Enhancements + LIKE Clause

Extend `TempTableField` and add table-level LIKE support in one phase (both modify `parse_define_temp_table`).

#### 2a. AST Changes

**File:** `crates/oxabl_ast/src/statement.rs`

- [ ] Replace `data_type: DataType` on `TempTableField` with `type_source: FieldTypeSource`
- [ ] Add `initial_value: Option<Vec<Expression>>` to `TempTableField` (scalar = 1-element vec; array = `INITIAL [1, 2, 3]`)
- [ ] Add `extent: Option<u32>` to `TempTableField` (None = scalar, Some(0) = dynamic)
- [ ] Add `like_table: Option<Identifier>` to `DefineTempTable`
- [ ] Add `use_indexes: Vec<UseIndex>` to `DefineTempTable`
- [ ] Add `validate: bool` to `DefineTempTable`

**New types:**

```rust
/// Source of a temp-table field's type — either explicit or inherited.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldTypeSource {
    /// Explicit type: `FIELD x AS INTEGER`
    Explicit(DataType),
    /// Inherited type: `FIELD x LIKE Customer.CustNum [VALIDATE]`
    Like { source: Identifier, validate: bool },
}

/// A USE-INDEX clause in a DEFINE TEMP-TABLE LIKE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseIndex {
    pub name: Identifier,
    pub as_primary: bool,
}
```

#### 2b. Parser Changes

**File:** `crates/oxabl_parser/src/parser/statements.rs`

- [ ] In `parse_define_temp_table()`, after NO-UNDO, check for `Kind::Like` or `Kind::LikeSequential`:
  - Parse table name into `like_table`
  - Optionally parse `Kind::Validate`
  - Parse zero or more `Kind::UseIndex` followed by name and optional `KwAs` `Kind::Primary`
- [ ] LIKE and explicit FIELDs can coexist (LIKE copies base, FIELD adds extra)
- [ ] In field parsing, after `FIELD name`, use `match` to dispatch:
  - `Kind::Like` → parse identifier into `FieldTypeSource::Like`
  - `Kind::KwAs` → parse data type into `FieldTypeSource::Explicit`
- [ ] After type source, parse field options using `match self.peek().kind`:
  - `Kind::Initial` → parse expression(s) into `initial_value` (handle `INITIAL [1, 2, 3]` array syntax)
  - `Kind::Extent` → parse optional integer into `extent`
  - `Kind::Format` | `Kind::Label` | `Kind::ColumnLabel` → **skip keyword + string value** (don't store)
  - `SERIALIZE-NAME` (text match) → skip keyword + string value
  - `SERIALIZE-HIDDEN` (text match) → skip
  - Other known field-option keywords (BGCOLOR, FGCOLOR, FONT, HELP, DECIMALS, VIEW-AS, etc.) → skip
  - `Kind::Field` | `Kind::Index` | `Kind::Period` → break

**Tests to add:**
- [ ] `FIELD name LIKE Customer.CustNum` — LIKE field reference
- [ ] `FIELD name LIKE Customer.CustNum VALIDATE` — field-level LIKE with VALIDATE
- [ ] `FIELD name LIKE Customer.CustNum LABEL "Number"` — LIKE with skipped override
- [ ] `FIELD name AS CHARACTER INITIAL ""` — initial empty string
- [ ] `FIELD name AS INTEGER INITIAL 0` — initial integer
- [ ] `FIELD name AS INTEGER EXTENT 5 INITIAL [1, 2, 3, 4, 5]` — array initial values
- [ ] `FIELD name AS INTEGER EXTENT 5` — array field
- [ ] `FIELD name AS CHARACTER FORMAT "x(20)"` — format string (verify skipped without error)
- [ ] `DEFINE TEMP-TABLE ttEmpty NO-UNDO.` — empty temp-table (no fields, no indexes)
- [ ] `DEFINE TEMP-TABLE tt LIKE Customer.` — basic table LIKE
- [ ] `DEFINE TEMP-TABLE tt LIKE Customer VALIDATE.` — LIKE with VALIDATE
- [ ] `DEFINE TEMP-TABLE tt LIKE Customer USE-INDEX CustNum USE-INDEX CountryPost AS PRIMARY.` — USE-INDEX
- [ ] `DEFINE TEMP-TABLE tt LIKE Customer FIELD extraField AS CHARACTER.` — LIKE + extra fields
- [ ] `DEFINE TEMP-TABLE tt NO-UNDO LIKE-SEQUENTIAL Customer.` — LIKE-SEQUENTIAL

**Update existing tests:**
- [ ] Update 3 existing temp-table tests to use `FieldTypeSource::Explicit(DataType::...)` instead of bare `DataType::...`

### Research Insights (Phase 2)

**`FieldTypeSource` enum eliminates impossible states (Architecture, Pattern Recognition, Performance):** Using `Option<DataType>` + `Option<QualifiedName>` creates 4 representable states, 2 of which are invalid. The enum makes exactly 2 states representable, both valid. This matches the `RunTarget` pattern already in the codebase. The parser code becomes cleaner:

```rust
let type_source = if self.check(Kind::Like) {
    self.advance();
    FieldTypeSource::Like(self.parse_identifier()?)
} else {
    self.expect_kind(Kind::KwAs, "Expected AS or LIKE after field name")?;
    FieldTypeSource::Explicit(self.parse_data_type()?)
};
```

**LIKE references as `Identifier` not `QualifiedName` (Simplicity):** For `FIELD x LIKE Customer.CustNum`, the lexer already produces the dot-separated tokens. The parser can capture `Customer.CustNum` as a single `Identifier` via the existing postfix/member-access parsing. If semantic analysis later needs to split qualifier from name, it can split on the dot. Introducing a `QualifiedName` struct for a single use case is premature.

**Skip+discard FORMAT/LABEL/SERIALIZE (Simplicity, Performance):** These have Low/None linting value per our feature table. Storing them adds 5 `Option` fields (~130 bytes) per field with no downstream consumer. Parse them to avoid choking, skip the value. If a future linter needs them, add the AST fields then.

---

### Phase 3: Index Enhancements

Add ASCENDING/DESCENDING and WORD-INDEX support.

**File:** `crates/oxabl_ast/src/statement.rs`

- [ ] Change `TempTableIndex.fields` from `Vec<Identifier>` to `Vec<IndexField>`
- [ ] Add `is_word_index: bool` to `TempTableIndex`

**New types:**
```rust
/// A field in an index definition with optional sort direction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexField {
    pub name: Identifier,
    pub direction: Option<SortDirection>,
}

/// Sort direction for index fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SortDirection {
    Ascending,
    Descending,
}
```

**File:** `crates/oxabl_parser/src/parser/statements.rs`

- [ ] In index field parsing loop, after each field name, check for `Kind::Ascending` or `Kind::Descending`
- [ ] Replace `eq_ignore_ascii_case("primary")` with `self.check(Kind::Primary)` in index parsing
- [ ] Parse index flags (PRIMARY, UNIQUE, WORD-INDEX) in a **loop accepting any order**:
  - Optionally consume `Kind::Is` or `Kind::KwAs` prefix (or neither — all three are valid)
  - Loop: check `Kind::Primary`, `Kind::Unique`, `Kind::WordIndex` — set flags, continue
  - Break when next token is not a flag keyword
  - This handles: `IS PRIMARY UNIQUE`, `AS UNIQUE PRIMARY`, `UNIQUE PRIMARY` (no IS/AS), `WORD-INDEX`, etc.

**Tests to add:**
- [ ] `INDEX idx1 CustNum ASCENDING Name DESCENDING.` — mixed directions
- [ ] `INDEX idx1 a DESC b c.` — direction propagation: parser stores `None` for b and c (propagation is semantic, not syntactic)
- [ ] `INDEX idx1 IS WORD-INDEX Name.` — word index
- [ ] `INDEX idx1 AS UNIQUE PRIMARY CustNum.` — AS instead of IS
- [ ] `INDEX idx1 PRIMARY UNIQUE CustNum.` — flags without IS/AS prefix
- [ ] `INDEX idx1 UNIQUE PRIMARY CustNum.` — flags in reverse order

**Update existing tests:**
- [ ] Update existing index tests to destructure `IndexField` instead of `Identifier`

### Research Insights (Phase 3)

**`IndexField` / `SortDirection` follow project patterns (Pattern Recognition):** Small enums and structs with doc comments are the norm. `SortDirection` as a two-variant enum is consistent with `FindType`, `LockType`, `ParameterDirection`. No issues.

**Descending propagation (ABL spec):** In ABL, `INDEX x a DESC b c` means b and c also get DESCENDING (the direction propagates to subsequent fields). The parser should store the explicitly-specified direction per field — the propagation rule is a semantic concern. Store `direction: None` for fields with no explicit direction marker.

---

### Phase 4: Buffer Enhancements

Extend BUFFER parsing for remaining ABL syntax.

**File:** `crates/oxabl_ast/src/statement.rs`

- [ ] Replace `table: Identifier` on `DefineBuffer` with `target: BufferTarget`
- [ ] Add `preselect: bool`
- [ ] Add `label: Option<String>`

**New type:**
```rust
/// Target of a DEFINE BUFFER statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BufferTarget {
    /// Buffer for a database table: `FOR Customer`
    Table(Identifier),
    /// Buffer for a temp-table: `FOR TEMP-TABLE ttCustomer`
    TempTable(Identifier),
}
```

**File:** `crates/oxabl_parser/src/parser/statements.rs`

- [ ] In `parse_define_buffer()`, after `Kind::KwFor`:
  - Check for `Kind::TempTable` → `BufferTarget::TempTable(self.parse_identifier()?)`
  - Otherwise → `BufferTarget::Table(self.parse_identifier()?)`
- [ ] After target, parse optional modifiers in a loop:
  - `Kind::Preselect` → set `preselect = true`
  - `Kind::Label` → parse string literal
  - Known XML/serialization keywords (NAMESPACE-URI, NAMESPACE-PREFIX, XML-NODE-NAME, SERIALIZE-NAME) → skip keyword + string value
  - Break on `Kind::Period`

**Tests to add:**
- [ ] `DEFINE BUFFER bTT FOR TEMP-TABLE ttCustomer.` — buffer for temp-table
- [ ] `DEFINE BUFFER bCust FOR Customer PRESELECT.` — preselect
- [ ] `DEFINE BUFFER bCust FOR Customer LABEL "Customer Buffer".` — label
- [ ] `DEFINE BUFFER bCust FOR Customer NAMESPACE-URI "urn:foo" SERIALIZE-NAME "cust".` — XML attrs skipped without error

**Update existing test:**
- [ ] Update `parse_define_buffer` test to use `BufferTarget::Table(...)` instead of bare `table`

### Research Insights (Phase 4)

**`BufferTarget` eliminates impossible states (Pattern Recognition):** Using `table: Identifier` + `for_temp_table: bool` allows representing `for_temp_table = true` with a database table name. The enum makes the two valid states (`FOR table` vs `FOR TEMP-TABLE table`) explicit and type-safe. Matches the `RunTarget` pattern.

---

### Phase 5: Robustness — Smart Error Recovery

Replace the `else { self.advance(); }` catch-all with structured recovery.

**File:** `crates/oxabl_parser/src/parser/statements.rs`

- [ ] Add a `can_start_statement(kind: Kind) -> bool` helper function that returns true for statement-starting keywords (DEFINE, DO, IF, REPEAT, FOR, PROCEDURE, RUN, DISPLAY, MESSAGE, FIND, CASE, RETURN, etc.)
- [ ] In the main `while` loop of `parse_define_temp_table()`, replace the blind skip with:
  1. Check `can_start_statement(self.peek().kind)` → emit error "Expected '.' to end DEFINE TEMP-TABLE"
  2. For known-but-unhandled keywords (XML-NODE-NAME, NAMESPACE-URI, etc.) → skip keyword + value using a precise mini-parser
  3. Otherwise → skip single token (future-proofing)

**Tests to add:**
- [ ] `DEFINE TEMP-TABLE tt FIELD x AS INTEGER NAMESPACE-URI "foo" NAMESPACE-PREFIX "bar".` — skips XML attrs gracefully
- [ ] `DEFINE TEMP-TABLE tt FIELD x AS INTEGER DEFINE VARIABLE y AS INTEGER.` — error on missing period
- [ ] `DEFINE TEMP-TABLE .` — error: missing table name
- [ ] `DEFINE TEMP-TABLE tt FIELD .` — error: missing field name
- [ ] `DEFINE TEMP-TABLE tt FIELD x AS .` — error: missing data type
- [ ] `DEFINE BUFFER FOR Customer.` — error: missing buffer name
- [ ] `DEFINE BUFFER bCust Customer.` — error: missing FOR keyword

### Research Insights (Phase 5)

**`can_start_statement()` helper (Architecture Strategist):** An explicit keyword list in the recovery code requires maintenance every time a new statement type is added. A centralized helper function is more maintainable and can be tested independently. Start with the obvious keywords (DEFINE, DO, IF, PROCEDURE, END) — these cover 90%+ of real "forgot the period" cases.

**Precise skip logic (Performance Oracle):** When skipping known keywords like `NAMESPACE-URI "foo"`, be precise about how many tokens to skip. Use named helper functions (e.g., `skip_keyword_with_string_value()`) rather than a generic "skip N tokens" approach to prevent parser desynchronization.

---

## Acceptance Criteria

- [ ] `DEFINE TEMP-TABLE` parses all common production patterns without error
- [ ] `DEFINE BUFFER` handles `FOR TEMP-TABLE` syntax
- [ ] No string-comparison dispatch remains for DEFINE sub-types
- [ ] Silent token skip replaced with explicit keyword handling
- [ ] All existing 184 tests continue to pass
- [ ] ~35-40 new tests covering all phases (including error paths and edge cases)
- [ ] `cargo fmt`, `cargo clippy -D warnings`, and `cargo test` all pass

## Dependencies & Risks

- **Lexer codegen:** Phase 1 requires adding keywords to `keyword_overrides.toml` and regenerating. `TEMP-TABLE` is currently tokenized as a hyphenated identifier. Adding it as a keyword changes lexer behavior globally. Must verify with tests.
- **`FieldTypeSource` replaces `data_type`:** Existing tests that pattern-match on `TempTableField.data_type` need updating. Do in the same commit as the struct change.
- **`IndexField` breaking change:** Changing `Vec<Identifier>` to `Vec<IndexField>` in Phase 3 will break existing tests. Update them in the same commit.
- **`BufferTarget` breaking change:** Changing `table: Identifier` to `target: BufferTarget` in Phase 4 breaks the existing buffer test. Update in the same commit.

## Deferred Work (Not In Scope)

- **Scope modifiers (`NEW [GLOBAL] SHARED`):** Should be done as a cross-cutting sweep across ALL DEFINE types (VARIABLE, TEMP-TABLE, BUFFER, PARAMETER) in one pass, not piecemeal. The silent-skip fallback handles these gracefully in the meantime.
- **BEFORE-TABLE / REFERENCE-ONLY:** ProDataSet features with no DATASET parser to consume them. Add when implementing `DEFINE DATASET`.
- **Class-context modifiers:** `PRIVATE`, `PROTECTED`, `STATIC`, `SERIALIZABLE` — only valid inside CLASS definitions. Defer until CLASS parsing is implemented.
- **Dynamic TEMP-TABLE:** `CREATE TEMP-TABLE handle` — entirely different syntax, separate plan.
- **ProDataSet:** `DEFINE DATASET` with data relations. Depends on temp-table being complete.
- **FORMAT/LABEL/SERIALIZE-NAME on AST:** Currently skip+discarded. Add AST fields if a linter ever needs them.
- **`QualifiedName` struct for LIKE references:** Currently using `Identifier`. Add structured splitting when name resolution is implemented.
- **`Identifier.name` as `Cow<'source, str>`:** Highest-impact single performance optimization for the entire parser. Out of scope here.
