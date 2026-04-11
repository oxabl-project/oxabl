---
title: "feat: Support LIKE syntax in DEFINE VARIABLE / DEFINE PARAMETER / VAR"
type: feat
status: completed
date: 2026-04-10
---

# feat: Support LIKE Syntax in DEFINE VARIABLE / DEFINE PARAMETER / VAR

## Enhancement Summary

**Deepened on:** 2026-04-10
**Research agents used:** architecture-strategist, code-simplicity-reviewer, pattern-recognition-specialist, performance-oracle, best-practices-researcher, learnings-researcher

### Key Improvements
1. **Naming corrected** — `TypeSpec` → `TypeSource`; `As(DataType)` variant → `Explicit(DataType)` to match the established `FieldTypeSource` convention in the codebase
2. **Field naming corrected** — `type_spec` → `type_source` to follow the `TempTableField.type_source: FieldTypeSource` precedent
3. **`FieldTypeSource` unification added** — two parallel near-identical enums is a YAGNI violation; move `validate: bool` from the enum variant onto `TempTableField` and reuse one type everywhere
4. **OO-ABL gap identified** — `parse_parenthesized_params` (method/constructor signatures) is a separate LIKE-eligible path not covered by the plan
5. **`parse_type_spec()` helper justified with scope bounds** — only valid at 3 sites; leave the other 8 `parse_data_type()` call sites untouched
6. **Derive macros specified** — `#[derive(Debug, Clone, PartialEq, Eq)]`; no `Copy` (contains `String` via `DataType::Class`)

### New Considerations Discovered
- `TriggerAssignParam.data_type: DataType` should stay `DataType` — LIKE is not valid there (ABL requires AS)
- `Statement::Property.data_type: DataType` is a grey area — ABL does not support `DEFINE PROPERTY LIKE` (confirmed via Progress Community)
- The `validate: bool` in `FieldTypeSource::Like` belongs on `TempTableField` structurally, not in the variant — moving it is the correct refactor
- All test patterns matching `data_type` (~30–50 sites in `tests.rs`) will require mechanical updating after the field rename

---

## Overview

ABL's `LIKE` phrase is a structural copy mechanism that lets a variable or parameter inherit its data type, format, label, initial value, decimals, and extent from an existing database field, temp-table field, or previously-defined variable. It is the **direct alternative to `AS`** — the two are mutually exclusive.

Currently the oxabl parser hard-requires `AS` after a variable/parameter name and will error on any ABL code using `LIKE`. This plan adds full support across all three relevant DEFINE paths.

---

## Problem Statement / Motivation

Real-world ABL codebases use `LIKE` extensively — especially for input/output parameters that mirror database field types and for variables that must stay in sync with the schema. Without `LIKE` support, `oxabl check` will reject or misparse large portions of any non-trivial ABL codebase.

`Kind::Like` already exists in the lexer. The pattern for parsing `LIKE field` is already established (`FieldTypeSource::Like { source, validate }`). This is a well-scoped extension.

---

## Proposed Solution

### Step 1 — Extend the AST

#### 1a. Unify `FieldTypeSource` into a shared `TypeSource` enum

The codebase already has `FieldTypeSource` for temp-table fields:

```rust
// Current (crates/oxabl_ast/src/statement.rs, lines 671–677)
pub enum FieldTypeSource {
    Explicit(DataType),
    Like { source: Identifier, validate: bool },
}
```

The plan would introduce a near-identical enum for variable/parameter declarations. Rather than creating two parallel enums, the correct approach is:

1. Move `validate: bool` out of the `Like` variant and onto `TempTableField` directly (it is only meaningful there)
2. Rename `FieldTypeSource` to `TypeSource` (or keep `FieldTypeSource` for backward compat and add a new `TypeSource` without `validate` — see decision note below)
3. Use `TypeSource` in both `TempTableField` and `VariableDeclaration`/`ParameterType::Variable`

**Decision point — two approaches:**

| Approach | Pros | Cons |
|---|---|---|
| **A: Rename `FieldTypeSource` → `TypeSource`, move `validate` to `TempTableField`** | One enum, no duplication | More diff churn (all `FieldTypeSource` match sites) |
| **B: Keep `FieldTypeSource` as-is, add new `TypeSource` without `validate`** | Minimal scope | Two near-identical enums forever |

**Recommendation: Approach A.** The codebase is pre-1.0 and this is the right time to unify. Approach B is a YAGNI debt that will cost more later.

**Resulting type:**

```rust
/// Source of a type specification — either explicit (AS type) or inherited (LIKE field).
///
/// Used in variable declarations, parameter declarations, and temp-table field definitions.
/// For temp-table fields, the enclosing struct carries any additional VALIDATE flag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSource {
    /// Explicit type: `... AS INTEGER` or `... AS CLASS Foo`
    Explicit(DataType),
    /// Inherited type: `... LIKE Customer.CustNum`
    Like { source: Identifier },
}
```

#### 1b. Affected AST nodes

**`TempTableField`** (`statement.rs`, lines ~679–690):
- `type_source: FieldTypeSource` → `type_source: TypeSource` (same field name, new type)
- Add `validate: bool` as a direct field on `TempTableField` (moved from `FieldTypeSource::Like`)

**`VariableDeclaration`** (`statement.rs`, lines ~22–29):
- `data_type: DataType` → `type_source: TypeSource`

**`ParameterType::Variable`** (`statement.rs`, lines ~860–878):
- `data_type: DataType` → `type_source: TypeSource`

**`TriggerAssignParam.data_type: DataType`** — leave unchanged. LIKE is not valid in `TRIGGER PROCEDURE FOR ASSIGN` (ABL requires explicit AS); keeping `DataType` here encodes that constraint in the type system.

**`Statement::Property.data_type: DataType`** — leave unchanged. DEFINE PROPERTY does not support LIKE in ABL (confirmed by Progress Community tracker — it is an open enhancement request, not implemented).

**Derives required on `TypeSource`:**

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
// No Copy — DataType::Class(String) is non-Copy
// No Default — no sensible default
// No Hash — not needed yet
```

### Step 2 — Update the Parser

Three change sites in `crates/oxabl_parser/src/parser/statements.rs`. Only these three sites should change; the other 8 call sites for `parse_data_type()` (function return types, property types, method return types, trigger parameters) must **not** be routed through the new path — those positions syntactically disallow LIKE.

#### 2a. DEFINE VARIABLE (line ~460) and DEFINE PARAMETER standard arm (line ~632)

Both use an explicit `AS` keyword. Replace the hard `expect_kind(Kind::KwAs, ...)` call with:

```rust
let type_source = self.parse_type_source()?;
```

New helper `parse_type_source()` in `mod.rs` — handles `AS type | LIKE field` where `AS` is syntactically required:

```rust
fn parse_type_source(&mut self) -> Result<TypeSource, ParseError> {
    if self.check(Kind::Like) {
        self.advance(); // consume LIKE
        let source = self.parse_qualified_identifier()?;
        Ok(TypeSource::Like { source })
    } else {
        self.expect_kind(Kind::KwAs, "Expected AS or LIKE")?;
        let data_type = self.parse_data_type()?;
        Ok(TypeSource::Explicit(data_type))
    }
}
```

Note: `parse_type_source()` is justified as an extraction because both DEFINE VARIABLE and DEFINE PARAMETER share the AS-consuming path exactly. It should **not** be `#[inline]` proactively — LLVM will inline if warranted; annotate only after a flamegraph confirms it.

#### 2b. VAR (line ~512)

VAR has **no `AS` keyword** — the type appears immediately after `VAR`. `parse_type_source()` cannot be used here. Inline the branch:

```rust
// Old:
let data_type = self.parse_data_type()?;
// ...
Statement::VariableDeclaration { name, data_type, ... }

// New:
let type_source = if self.check(Kind::Like) {
    self.advance(); // consume LIKE
    TypeSource::Like { source: self.parse_qualified_identifier()? }
} else {
    TypeSource::Explicit(self.parse_data_type()?)
};
// ...
Statement::VariableDeclaration { name, type_source, ... }
```

#### 2c. VAR dispatch lookahead (line ~271)

The VAR dispatch guard uses `is_data_type_kind()`. Do **not** add `Kind::Like` to `is_data_type_kind()` — LIKE is not a data type keyword. Add an explicit OR-branch:

```rust
// Old:
if self.check(Kind::Variable) && Self::is_data_type_kind(self.peek_at(1).kind) {

// New:
if self.check(Kind::Variable)
    && (Self::is_data_type_kind(self.peek_at(1).kind)
        || self.check_at(1, Kind::Like))
{
```

#### 2d. Temp-table field parser (line ~810–826)

The temp-table field LIKE parser currently sets `type_source: FieldTypeSource::Like { source, validate }`. After renaming, update to:

```rust
// Old result:
FieldTypeSource::Like { source, validate }

// New result:
// type_source on TempTableField:
TypeSource::Like { source }
// validate stored as direct TempTableField field:
field.validate = true;  // or pass through the struct literal
```

#### 2e. OO-ABL method/constructor signatures — identified gap

`parse_parenthesized_params()` at line ~413 in `statements.rs` parses parameter lists for `METHOD` and `CONSTRUCTOR` definitions. It currently constructs `ParameterType::Variable { data_type: self.parse_data_type()?, .. }` directly. Once `ParameterType::Variable` is updated to `type_source: TypeSource`, this site will need the same `parse_type_source()` call.

**Decision needed:** Is `DEFINE METHOD foo (INPUT x LIKE Customer.CustNum)` valid ABL? If yes, this must be included in this PR. If not confirmed, add a TODO comment at the site and defer to a follow-up.

### Step 3 — Add Tests

All new tests go in `crates/oxabl_parser/src/parser/tests.rs`:

| Test name | Input | Verifies |
|---|---|---|
| `parse_define_variable_like` | `DEFINE VARIABLE v LIKE Customer.CustName NO-UNDO.` | `TypeSource::Like { source: "Customer.CustName" }` |
| `parse_define_variable_like_simple_name` | `DEFINE VARIABLE v LIKE iVar NO-UNDO.` | Non-dotted source identifier |
| `parse_define_variable_like_with_format` | `DEFINE VARIABLE v LIKE Customer.CustName FORMAT "X(40)" NO-UNDO.` | LIKE + FORMAT override coexist |
| `parse_var_like` | `VAR LIKE Customer.CustName vCust.` | VAR form with LIKE |
| `parse_define_input_parameter_like` | `DEFINE INPUT PARAMETER p LIKE Customer.CustName NO-UNDO.` | Input direction |
| `parse_define_output_parameter_like` | `DEFINE OUTPUT PARAMETER p LIKE Order.Amount NO-UNDO.` | Output direction |
| `parse_define_input_output_parameter_like` | `DEFINE INPUT-OUTPUT PARAMETER p LIKE Invoice.InvNum NO-UNDO.` | Input-output direction |

Also add a regression test verifying `validate: bool` still works on temp-table fields after the `FieldTypeSource` → `TypeSource` migration:

| Test name | Input | Verifies |
|---|---|---|
| `parse_temp_table_field_like_validate_regression` | `DEFINE TEMP-TABLE tt FIELD f LIKE Customer.CustName VALIDATE.` | `validate: true` on `TempTableField`, `type_source: TypeSource::Like` |

---

## Technical Considerations

- **`Kind::Like` already exists** — no lexer or codegen changes required.
- **`parse_qualified_identifier()`** already handles dotted names like `Customer.CustName` — reuse it directly.
- **Test churn**: approximately 30–50 match sites in `tests.rs` reference `data_type` or `FieldTypeSource::Explicit` by name — all will require mechanical updating. These are compile-time errors, not logic errors.
- **`FieldTypeSource` removal**: remove `FieldTypeSource` entirely in the same commit that introduces `TypeSource`. Do not leave both types in the codebase — the next contributor will use whichever they find first.
- **`parse_type_source()` scope**: only call from the three variable/parameter declaration sites. The eight other `parse_data_type()` call sites (function return types, property types, method return types, trigger parameters, etc.) must remain on `parse_data_type()` directly — those positions do not permit LIKE.
- **Industry precedent**: oxc, SWC, and sqlparser-rs all use sibling enum variants for the "explicit type vs reference" distinction (e.g., `TSType::TSKeywordType` vs `TSType::TSTypeRef`). `TypeSource::Explicit` / `TypeSource::Like` follows this pattern exactly.
- **No semantic resolution here**: `TypeSource::Like { source }` remains an unresolved name reference. Do not resolve LIKE during parsing — that requires a schema/symbol table and belongs in a future analysis pass.

---

## System-Wide Impact

- **`VariableDeclaration.data_type` rename** → `type_source`: run `grep -r "data_type" crates/` before merging to audit all consumers.
- **`FieldTypeSource` removal**: all pattern matches on `FieldTypeSource::Explicit` and `FieldTypeSource::Like` must be updated to `TypeSource::Explicit` and `TypeSource::Like`.
- **`TempTableField.validate` addition**: any code that constructs `TempTableField` literals needs updating.
- **`ParameterType::Variable { data_type, .. }` destructuring**: update all match arms.
- **No external API breakage**: no public crate API surface yet; all changes are internal to the workspace.
- **Future formatter**: will need to emit `AS <type>` for `TypeSource::Explicit` and `LIKE <name>` for `TypeSource::Like` — a straightforward `match` on the variant; span information is preserved on both `DataType` and `Identifier`.

---

## Acceptance Criteria

- [x] `TypeSource` enum added to `oxabl_ast` with `Explicit(DataType)` and `Like { source: Identifier }` variants, `#[derive(Debug, Clone, PartialEq, Eq)]`
- [x] `FieldTypeSource` removed; all former usages migrated to `TypeSource`
- [x] `validate: bool` moved from `FieldTypeSource::Like` to `TempTableField` as a direct field
- [x] `VariableDeclaration.data_type: DataType` → `type_source: TypeSource`
- [x] `ParameterType::Variable.data_type: DataType` → `type_source: TypeSource`
- [x] `parse_type_source()` helper added; called only from DEFINE VARIABLE and DEFINE PARAMETER paths
- [x] VAR dispatch guard at line ~271 extended with `|| self.check_at(1, Kind::Like)` (not via `is_data_type_kind`)
- [x] DEFINE VARIABLE `LIKE` parses correctly (simple name + dotted name)
- [x] VAR `LIKE` parses correctly via inline branch (no `AS` consumed)
- [x] DEFINE PARAMETER (all directions) `LIKE` parses correctly
- [x] `TriggerAssignParam.data_type` and `Property.data_type` remain `DataType` (unchanged)
- [x] All 8 new tests pass (7 LIKE tests + 1 validate regression)
- [x] `cargo test` passes workspace-wide
- [x] `cargo clippy -D warnings` clean
- [x] `VALIDATE` is NOT accepted on DEFINE VARIABLE LIKE

---

## Out of Scope

- `DEFINE PROPERTY` — LIKE is not valid ABL syntax; `Property.data_type` stays `DataType`
- `DEFINE DATASET` — LIKE is not applicable
- Semantic resolution of `LIKE` sources (requires a schema/symbol table, future work)
- EXTENT semantic inheritance from LIKE source — `EXTENT [constant]` is already parsed as a separate clause on `VariableDeclaration.extent` and continues to work unchanged; resolving what extent a source *field* has is a future semantic concern
- OO-ABL method parameter LIKE (Step 2e) — confirm ABL validity first, then add in a follow-up or include in this PR after confirmation

---

## Sources & References

### Internal References

- DEFINE VARIABLE parser: `crates/oxabl_parser/src/parser/statements.rs:424–465`
- VAR parser: `crates/oxabl_parser/src/parser/statements.rs:508–550`
- DEFINE PARAMETER parser: `crates/oxabl_parser/src/parser/statements.rs:630–635`
- `parse_parenthesized_params` (OO-ABL gap): `crates/oxabl_parser/src/parser/statements.rs:413–500`
- Existing `FieldTypeSource` (to be replaced): `crates/oxabl_ast/src/statement.rs:671–677`
- `TempTableField` struct: `crates/oxabl_ast/src/statement.rs:679–690`
- Temp-table field LIKE parser: `crates/oxabl_parser/src/parser/statements.rs:810–826`
- `parse_qualified_identifier()`: `crates/oxabl_parser/src/parser/mod.rs:330–357`
- `parse_data_type()`: `crates/oxabl_parser/src/parser/mod.rs:465–517`
- `is_data_type_kind()`: `crates/oxabl_parser/src/parser/mod.rs:303–324`
- `DataType` enum: `crates/oxabl_ast/src/statement.rs:576–600`
- `VariableDeclaration` struct: `crates/oxabl_ast/src/statement.rs:22–29`
- `ParameterType` enum: `crates/oxabl_ast/src/statement.rs:860–878`
- `TriggerAssignParam`: `crates/oxabl_ast/src/statement.rs:1031`
- `Kind::Like` token: `crates/oxabl_lexer/src/kind.rs:488`
- Existing variable tests: `crates/oxabl_parser/src/parser/tests.rs:1801–1911`
- Existing parameter tests: `crates/oxabl_parser/src/parser/tests.rs:~2968–3043`

### External References

- [DEFINE VARIABLE syntax — Progress docs](https://docs.progress.com/bundle/abl-reference/page/DEFINE-VARIABLE-statement.html)
- [DEFINE VARIABLE syntax — consultingwerk mirror OE 11.3](https://help.consultingwerkcloud.com/openedge/113/langref-42_2.html) *(confirmed: VALIDATE absent from DEFINE VARIABLE LIKE; LIKE inherits data type, format, label, initial, decimals, extent, case-sensitivity; does NOT inherit help or validate options)*
- [DEFINE PARAMETER syntax — consultingwerk mirror OE 11.3](https://help.consultingwerkcloud.com/openedge/113/langref-35_2.html)
- [oxc TSType enum (reference for variant-per-source pattern)](https://docs.rs/oxc/latest/oxc/ast/ast/enum.TSType.html)
- [Cargo SemVer compatibility (re-exporting enums)](https://doc.rust-lang.org/cargo/reference/semver.html)
