---
title: "refactor: Migrate parser keyword workarounds to proper lexer tokens"
type: refactor
status: completed
date: 2026-04-04
origin: docs/brainstorms/2026-04-04-migrate-parser-keyword-workarounds-brainstorm.md
---

# refactor: Migrate parser keyword workarounds to proper lexer tokens

## Enhancement Summary

**Deepened on:** 2026-04-04
**Sections enhanced:** 6
**Research agents used:** architecture-strategist, code-simplicity-reviewer, pattern-recognition-specialist, performance-oracle, best-practices-researcher

### Key Improvements
1. Expanded scope: data type keywords (INTEGER, CHARACTER, etc.) now included — they had the same string-comparison anti-pattern in `parse_data_type()`
2. Data type Kind variants enable positive lookahead (`is_data_type_kind()`) for VAR disambiguation — architecturally cleaner than negative `!check_at(1, Kind::Equals)`
3. Added `check_at(offset, kind)` as the minimal lookahead primitive
4. Confirmed approach matches industry best practices (Roslyn, TypeScript, rust-analyzer all use 1-token lookahead for contextual keywords)

### New Considerations Discovered
- Expression parser's primary path must handle new Kind variants in fallthrough (verified: `can_be_identifier()` is used in `parse_identifier()` which feeds into expression parsing)
- Pre-existing duplicate `Kind::Run` dispatch at lines 99 & 109 — opportunistic cleanup
- Pre-existing `Keyfunction`/`KeyFunction` naming inconsistency in generated kind.rs — separate codegen concern

---

## Overview

Multiple ABL keywords are handled via string comparison in the parser instead of proper `Kind` token dispatch. This includes:

1. **Statement keywords** (VARIABLE/VAR, FUNCTION, CATCH, FINALLY) — dispatched via `eq_ignore_ascii_case()` / `is_identifier_text()` in statement parsing
2. **Data type keywords** (INTEGER, CHARACTER, DECIMAL, etc.) — resolved via `text.to_uppercase()` + string matching in `parse_data_type()`

Both violate the same principle: **the lexer should classify tokens as distinctly as possible so the parser dispatches on token kind, not token text.** This refactor adds all missing keywords to `keyword_overrides.toml`, regenerates the lexer, and updates the parser to use `Kind` variants everywhere — following the pattern established in commit `f6f1853` (TEMP-TABLE/BUFFER migration).

## Problem Statement

The parser has two keyword dispatch mechanisms:

1. **Correct:** `self.check(Kind::Keyword)` — used for DEFINE, DO, IF, PROCEDURE, etc.
2. **Workaround:** String comparison against token text — used in two places:
   - `text.eq_ignore_ascii_case("keyword")` / `is_identifier_text("keyword")` for VAR, FUNCTION, CATCH, FINALLY
   - `text.to_uppercase()` + `match` for all data type keywords in `parse_data_type()`

The workaround exists because these are **unreserved** ABL keywords absent from `abl_reserved_keywords.txt`. They need manual `keyword_overrides.toml` entries. PROCEDURE got this treatment early; the others used string comparisons as an expedient shortcut during rapid development (see brainstorm: `docs/brainstorms/2026-04-04-migrate-parser-keyword-workarounds-brainstorm.md`).

## Proposed Solution

Add all missing keywords to `keyword_overrides.toml`, regenerate lexer files, update the parser to dispatch on `Kind` variants everywhere, and add unreserved keywords to `can_be_identifier()`.

### Research Insights

**Industry Precedent:**
This approach — "lex unreserved keywords as their own Kind, allow them as identifiers via `can_be_identifier()`" — matches the Go/rustc pattern. It is the simplest of three established strategies (the alternatives being rust-analyzer's `contextual_kind` dual-token approach and TypeScript's context-flag approach). Since ABL's disambiguation needs are simple (1-token lookahead suffices), the Go-style approach is correct here.

**References:**
- [Roslyn Parser Design](https://github.com/dotnet/roslyn/blob/main/docs/compilers/Design/Parser.md) — C# `var` uses identical 1-token lookahead
- [rust-analyzer parser.rs](https://github.com/rust-lang/rust-analyzer/blob/master/crates/parser/src/parser.rs) — `at_contextual_kw` / `bump_remap` pattern
- [How to parse contextual keywords - Waleed Khan](https://blog.waleedkhan.name/parsing-contextual-keywords/)

## Technical Considerations

### Part A: Data Type Kind Variants

#### New keywords to add

15 data type keywords need Kind variants (RECID and ROWID already exist as reserved):

| Keyword | Reserved | Abbreviation | Kind Variant | Notes |
|---------|----------|-------------|-------------|-------|
| INTEGER | No | INT | `Kind::Integer` | Distinct from `Kind::IntegerLiteral` (literal values) |
| INT64 | No | (none) | `Kind::Int64` | |
| DECIMAL | No | DEC | `Kind::Decimal` | Distinct from `Kind::DecimalLiteral` (literal values) |
| CHARACTER | No | CHAR | `Kind::Character` | |
| LOGICAL | No | LOG | `Kind::Logical` | LOG confirmed as valid ABL abbreviation |
| DATE | No | (none) | `Kind::Date` | |
| DATETIME | No | (none) | `Kind::Datetime` | Not in HTML index; add manually |
| DATETIME-TZ | No | (none) | `Kind::DatetimeTz` | Not in HTML index; add manually |
| HANDLE | No | (none) | `Kind::Handle` | |
| RAW | No | (none) | `Kind::Raw` | |
| MEMPTR | No | (none) | `Kind::Memptr` | Not in HTML index; add manually |
| LONGCHAR | No | (none) | `Kind::Longchar` | Not in HTML index; add manually |
| CLOB | No | (none) | `Kind::Clob` | Not in HTML index; add manually |
| BLOB | No | (none) | `Kind::Blob` | Not in HTML index; add manually |
| COM-HANDLE | No | (none) | `Kind::ComHandle` | |

Already exist: `Kind::Recid` (reserved), `Kind::Rowid` (reserved)

#### `parse_data_type()` rewrite

Replace the entire string-comparison function with Kind-based dispatch:

```rust
fn parse_data_type(&mut self) -> ParseResult<DataType> {
    let token = self.peek();
    let data_type = match token.kind {
        Kind::Integer => DataType::Integer,
        Kind::Int64 => DataType::Int64,
        Kind::Decimal => DataType::Decimal,
        Kind::Character => DataType::Character,
        Kind::Logical => DataType::Logical,
        Kind::Date => DataType::Date,
        Kind::Datetime => DataType::DateTime,
        Kind::DatetimeTz => DataType::DateTimeTz,
        Kind::Handle => DataType::Handle,
        Kind::Rowid => DataType::Rowid,
        Kind::Recid => DataType::Recid,
        Kind::Raw => DataType::Raw,
        Kind::Memptr => DataType::Memptr,
        Kind::Longchar => DataType::Longchar,
        Kind::Clob => DataType::Clob,
        Kind::Blob => DataType::Blob,
        Kind::ComHandle => DataType::Com,
        _ => {
            return Err(ParseError {
                message: format!(
                    "Unknown data type: {}",
                    &self.source[token.start..token.end]
                ),
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            });
        }
    };
    self.advance();
    Ok(data_type)
}
```

This eliminates the `to_uppercase()` heap allocation per call — a direct performance improvement.

#### `is_data_type_kind()` predicate

Add a helper for the VAR statement disambiguation:

```rust
fn is_data_type_kind(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Integer
            | Kind::Int64
            | Kind::Decimal
            | Kind::Character
            | Kind::Logical
            | Kind::Date
            | Kind::Datetime
            | Kind::DatetimeTz
            | Kind::Handle
            | Kind::Rowid
            | Kind::Recid
            | Kind::Raw
            | Kind::Memptr
            | Kind::Longchar
            | Kind::Clob
            | Kind::Blob
            | Kind::ComHandle
    )
}
```

#### `can_be_identifier()` — data types

All unreserved data type keywords must be added to `can_be_identifier()` since ABL allows them as identifier names. RECID and ROWID are reserved, so they do NOT need to be added (they cannot be used as identifiers).

### Part B: Statement Keyword Migration

#### Disambiguation: Keyword-as-statement vs keyword-as-identifier

After migration, the lexer will emit `Kind::Variable` for "var", "vari", ..., "variable" and `Kind::Function` for "function". Currently the parser's text check at line 138 only matches `"var"` exactly, so `variable = 5.` falls through to assignment parsing. Post-migration, `Kind::Variable` fires for ALL abbreviation lengths, meaning `variable = 5.` would incorrectly dispatch to `parse_var_statement()`.

**Solution: Positive lookahead using `is_data_type_kind()`.**

Now that data types have Kind variants, we can use the architecturally clean positive check — if the next token is a data type keyword, it's a VAR statement:

```rust
// In parse_statement():
if self.check(Kind::Variable) && Self::is_data_type_kind(self.peek_at(1).kind) {
    return self.parse_var_statement();
}
// Fall through: "variable = 5." or "variable + 5." treated as expression
```

**Edge case analysis:**
- `VAR INTEGER x.` — next is `Kind::Integer` (data type). Dispatches correctly.
- `variable = 5.` — next is `Kind::Equals`. Falls through to assignment. Correct.
- `var = 5.` — next is `Kind::Equals`. Falls through. Correct.
- `variable + 5.` — next is `Kind::Plus`. Falls through to expression parsing. Correct.
- `VAR ClassName x.` — next is `Kind::Identifier` (not a data type Kind). Falls through. `parse_data_type()` would reject it anyway since class types aren't supported yet. When class support is added, extend the lookahead to also accept `Kind::Identifier`.

For `Kind::Function`: peek for a non-`=` token. A FUNCTION definition always has `FUNCTION <name> RETURNS ...`. An assignment would be `function = 5.`

```rust
if self.check(Kind::Function) && !self.check_at(1, Kind::Equals) {
    return self.parse_function();
}
// Fall through to expression/assignment
```

For `Kind::Catch` and `Kind::Finally`: **NOT dispatched in `parse_statement()` at all.** They are block-body-only constructs handled exclusively in `parse_block_body()`. Do NOT add them to `can_start_statement()`.

### `check_at()` — new minimal lookahead primitive

Add a helper for cases where we need a simple kind check at an offset:

```rust
/// Check if the token at `current + offset` has the given kind.
/// Safe because the token slice always ends with Kind::Eof.
fn check_at(&self, offset: usize, kind: Kind) -> bool {
    self.tokens.get(self.current + offset)
        .is_some_and(|t| t.kind == kind)
}
```

Also add `peek_at()` for cases where we need the full token (used in `is_data_type_kind` lookahead):

```rust
/// Peek at the token `offset` positions ahead of current.
/// Safe because the token slice always ends with Kind::Eof.
fn peek_at(&self, offset: usize) -> &Token {
    &self.tokens[self.current + offset]
}
```

Refactor existing raw `self.tokens.get(self.current + 1)` usages in `parse_catch_block()` and `parse_procedure_name()` to use these helpers.

### `can_be_identifier()` — full update

All unreserved keywords must be added. Reserved keywords (RECID, ROWID) are NOT added since they cannot be used as identifiers in ABL.

```rust
fn can_be_identifier(kind: Kind) -> bool {
    is_callable_kind(kind)
        || matches!(
            kind,
            // Existing
            Kind::Buffer | Kind::TempTable | Kind::Initial | Kind::Extent
                | Kind::Primary | Kind::Validate | Kind::BeforeTable
                | Kind::WordIndex | Kind::Preselect | Kind::Format
                | Kind::Label | Kind::ColumnLabel | Kind::Ascending
                | Kind::Descending | Kind::Shared | Kind::Global
            // Statement keywords (unreserved)
                | Kind::Variable | Kind::Function | Kind::Catch | Kind::Finally
            // Data type keywords (unreserved)
                | Kind::Integer | Kind::Int64 | Kind::Decimal | Kind::Character
                | Kind::Logical | Kind::Date | Kind::Datetime | Kind::DatetimeTz
                | Kind::Handle | Kind::Raw | Kind::Memptr | Kind::Longchar
                | Kind::Clob | Kind::Blob | Kind::ComHandle
        )
}
```

### `parse_define_statement()` restructuring

Replace `can_be_identifier()` gate + text check (lines 185-208) with direct `Kind::Variable` check:

```rust
if self.check(Kind::Variable) {
    self.advance();
} else {
    return Err(...);
}
```

**Cursor contract:** `parse_var_statement()` and `parse_function()` both expect the cursor ON their keyword token and will consume it via `self.advance()`. The dispatcher (`parse_statement()`) does NOT advance before calling them.

### `callable.rs` safety

The codegen only includes `keyword_type = "Function"` in `is_callable_kind()` (`codegen/main.rs:819-822`). Adding keywords as `keyword_type = "Statement"` or `keyword_type = "DataType"` ensures they are NOT callable. Verify after regeneration.

**Note on FUNCTION naming:** The ABL keyword `FUNCTION` (which defines functions) gets `keyword_type = "Statement"`, while built-in functions like TRIM/NOW get `keyword_type = "Function"`. This is correct but counter-intuitive. Add a comment in `keyword_overrides.toml` explaining this.

### `END FUNCTION` / `END CATCH` / `END FINALLY` closures

The END-keyword checks currently use `is_identifier_text()` or `eq_ignore_ascii_case()`. Update to Kind-based checks:
- `statements.rs:1425` — `END FUNCTION` closure: check `Kind::Function`
- `statements.rs:1513` — `END CATCH`: check `Kind::Catch`
- `statements.rs:1539` — `END FINALLY`: check `Kind::Finally`

### Opportunistic cleanup: duplicate Run dispatch

`parse_statement()` has duplicate `Kind::Run` checks at lines 99-101 and 109-111. The second is dead code. Remove it.

### Explicitly out of scope

- **File extension checks** (`statements.rs:1592-1598`): These check "p", "w", "r", "i", "cls" for RUN statement filename parsing. This is legitimate text inspection, not a keyword workaround.
- **FUNCTION body using `parse_block_body()`**: Currently FUNCTION bodies use inline `parse_statement()` loops and don't support CATCH/FINALLY. Defer to separate issue. Note: once `Kind::Catch` and `Kind::Finally` exist, this becomes a simple change.
- **`Keyfunction`/`KeyFunction` naming inconsistency**: Pre-existing codegen issue in `kind.rs`. Separate concern.
- **Class type support in VAR**: `VAR ClassName x.` is not yet parseable. When added, extend the VAR lookahead to also accept `Kind::Identifier`.

## Acceptance Criteria

### Lexer / Codegen
- [x] 15 data type keywords added to `keyword_overrides.toml` with correct abbreviations
- [x] VARIABLE (with VAR abbreviation), FUNCTION, CATCH, FINALLY added to `keyword_overrides.toml`
- [x] `cargo run -p oxabl_codegen` regenerates `kind.rs`, `build.rs`, `callable.rs` without errors
- [x] None of the new keywords appear in generated `is_callable_kind()` (verify in `callable.rs`)

### Parser
- [x] `check_at(offset, kind)` and `peek_at(offset)` helpers added to Parser
- [x] `is_data_type_kind()` predicate added
- [x] `parse_data_type()` rewritten to use Kind-based dispatch (no string comparison)
- [x] All unreserved keywords added to `can_be_identifier()` in `parser/mod.rs`
- [x] `parse_statement()` uses `Kind::Variable` with `is_data_type_kind()` positive lookahead
- [x] `parse_statement()` uses `Kind::Function` with `!check_at(1, Kind::Equals)` disambiguation
- [x] `parse_define_statement()` uses `Kind::Variable` directly (remove `can_be_identifier` gate + text check)
- [x] `parse_block_body()` uses `Kind::Catch` and `Kind::Finally` instead of `is_identifier_text()`
- [x] `END FUNCTION/CATCH/FINALLY` closures use Kind-based checks
- [x] `Kind::Variable` and `Kind::Function` added to `can_start_statement()`
- [x] `Kind::Catch` and `Kind::Finally` NOT added to `can_start_statement()` or `parse_statement()` dispatch
- [x] `is_identifier_text()` helper deleted (no remaining callers)
- [x] Duplicate `Kind::Run` dispatch removed (line 109-111)
- [x] Existing `self.tokens.get(self.current + 1)` usages refactored to use `check_at()`/`peek_at()`

### Quality
- [x] All existing parser tests pass (`cargo test -p oxabl_parser`)
- [x] All existing lexer tests pass (`cargo test -p oxabl_lexer`)
- [x] New tests for identifier-name collision cases
- [x] New tests for data type Kind matching
- [x] `cargo fmt --check` passes
- [x] `cargo clippy -D warnings` passes

## Test Cases

### New tests required

Use multiline `r#"..."#` strings for block constructs to match existing test style.

```rust
// Keyword-as-identifier edge cases (variable name position):
"DEFINE VARIABLE variable AS INTEGER."  // variable named "variable"
"DEFINE VARIABLE function AS INTEGER."  // variable named "function"
"DEFINE VARIABLE catch AS INTEGER."     // variable named "catch"
"DEFINE VARIABLE var AS INTEGER."       // variable named "var"
"DEFINE VARIABLE integer AS INTEGER."   // variable named "integer" (data type as identifier)
"DEFINE VARIABLE date AS DATE."         // variable named "date" (data type as identifier)

// Assignment disambiguation (keyword falls through to assignment):
"variable = 5."   // should parse as assignment, not VAR statement
"function = 5."   // should parse as assignment, not FUNCTION definition

// VAR with data type positive lookahead:
"VAR INTEGER x."           // standalone VAR statement
"VAR CHARACTER name."      // standalone VAR with different type
"VAR LOGICAL flag."        // standalone VAR with LOGICAL
"VAR INT x."               // abbreviation for INTEGER
"VAR DEC x."               // abbreviation for DECIMAL
"VAR CHAR x."              // abbreviation for CHARACTER
"VAR LOG x."               // abbreviation for LOGICAL

// Traditional DEFINE VARIABLE:
"DEFINE VARIABLE x AS INTEGER."
"DEFINE VAR x AS INTEGER."       // abbreviation

// Function definition:
r#"FUNCTION foo RETURNS INTEGER:
    RETURN 1.
END FUNCTION."#

// Block constructs with CATCH/FINALLY (multiline format):
r#"DO:
    MESSAGE "hello".
    CATCH e AS Progress.Lang.Error:
        MESSAGE "error".
    END CATCH.
END."#

r#"DO:
    MESSAGE "hello".
    FINALLY:
        MESSAGE "cleanup".
    END FINALLY.
END."#
```

### Test naming convention

Follow existing `parse_<construct>_<description>` pattern:
- `parse_var_keyword_as_variable_name`
- `parse_function_keyword_as_variable_name`
- `parse_data_type_keyword_as_variable_name`
- `parse_variable_keyword_assignment_disambiguation`
- `parse_function_keyword_assignment_disambiguation`
- `parse_var_with_abbreviated_data_types`

### Existing tests to verify

All 91+ existing parser tests must continue to pass unchanged. The migration should be transparent to correctly-written ABL code.

### Performance insight

The migration replaces O(n) string comparisons + `to_uppercase()` heap allocations with O(1) integer comparisons. This affects every data type parse (frequent) and every statement dispatch involving these keywords. Net performance improvement.

## MVP Implementation Steps

### Step 1: Add data type keywords to `keyword_overrides.toml`

```toml
# resources/keyword_overrides.toml

# =============================================================================
# DATA TYPE KEYWORDS
# ABL data types need their own Kind variants for proper parser dispatch.
# These are unreserved and use string comparison in parse_data_type() today.
# RECID and ROWID are already in the reserved keywords file.
# =============================================================================

[[add]]
name = "INTEGER"
keyword_type = "DataType"
min_abbreviation = "INT"

[[add]]
name = "INT64"
keyword_type = "DataType"

[[add]]
name = "DECIMAL"
keyword_type = "DataType"
min_abbreviation = "DEC"

[[add]]
name = "CHARACTER"
keyword_type = "DataType"
min_abbreviation = "CHAR"

[[add]]
name = "LOGICAL"
keyword_type = "DataType"
min_abbreviation = "LOG"

[[add]]
name = "DATE"
keyword_type = "DataType"

[[add]]
name = "DATETIME"
keyword_type = "DataType"

[[add]]
name = "DATETIME-TZ"
keyword_type = "DataType"

[[add]]
name = "HANDLE"
keyword_type = "DataType"

[[add]]
name = "RAW"
keyword_type = "DataType"

[[add]]
name = "MEMPTR"
keyword_type = "DataType"

[[add]]
name = "LONGCHAR"
keyword_type = "DataType"

[[add]]
name = "CLOB"
keyword_type = "DataType"

[[add]]
name = "BLOB"
keyword_type = "DataType"

[[add]]
name = "COM-HANDLE"
keyword_type = "DataType"
```

**Note on `keyword_type = "DataType"`:** The codegen's `KeywordType` enum may not have a `DataType` variant yet. If not, use `"Option"` (which keeps them out of `is_callable_kind()`), OR add a `DataType` variant to the codegen. Using a dedicated type is cleaner for the `is_data_type_kind()` predicate. **Check the codegen's KeywordType enum and decide during implementation.**

### Step 2: Add statement keywords to `keyword_overrides.toml`

```toml
# =============================================================================
# STATEMENT KEYWORDS
# Unreserved keywords needed for parser statement dispatch.
# =============================================================================

# VARIABLE is unreserved; VAR is its minimum abbreviation.
[[add]]
name = "VARIABLE"
keyword_type = "Statement"
min_abbreviation = "VAR"

# FUNCTION is unreserved; no abbreviation.
# Classified as Statement, not Function — it is the statement that *defines* functions,
# not a callable built-in. See callable.rs for the distinction.
[[add]]
name = "FUNCTION"
keyword_type = "Statement"

# CATCH is unreserved; no abbreviation
[[add]]
name = "CATCH"
keyword_type = "Statement"

# FINALLY is unreserved; no abbreviation
[[add]]
name = "FINALLY"
keyword_type = "Statement"
```

### Step 3: Regenerate lexer files

```bash
cargo run -p oxabl_codegen
```

Verify:
- `kind.rs` contains all new Kind variants
- `callable.rs` does NOT contain any of the new keywords
- `build.rs` includes atom entries for all abbreviation forms

### Step 4: Add helpers to Parser in `mod.rs`

```rust
fn check_at(&self, offset: usize, kind: Kind) -> bool {
    self.tokens.get(self.current + offset)
        .is_some_and(|t| t.kind == kind)
}

fn peek_at(&self, offset: usize) -> &Token {
    &self.tokens[self.current + offset]
}

fn is_data_type_kind(kind: Kind) -> bool {
    matches!(kind,
        Kind::Integer | Kind::Int64 | Kind::Decimal | Kind::Character
            | Kind::Logical | Kind::Date | Kind::Datetime | Kind::DatetimeTz
            | Kind::Handle | Kind::Rowid | Kind::Recid | Kind::Raw
            | Kind::Memptr | Kind::Longchar | Kind::Clob | Kind::Blob
            | Kind::ComHandle
    )
}
```

### Step 5: Update `can_be_identifier()` in `parser/mod.rs`

Add all unreserved keywords (statement + data type). Do NOT add RECID or ROWID (reserved).

### Step 6: Update `can_start_statement()` in `statements.rs`

Add `Kind::Variable` and `Kind::Function`. Do NOT add CATCH, FINALLY, or data types.

### Step 7: Update `parse_statement()` dispatch with lookahead

```rust
// VAR statement: positive lookahead for data type
if self.check(Kind::Variable) && Self::is_data_type_kind(self.peek_at(1).kind) {
    return self.parse_var_statement();
}

// FUNCTION definition: negative lookahead (not assignment)
if self.check(Kind::Function) && !self.check_at(1, Kind::Equals) {
    return self.parse_function();
}
```

Also remove the duplicate `Kind::Run` dispatch (lines 109-111).

### Step 8: Update `parse_define_statement()`

Replace `can_be_identifier()` gate + text check with direct `Kind::Variable` check.

### Step 9: Rewrite `parse_data_type()`

Replace string-comparison function with Kind-based dispatch (see code in Part A above).

### Step 10: Update `parse_block_body()`

Replace `is_identifier_text("catch")` and `is_identifier_text("finally")` with `self.check(Kind::Catch)` and `self.check(Kind::Finally)`.

### Step 11: Update END closures

- `parse_function()` END FUNCTION closure: `Kind::Function`
- `parse_catch_block()` END CATCH: `Kind::Catch`
- `parse_finally_block()` END FINALLY: `Kind::Finally`

Refactor existing raw `self.tokens.get(self.current + 1)` usages to use `check_at()`/`peek_at()`.

### Step 12: Delete `is_identifier_text()` helper

Remove the method at `statements.rs:1466-1471`. Verify no remaining callers.

### Step 13: Add new tests and run full suite

```bash
cargo test
cargo fmt --check
cargo clippy -D warnings
```

## Sources & References

- **Origin brainstorm:** [docs/brainstorms/2026-04-04-migrate-parser-keyword-workarounds-brainstorm.md](docs/brainstorms/2026-04-04-migrate-parser-keyword-workarounds-brainstorm.md) — Key decisions: single Kind::Variable with VAR abbreviation, FUNCTION/CATCH/FINALLY as exact matches, follow established migration pattern
- Prior migration precedent: commit `f6f1853` (TEMP-TABLE/BUFFER/INITIAL/EXTENT migration)
- ABL keyword reference: `resources/abl_keyword_index.html` (confirms keyword reserved status and abbreviations)
- Codegen pipeline: `crates/oxabl_codegen/src/main.rs`
- Parser dispatch: `crates/oxabl_parser/src/parser/statements.rs:42-160`
- `can_be_identifier()`: `crates/oxabl_parser/src/parser/mod.rs:148-169`
- `parse_data_type()`: `crates/oxabl_parser/src/parser/mod.rs:229-264` — current string-comparison implementation
- [Roslyn Parser Design](https://github.com/dotnet/roslyn/blob/main/docs/compilers/Design/Parser.md) — C# `var` disambiguation precedent
- [How to parse contextual keywords - Waleed Khan](https://blog.waleedkhan.name/parsing-contextual-keywords/)
- [rust-analyzer parser.rs](https://github.com/rust-lang/rust-analyzer/blob/master/crates/parser/src/parser.rs) — contextual keyword handling
