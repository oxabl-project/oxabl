---
title: "feat: Add database ops, preprocessor statements, and stream/frame parsing"
type: feat
status: active
date: 2026-04-04
origin: docs/brainstorms/2026-04-04-parser-gaps-brainstorm.md
deepened: 2026-04-04
---

# feat: Add database ops, preprocessor statements, and stream/frame parsing

## Enhancement Summary

**Deepened on:** 2026-04-04
**Sections enhanced:** All 3 phases + system-wide impact
**Agents used:** Architecture Strategist, Code Simplicity Reviewer, Pattern Recognition Specialist, Performance Oracle, Best Practices Researcher

### Key Improvements

1. **`PreprocIf<T>` generic struct** -- Instead of duplicating preprocessor fields across Statement/Expression/DataType enums, use a shared generic. Reduces drift, enables a single `parse_preproc_if<T>()` with closure parameter. Consensus across all 4 reviewers.
2. **`Kind::PreprocEnd` synthetic token** -- Replace the proposed `PreprocValue` raw-text token with a `PreprocEnd` newline marker (GCC's pattern). The lexer sets `in_directive` flag and emits `PreprocEnd` on newline. This preserves internal token structure within define values and avoids String allocation.
3. **Expression/DataType `PreprocIf` else branches are non-optional** -- Mid-expression `&IF` without `&ELSE` is syntactically invalid in ABL. Only Statement-level keeps `Option<>` for the else branch.
4. **Shared helpers** -- Commit to `parse_stream_io()` (one function with direction parameter) and extract `parse_assign_pairs()` for reuse between `parse_assign_statement()` and BUFFER-COPY.

### New Considerations Discovered

- **Straddling directives**: `&IF` can split the middle of a statement across branches. Detect and preserve as raw text; do not attempt to parse.
- **Preprocessor error recovery**: Needs a depth counter and `&ENDIF`/`&ELSE`/`&ELSEIF` as synchronization points when inside preprocessor context.
- **DEFINE FRAME should preserve `raw_span: Span`** for formatter round-tripping of unparsed content.
- **`Create` should include `no_error: bool`** for consistency with DELETE/RELEASE/VALIDATE (ABL supports `CREATE ... NO-ERROR`).

---

## Overview

Add three major parser feature areas to produce formatter/linter-ready AST nodes, delivered as 3 separate PRs:

1. **PR 1: Database manipulation** -- CREATE, DELETE, RELEASE, VALIDATE, BUFFER-COPY, BUFFER-COMPARE
2. **PR 2: Preprocessor statements** -- &IF/&DEFINE/&UNDEFINE/&MESSAGE as AST nodes at both statement and expression levels
3. **PR 3: Streams and frames** -- DEFINE STREAM/FRAME, INPUT/OUTPUT/CLOSE, named stream references

(see brainstorm: docs/brainstorms/2026-04-04-parser-gaps-brainstorm.md)

## Problem Statement

The parser cannot handle database record manipulation, preprocessor directives, or stream/frame I/O -- three of the most common constructs in production ABL codebases. Without these, the tooling suite cannot parse real-world files end-to-end.

## Proposed Solution

Follow the established parser pattern: add Kind variants via codegen, create AST node definitions, implement `parse_*()` functions with keyword dispatch, update `can_start_statement()`, and write comprehensive tests. Each PR is self-contained and ships independently.

---

## Phase 1: Database Manipulation (PR 1)

### Prerequisite: Lexer Keywords

Add to `resources/keyword_overrides.toml`:

```toml
[[add]]
name = "BUFFER-COPY"
keyword_type = "Statement"

[[add]]
name = "BUFFER-COMPARE"
keyword_type = "Statement"
```

Then regenerate: `cargo run -p oxabl_codegen -- kind && cargo run -p oxabl_codegen -- atoms`

Verify `Kind::To` already exists (used by FOR EACH / DO loops). `Kind::NoError` already exists.

### AST Nodes

Add to `crates/oxabl_ast/src/statement.rs`:

```rust
/// CREATE buffer-name [NO-ERROR].
Create {
    buffer: Identifier,
    no_error: bool,
},

/// DELETE buffer-name [NO-ERROR].
Delete {
    buffer: Identifier,
    no_error: bool,
},

/// RELEASE buffer-name [NO-ERROR].
Release {
    buffer: Identifier,
    no_error: bool,
},

/// VALIDATE buffer-name [NO-ERROR].
Validate {
    buffer: Identifier,
    no_error: bool,
},

/// BUFFER-COPY source TO target [ASSIGN field = expr ...] [NO-ERROR].
BufferCopy {
    source: Identifier,
    target: Identifier,
    assignments: Vec<(Identifier, Expression)>,
    no_error: bool,
},

/// BUFFER-COMPARE source TO target [SAVE RESULT IN lvar] [NO-ERROR].
BufferCompare {
    source: Identifier,
    target: Identifier,
    result_var: Option<Identifier>,
    no_error: bool,
},
```

### Parser Functions

In `crates/oxabl_parser/src/parser/statements.rs`:

- `parse_create_statement()` -- advance, parse identifier, optional NO-ERROR, expect period
- `parse_delete_statement()` -- advance, parse identifier, optional NO-ERROR, expect period
- `parse_release_statement()` -- advance, parse identifier, optional NO-ERROR, expect period
- `parse_validate_statement()` -- advance, parse identifier, optional NO-ERROR, expect period
- `parse_buffer_copy()` -- advance, parse source, expect TO, parse target, optional ASSIGN pairs via shared `parse_assign_pairs()` helper, optional NO-ERROR, expect period
- `parse_buffer_compare()` -- advance, parse source, expect TO, parse target, optional SAVE RESULT IN with 3-token lookahead (parse `SAVE` `RESULT` `IN` as identifiers, not Kind variants), optional NO-ERROR, expect period

Extract a shared `parse_assign_pairs() -> ParseResult<Vec<(Identifier, Expression)>>` helper that both `parse_assign_statement()` and BUFFER-COPY's ASSIGN clause can call. This avoids duplicating the `target = value` loop logic and follows the codebase pattern of shared helpers (`parse_lock_type()`, `parse_access_modifier()`, `parse_block_body()`).

Add dispatch in `parse_statement()` and update `can_start_statement()` with: `Kind::Create`, `Kind::Delete`, `Kind::Release`, `Kind::Validate`, `Kind::BufferCopy`, `Kind::BufferCompare`.

### Disambiguation: CREATE/DELETE Multi-Form

ABL has `CREATE SERVER`, `CREATE WIDGET-POOL`, `DELETE OBJECT`, `DELETE PROCEDURE`, etc. **These are out of scope for PR 1.** The parser should parse `CREATE <identifier>.` generically -- the identifier could be a buffer name, server handle, etc. Semantic validation (is this a valid buffer?) is a separate concern. If the next token after CREATE is a keyword like SERVER or WIDGET-POOL, parse it as the identifier/target anyway. The AST just captures the syntax.

Similarly, `DELETE OBJECT hWidget.` should parse as `Delete { buffer: Identifier("OBJECT"), ... }` for now -- the semantic layer can distinguish record deletes from object deletes later.

### Tests (~15-20 tests)

- Basic CREATE, DELETE, RELEASE, VALIDATE
- DELETE/RELEASE/VALIDATE with NO-ERROR
- BUFFER-COPY basic, with ASSIGN, with multiple ASSIGN pairs, with NO-ERROR
- BUFFER-COMPARE basic, with SAVE RESULT IN, with NO-ERROR
- Combined options (e.g., BUFFER-COPY with ASSIGN and NO-ERROR)
- Case insensitivity (lowercase/mixed case)
- Error cases: missing period, missing buffer name, missing TO in BUFFER-COPY

### Acceptance Criteria

- [x] `Kind::BufferCopy` and `Kind::BufferCompare` added via codegen
- [x] 6 new `Statement` variants in AST
- [x] 6 `parse_*()` functions in `statements.rs`
- [x] `can_start_statement()` updated
- [x] 15+ tests covering all statement forms and options (19 tests)
- [x] `cargo test`, `cargo clippy -D warnings`, `cargo fmt --check` pass
- [x] CLAUDE.md "Not yet implemented" list updated

---

## Phase 2: Preprocessor Statements (PR 2)

### Research Insights: How Other Tools Handle Preprocessor Directives

Research into clang, Roslyn, rust-analyzer, and tree-sitter revealed three established patterns:
- **Clang**: Preprocessor runs as a separate phase; directives are invisible in the AST. Unsuitable for formatting.
- **Roslyn (C#)**: Directives are "structured trivia" attached to tokens. Gold standard for round-tripping, but requires trivia infrastructure oxabl doesn't have.
- **tree-sitter-c**: Directives are real grammar rules duplicated per context (`preprocIf` in statements, in field lists, etc.). Acknowledges mid-expression `#if` produces ERROR nodes.
- **rust-analyzer/rowan**: Lossless CST where everything is a node. Ideal long-term but a major architectural shift.

**Chosen approach**: Pattern A (AST nodes) from tree-sitter, enhanced with a generic struct. Migrate to lossless CST (Pattern C) when the formatter is mature.

### Critical Design Decision 1: `PreprocIf<T>` Generic Struct

**Problem:** Placing `PreprocIf` fields in 3 separate enums (Statement, Expression, DataType) breaks the clean type separation that the codebase maintains, creates a maintenance multiplier for all future AST visitors, and risks drift between representations.

**Solution:** Define a shared generic struct in `crates/oxabl_ast/src/statement.rs`:

```rust
/// Preprocessor conditional block, generic over the content type.
/// Used as Statement::PreprocIf(PreprocIf<Vec<Statement>>),
///         Expression::PreprocIf(Box<PreprocIf<Expression>>),
///         DataType::PreprocIf(Box<PreprocIf<DataType>>).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocIf<T> {
    pub condition: Expression,
    pub then_branch: T,
    pub elseif_branches: Vec<(Expression, T)>,
    pub else_branch: Option<T>,
}
```

This enables a single generic parser function (see Parser Functions below).

### Critical Design Decision 2: `Kind::PreprocEnd` Synthetic Token

**Problem:** `&SCOPED-DEFINE name value` values extend to end-of-line, but the lexer strips newlines as whitespace. The parser has no way to detect where the value ends.

**Solution (GCC pattern):** Add a `Kind::PreprocEnd` synthetic token. When the lexer recognizes `&SCOPED-DEFINE` or `&GLOBAL-DEFINE`, it sets an `in_directive: bool` flag. While the flag is set, the lexer does NOT skip newlines -- instead it emits `Kind::PreprocEnd` on newline (and on EOF for directives on the last line). This preserves the internal token structure of define values (the parser sees individual tokens like `Kind::Plus`, `Kind::IntegerLiteral`, etc.) while clearly delimiting where the value ends.

Add `PreprocEnd` directly to the `kind.rs` codegen template (it is not a keyword match target).

### Lexer Changes

In `crates/oxabl_lexer/src/lib.rs`:
- Add `in_directive: bool` field to the Lexer struct (default `false`)
- In `read_preprocessor_directive()`: after matching `PreprocScopedDefine` or `PreprocGlobalDefine`, set `self.in_directive = true`
- In `skip_whitespace()`: when `in_directive` is true and a `\n` is encountered, emit `Kind::PreprocEnd`, clear the flag, and return instead of consuming the newline
- Edge case: if EOF is reached while `in_directive` is true, emit `Kind::PreprocEnd` before `Kind::Eof`

**Performance note (from Performance Oracle):** This adds one boolean check per whitespace-skip call, which is negligible. The `in_directive` flag is only set for `&DEFINE` lines (~0-20 per file vs thousands of tokens). No String allocation needed -- the parser extracts define values from source using token byte offsets.

This is a lexer-level change, so it needs its own tests in the lexer crate.

### AST Nodes

Add to `crates/oxabl_ast/src/statement.rs`:

```rust
/// Preprocessor conditional (generic -- see PreprocIf<T> struct above)
/// Statement level: &IF cond &THEN stmts [&ELSEIF cond &THEN stmts]... [&ELSE stmts] &ENDIF
PreprocIf(PreprocIf<Vec<Statement>>),

/// &SCOPED-DEFINE name value / &GLOBAL-DEFINE name value
PreprocDefine {
    name: Identifier,
    value_span: Option<Span>,  // byte offsets into source, not a String allocation
    is_global: bool,
},

/// &UNDEFINE name
PreprocUndefine {
    name: Identifier,
},

/// &MESSAGE "text"
PreprocMessage {
    expression: Expression,
},
```

Add to `crates/oxabl_ast/src/expression.rs`:

```rust
/// Preprocessor variable reference: {&variable}
PreprocReference(String),

/// Mid-expression preprocessor conditional.
/// Note: else_expr is NOT optional -- mid-expression &IF without &ELSE is syntactically invalid.
PreprocIf(Box<PreprocIf<Expression>>),
```

Add to `crates/oxabl_ast/src/statement.rs` DataType enum:

```rust
/// Conditional data type: &IF DEFINED(x) &THEN INTEGER &ELSE CHARACTER &ENDIF
/// Note: else_type is NOT optional -- mid-type &IF without &ELSE is syntactically invalid.
PreprocIf(Box<PreprocIf<DataType>>),
```

**Design note:** At the Expression and DataType levels, the `else_branch` field on `PreprocIf<T>` is semantically required (an `&IF` without `&ELSE` leaves a syntactic hole). The parser should enforce this by emitting an error if `&ELSE` is missing at these levels. At the Statement level, `&IF ... &THEN ... &ENDIF` without `&ELSE` is valid (conditionally includes code).

### Parser Functions

**Generic preprocessor parser (in `crates/oxabl_parser/src/parser/statements.rs`):**

```rust
fn parse_preproc_if<T>(
    &mut self,
    parse_branch: impl Fn(&mut Self) -> ParseResult<T>,
) -> ParseResult<PreprocIf<T>> {
    // Shared logic: condition, &THEN, branch, elseif chain, else, &ENDIF
}
```

**Statement-level:**
- `parse_preproc_if_statement()` -- calls `parse_preproc_if(|p| p.parse_block_until_preproc_boundary())`
- `parse_preproc_define()` -- advance, expect identifier (name), collect tokens until `PreprocEnd` as value span, construct node with `is_global` flag
- `parse_preproc_undefine()` -- advance, expect identifier
- `parse_preproc_message()` -- advance, parse expression

**Expression-level (in `crates/oxabl_parser/src/parser/expressions.rs`):**
- In `parse_primary()`, handle `Kind::PreprocIf` -- calls `parse_preproc_if(|p| p.parse_expression())`, validates else_branch is `Some`, wraps in `Expression::PreprocIf`.
- In `parse_primary()`, handle `Kind::Preprop` -- return `Expression::PreprocReference`.

**Type-level:**
- In `parse_data_type()`, handle `Kind::PreprocIf` -- calls `parse_preproc_if(|p| p.parse_data_type())`, validates else_branch is `Some`, wraps in `DataType::PreprocIf`.

### &IF Condition Expressions

Reuse `parse_expression()` for preprocessor conditions. ABL preprocessor conditions use `DEFINED(name)`, comparison operators, and logical operators -- all already supported by the expression parser. `DEFINED` will parse as a function call, which is correct structurally.

### Straddling Directives

ABL allows `&IF` to split the middle of a statement:
```abl
&IF DEFINED(use-widget) &THEN
DEFINE VARIABLE x AS
&ELSE
DEFINE VARIABLE x AS
&ENDIF
    INTEGER NO-UNDO.
```

This is neither a statement-level nor expression-level `&IF`. **These should be detected and preserved as raw text** (opaque token spans) that the formatter reproduces verbatim without reformatting. Detection: check whether `&IF`/`&ENDIF` spans align with statement boundaries.

### Error Recovery

Preprocessor directives don't end with periods, so `synchronize()` cannot use its normal period-boundary recovery.

**Add preprocessor-aware recovery:**
- Track preprocessor nesting depth in the parser (a simple `preproc_depth: u32` counter, incremented on `&IF`, decremented on `&ENDIF`)
- When `preproc_depth > 0`, add `Kind::PreprocElse`, `Kind::PreprocElseif`, and `Kind::PreprocEndif` as synchronization points alongside periods and statement-starting keywords
- For `&DEFINE`/`&UNDEFINE`/`&MESSAGE`, recovery is handled by `Kind::PreprocEnd` (the lexer's newline boundary)
- **Depth limit:** Add a `MAX_PREPROC_DEPTH` constant (e.g., 64) to prevent stack overflow on pathological input with deeply nested `&IF` blocks

Update `can_start_statement()` with: `Kind::PreprocIf`, `Kind::PreprocScopedDefine`, `Kind::PreprocGlobalDefine`, `Kind::PreprocUndefine`, `Kind::PreprocMessage`.

### Tests (~25-30 tests)

**Lexer tests:**
- `&SCOPED-DEFINE` emits directive token, name tokens, value tokens, and `PreprocEnd`
- `&GLOBAL-DEFINE` same pattern
- Value with spaces, operators, special characters (tokens preserved individually)
- Define with no value (just name, then `PreprocEnd`)
- `in_directive` flag clears on newline and EOF

**Parser statement-level tests:**
- `&IF ... &THEN ... &ENDIF` (no else)
- `&IF ... &THEN ... &ELSE ... &ENDIF`
- `&IF ... &THEN ... &ELSEIF ... &THEN ... &ENDIF`
- `&IF ... &THEN ... &ELSEIF ... &THEN ... &ELSE ... &ENDIF`
- Nested `&IF` inside `&IF`
- `&SCOPED-DEFINE` with value (check value_span covers correct byte range)
- `&SCOPED-DEFINE` with no value
- `&GLOBAL-DEFINE`
- `&UNDEFINE`
- `&MESSAGE` with string literal

**Parser expression-level tests:**
- `{&variable}` as expression (PreprocReference)
- `&IF DEFINED(x) &THEN 1 &ELSE 2 &ENDIF` as expression
- Expression-level `&IF` without `&ELSE` produces parse error
- `DEFINE VARIABLE x AS &IF DEFINED(use-int) &THEN INTEGER &ELSE CHARACTER &ENDIF NO-UNDO.` (mid-statement data type)
- DataType-level `&IF` without `&ELSE` produces parse error

**Generic PreprocIf<T> tests:**
- Verify Statement, Expression, and DataType variants all produce structurally consistent `PreprocIf<T>` nodes

### Acceptance Criteria

- [x] `Kind::PreprocEnd` added to codegen template; `in_directive` flag in lexer
- [x] Lexer tests for new `PreprocEnd` tokenization behavior (6 tests)
- [x] `PreprocIf<T>` generic struct defined in oxabl_ast
- [x] 4 new `Statement` variants, 2 new `Expression` variants, 1 new `DataType` variant
- [x] Generic `parse_preproc_if<T>()` with closure parameter
- [x] Statement-level preprocessor parsing with full &IF/&ELSEIF/&ELSE chain
- [x] Expression-level `&IF` and `{&variable}` reference parsing (else required)
- [x] Data type-level `&IF` for conditional types in DEFINE VARIABLE (else required)
- [x] Preprocessor-aware error recovery with depth tracking
- [x] 30 tests covering all forms (6 lexer + 24 parser)
- [x] `cargo test`, `cargo clippy -D warnings`, `cargo fmt --check` pass

---

## Phase 3: Streams and Frames (PR 3)

### Prerequisite: Lexer Keywords

Verify existing Kind variants: `Kind::Stream`, `Kind::Frame`, `Kind::Input`, `Kind::Output`, `Kind::InputOutput`, `Kind::From`.

Add to `resources/keyword_overrides.toml` if missing:

```toml
[[add]]
name = "THROUGH"
keyword_type = "Option"
min_abbreviation = "THRU"

[[add]]
name = "APPEND"
keyword_type = "Option"

[[add]]
name = "CLOSE"
keyword_type = "Statement"
```

Regenerate via codegen.

### AST Nodes

Add to `crates/oxabl_ast/src/statement.rs`:

```rust
/// DEFINE STREAM stream-name.
DefineStream {
    name: Identifier,
},

/// DEFINE FRAME frame-name ... .
/// Simplified: captures name and raw span of unparsed content for formatter round-tripping.
DefineFrame {
    name: Identifier,
    raw_span: Span,  // byte offsets of everything between name and period
},

/// INPUT [STREAM stream-name] FROM file / THROUGH program / CLOSE.
/// OUTPUT [STREAM stream-name] TO file [APPEND] / THROUGH program / CLOSE.
/// INPUT-OUTPUT [STREAM stream-name] THROUGH program / CLOSE.
StreamIo {
    direction: StreamDirection,
    stream_name: Option<Identifier>,
    operation: StreamOperation,
},
```

New enums:

```rust
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum StreamDirection {
    Input,
    Output,
    InputOutput,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StreamOperation {
    From(Expression),
    To { target: Expression, append: bool },
    Through(Expression),
    Close,
}
```

Using a single `StreamIo` variant with enums instead of 6+ separate variants keeps the Statement enum from bloating and groups related semantics together.

### Parser Functions

**DEFINE sub-dispatch** (in `parse_define_statement()`, after BUFFER check):
- `parse_define_stream()` -- simple: advance past STREAM, parse identifier, expect period
- `parse_define_frame()` -- advance past FRAME, parse identifier, skip tokens until period (simplified). Uses a loop consuming tokens until `Kind::Period` without trying to parse frame phrase details.

**Stream I/O statements** (new dispatch entries in `parse_statement()`):

Use a single `parse_stream_io(direction: StreamDirection)` function dispatched from `Kind::Input`, `Kind::Output`, and `Kind::InputOutput`. The three directions share 90% of their logic:
1. Advance past direction keyword
2. Optional `STREAM stream-name` (check for `Kind::Stream`, then parse identifier)
3. Dispatch on operation: `FROM` / `TO` / `THROUGH` / `CLOSE`
4. Parse target (expression) for FROM/TO/THROUGH
5. Optional `APPEND` for OUTPUT TO
6. Expect period
7. Validate operation is valid for direction (INPUT doesn't support TO, OUTPUT doesn't support FROM, INPUT-OUTPUT only supports THROUGH/CLOSE)

### Disambiguation: INPUT/OUTPUT as Statement vs. Expression

`Kind::Input` at statement level means stream I/O. `Kind::Input` after DEFINE means parameter direction. `INPUT(param)` is a callable function.

Strategy: In `parse_statement()`, when `Kind::Input` or `Kind::Output` is seen, use 1-token lookahead:
- If next is `FROM`, `TO`, `THROUGH`, `CLOSE`, or `STREAM` -> stream I/O statement
- Otherwise -> fall through to expression parsing (handles `INPUT(param)` callable)

This matches the existing disambiguation pattern used elsewhere (e.g., `Kind::Function` with negative `=` lookahead).

### STREAM Clause on DISPLAY

Modify `parse_display_statement()` to check for `Kind::Stream` immediately after advancing past DISPLAY. If present, consume `STREAM identifier` before entering the item loop.

Update the `Display` statement variant in the AST to include `stream_name: Option<Identifier>`.

### DEFINE FRAME: Skipping Strategy

DEFINE FRAME has extremely complex syntax. The simplified approach:
1. Parse `DEFINE FRAME frame-name`
2. Record current byte offset as `raw_span.start`
3. Consume all tokens until `Kind::Period` (record offset as `raw_span.end`)
4. Store name and `raw_span` in the AST

The `raw_span` preserves the unparsed content so a formatter can reproduce it verbatim. This is honest about scope -- a formatter can identify frame definitions and round-trip their content. Full frame phrase parsing is a separate, large effort.

**Note (from Pattern Recognition review):** There is no existing precedent for skip-until-period in the codebase. Document this strategy clearly in the variant's doc comment. Consider counting brace/paren nesting within the skip to detect obvious mismatches and avoid swallowing the next statement.

### Tests (~20-25 tests)

- DEFINE STREAM basic
- DEFINE FRAME basic (name only, rest skipped)
- INPUT FROM string literal / identifier
- INPUT THROUGH
- INPUT CLOSE
- INPUT STREAM s1 FROM (named stream)
- OUTPUT TO basic
- OUTPUT TO with APPEND
- OUTPUT THROUGH
- OUTPUT CLOSE
- INPUT-OUTPUT THROUGH
- INPUT-OUTPUT CLOSE
- DISPLAY STREAM s1 (stream clause on display)
- Disambiguation: INPUT(param) as expression vs INPUT FROM as statement
- Case insensitivity
- Error cases

### Acceptance Criteria

- [x] `Kind::Through`, `Kind::Thru`, `Kind::Append`, `Kind::Close` added via codegen
- [x] `DefineStream`, `DefineFrame` (with `raw_span`), `StreamIo` variants in AST
- [x] `StreamDirection` (with `Copy`) and `StreamOperation` enums
- [x] DEFINE sub-dispatch updated for STREAM and FRAME
- [x] Single `parse_stream_io(direction)` function handling all 3 directions
- [x] Stream I/O statement parsing with named stream support
- [x] INPUT/OUTPUT disambiguation via lookahead
- [x] DISPLAY updated with optional STREAM clause
- [x] 25 tests covering all forms
- [x] `cargo test`, `cargo clippy -D warnings`, `cargo fmt --check` pass

---

## System-Wide Impact

### Interaction Graph

- Adding new Statement variants requires updating any downstream `match` on `Statement` (currently only in tests and parser internals -- no formatter/linter yet)
- Preprocessor `Expression` variants affect expression visitors (none exist yet beyond tests)
- New Kind variants from codegen update `kind.rs` and `build.rs` (atoms) -- these are generated files

### Error Propagation

- Parse errors in new statement forms propagate as `ParseError` and are caught by `parse_program()`'s `synchronize()` mechanism
- Preprocessor blocks need special recovery (advance to `&ENDIF` not just period)
- Lexer errors in preprocessor value tokenization should produce `Kind::Invalid` with a useful message

### State Lifecycle Risks

- None -- the parser is stateless (no database, no side effects). Each parse is independent.

### API Surface Parity

- `Statement` enum is the public API. New variants are additive (non-breaking for pattern matches with `_` wildcard, breaking for exhaustive matches)
- `Expression` and `DataType` enums similarly affected

---

## Sources & References

### Origin

- **Brainstorm document:** [docs/brainstorms/2026-04-04-parser-gaps-brainstorm.md](docs/brainstorms/2026-04-04-parser-gaps-brainstorm.md) -- Key decisions: 3 separate PRs, preprocessor as AST nodes at multiple levels, frames scoped to DEFINE + references, DB ops include full buffer operations.

### Internal References

- Parser dispatch pattern: `crates/oxabl_parser/src/parser/statements.rs:51`
- DEFINE sub-dispatch: `crates/oxabl_parser/src/parser/statements.rs:188`
- Expression AST: `crates/oxabl_ast/src/expression.rs:17`
- Statement AST: `crates/oxabl_ast/src/statement.rs:6`
- Lexer preprocessor handling: `crates/oxabl_lexer/src/lib.rs:461` (references), `lib.rs:478` (directives)
- Keyword overrides: `resources/keyword_overrides.toml`
- Prior art -- OO-ABL plan: `docs/plans/2026-04-04-002-feat-parser-next-phase-plan.md`
- Prior art -- keyword migration plan: `docs/plans/2026-04-04-003-refactor-migrate-parser-keyword-workarounds-plan.md`

### Institutional Learnings

- Heap allocation in `match_keyword()`: `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md` -- never `to_lowercase()` on hot paths
- Always add keywords via `keyword_overrides.toml` + codegen, never string comparisons
- Use `Identifier` (not `String`) for AST names that need source spans
- Disambiguation via 1-token lookahead using `check_at()`/`peek_at()`
- Use byte offsets (not String allocations) for raw-text captures -- parser extracts from source via token spans

### External References (from Deepen Research)

- [tree-sitter-c grammar.js](https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js) -- `preprocIf` helper pattern for context-specific preprocessor rules; `'\n'` as terminal in `preproc_def`
- [Roslyn: Structured Trivia for Preprocessor Directives](https://learn.microsoft.com/en-us/dotnet/csharp/roslyn-sdk/get-started/syntax-analysis) -- Gold standard for source-faithful round-tripping
- [GCC C Preprocessor Internals: Lexer](https://gcc.gnu.org/onlinedocs/gcc-3.2/cppinternals/Lexer.html) -- `CPP_EOF` synthetic end-of-directive token pattern (basis for `PreprocEnd`)
- [Lossless Syntax Trees (CAD97/rowan)](https://dev.to/cad97/lossless-syntax-trees-280c) -- Future migration path for lossless CST
- [Closing the gap between preprocessor and AST (LLVM)](https://discourse.llvm.org/t/closing-the-gap-between-the-preprocessor-and-the-ast/6254) -- Why full unification is impractical for C (but ABL is simpler)
