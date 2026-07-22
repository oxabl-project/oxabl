---
title: "feat: Add include file parsing support"
type: feat
status: active
date: 2026-04-10
---

# feat: Add Include File Parsing Support

## Overview

ABL (Progress OpenEdge) uses curly-brace syntax for include file references (`{file.i}`, `{file.i args}`, `{1}`, `{&var}`). The lexer already handles preprocessor variable references (`{&var}`) as single `Preprop` tokens, but bare include file references like `{mod/file.i}` currently cause a parser failure: the lexer emits `LeftBrace` and the parser has no dispatch for it, producing "Unexpected token LeftBrace".

This feature adds lexer and parser support for include file references as first-class constructs.

## Problem Statement / Motivation

Any real-world ABL codebase relies heavily on include files. Without this support, the parser cannot process the vast majority of production ABL code. Include files are used for:

- Shared variable/temp-table definitions (`{globals/globals.i}`)
- Code reuse via parameterized templates (`{file.i NEW}`, `{file.i &name="value"}`)
- Preprocessor argument substitution inside include files (`{1}`, `{2}`)
- Include guards and conditional compilation

## Proposed Solution

Extend the lexer to recognize include file references as single tokens (consistent with existing `{&var}` handling), add AST nodes, and add parser support.

### Scope

**In scope (MVP):**
- `{file.i}` -- simple include, no arguments
- `{path/to/file.i}` -- include with path separators
- `{file.i arg1 arg2}` -- include with positional arguments
- `{file.i &name=value}` -- include with named arguments
- `{file.i &n1=v1 &n2=v2}` -- include with multiple named arguments
- `{file.i "quoted arg"}` -- include with quoted string arguments
- `{1}`, `{2}`, `{0}` -- positional argument references (inside include files)
- Include references at statement position
- Include references at expression position (ABL text substitution can appear anywhere)

**Out of scope (future work):**
- `{{&variable}}` -- indirect includes (requires nested brace tracking)
- Actual include file resolution/expansion (file I/O, PROPATH lookup)
- Preprocessor directive parsing (`&IF`/`&ENDIF` blocks) in the parser
- Semantic analysis of include arguments

### Design Decisions

**D1: Lexer-level tokenization (not parser-level).** Consume everything between `{` and `}` as a single token, consistent with how `{&var}` is already handled. This avoids the parser needing to reconstruct include references from multiple tokens.

**D2: Raw content storage.** Store the raw text between braces (trimmed), not a parsed structure. Argument parsing can be done in a future preprocessing phase. This matches the `Preprop` token approach.

**D3: Separate token kinds for includes vs argument references.** `{file.i}` produces `Kind::IncludeReference`, while `{1}` produces `Kind::IncludeArgReference`. They have different semantics and downstream tooling needs to distinguish them.

**D4: Both Statement and Expression AST variants.** ABL includes are text substitution and can appear anywhere. The parser should accept them in both positions.

**D5: Manual Kind additions.** Add new Kind variants to the non-generated section of `kind.rs` to avoid conflicting with the codegen pipeline.

## Technical Approach

### Lexer Changes (`crates/oxabl_lexer/src/lib.rs`)

Modify the `{` match arm (line 215) with this disambiguation:

| Next char after `{` | Token produced | Method |
|---|---|---|
| `&` | `Kind::Preprop` (unchanged) | `read_preprocessor_reference()` |
| digit | `Kind::IncludeArgReference` | New: `read_include_arg_reference()` |
| alpha, `/`, `.`, `"` | `Kind::IncludeReference` | New: `read_include_reference()` |
| `{` | `Kind::LeftBrace` (unchanged) | Preserves current behavior for `{{&var}}` |
| other | `Kind::LeftBrace` (unchanged) | Fallback |

New methods:
- `read_include_reference(start)` -- consume characters until `}`, store trimmed raw content as `TokenValue::String`
- `read_include_arg_reference(start)` -- consume digits until `}`, store as `TokenValue::Integer`

### Kind Changes (`crates/oxabl_lexer/src/kind.rs`)

Add two new variants in a manual section below the generated code:
- `Kind::IncludeReference` -- for `{file.i}`, `{file.i args}`
- `Kind::IncludeArgReference` -- for `{1}`, `{2}`, `{0}`

### AST Changes (`crates/oxabl_ast/src/`)

**statement.rs:** Add variant:
```rust
IncludeReference {
    path_and_args: String,  // raw content between braces
    span: Span,
}
```

**expression.rs:** Add variant:
```rust
IncludeReference {
    path_and_args: String,
    span: Span,
}

IncludeArgReference {
    index: i64,
    span: Span,
}
```

### Parser Changes (`crates/oxabl_parser/src/parser/`)

**statements.rs:** Add `Kind::IncludeReference` and `Kind::IncludeArgReference` checks in `parse_statement` dispatch, before the fallthrough to assignment/expression parsing.

**expressions.rs:** Add handling in `parse_primary` for both new token kinds, producing the corresponding Expression variants.

## Acceptance Criteria

- [ ] `{file.i}` tokenized as single `IncludeReference` token
- [ ] `{path/to/file.i}` tokenized correctly with path separators
- [ ] `{file.i arg1 arg2}` tokenized with arguments in raw content
- [ ] `{file.i &name=value}` tokenized with named arguments in raw content
- [ ] `{1}`, `{2}`, `{0}` tokenized as `IncludeArgReference` with correct index
- [ ] `{&var}` still works as `Preprop` (regression check)
- [ ] Parser accepts include references at statement level
- [ ] Parser accepts include references at expression level
- [ ] All existing tests pass (no regressions)
- [ ] Unterminated `{file.i` produces `Kind::Invalid`
- [ ] Content between braces is trimmed of leading/trailing whitespace

## Test Plan

### Lexer Tests
1. Simple include: `{file.i}` -> IncludeReference with value "file.i"
2. Path include: `{mod/file.i}` -> IncludeReference with value "mod/file.i"
3. Include with positional args: `{file.i NEW}` -> IncludeReference with value "file.i NEW"
4. Include with named args: `{file.i &name=value}` -> IncludeReference with full raw content
5. Argument reference: `{1}` -> IncludeArgReference with value 1
6. Argument reference zero: `{0}` -> IncludeArgReference with value 0
7. Preprocessor regression: `{&var}` -> Preprop (unchanged)
8. Whitespace trimming: `{ file.i }` -> IncludeReference with value "file.i"
9. Unterminated include: `{file.i` -> Kind::Invalid

### Parser Tests
10. Statement-level include: `{file.i}.` -> Statement::IncludeReference
11. Expression-level include: `x = {file.i}.` -> Assignment with Expression::IncludeReference
12. Argument reference in expression: `DEF VAR x AS INT INIT {1}.` -> uses IncludeArgReference
13. Include inside block: `DO: {body.i} END.` -> Block containing IncludeReference statement
14. Multiple includes: `{a.i} {b.i}.` -> two statements (first is period-less, second has period)

## Sources & References

- Existing preprocessor handling: `crates/oxabl_lexer/src/lib.rs:215-227` (brace dispatch), `crates/oxabl_lexer/src/lib.rs:459-472` (read_preprocessor_reference)
- Parser dispatch: `crates/oxabl_parser/src/parser/statements.rs:12-113`
- Expression primary: `crates/oxabl_parser/src/parser/expressions.rs`
- ABL include file syntax reference: real-world ABL codebase analysis (extensive real-world examples)
