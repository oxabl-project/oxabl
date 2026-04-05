# Brainstorm: Migrate Parser Keyword Workarounds to Proper Lexer Tokens

**Date:** 2026-04-04
**Status:** Ready for planning

## What We're Building

Migrate four ABL keywords from string-comparison workarounds in the parser to properly tokenized `Kind` variants in the lexer. Currently, VARIABLE/VAR, FUNCTION, CATCH, and FINALLY are tokenized as `Kind::Identifier` by the lexer, and the parser uses `eq_ignore_ascii_case()` / `is_identifier_text()` to match them by text. This breaks the fundamental parser contract: dispatch on token kind, not token text.

## Why This Approach

### The Problem

The parser has two dispatch mechanisms:
1. **Correct:** `self.check(Kind::Keyword)` — used for DEFINE, DO, IF, REPEAT, PROCEDURE, etc.
2. **Workaround:** `text.eq_ignore_ascii_case("keyword")` — used for VAR, FUNCTION, CATCH, FINALLY

The workaround exists because these keywords are **unreserved** in ABL and aren't in the reserved keywords file that the codegen pipeline reads. Rather than adding them to `keyword_overrides.toml` (the established mechanism for this), string comparisons were used as an expedient shortcut during rapid feature development.

### Why Fix It Now

- **Inconsistency:** PROCEDURE was correctly added to `keyword_overrides.toml` early on (Feb 1). FUNCTION, added two months later (Mar 31), was not. Same language construct, different handling.
- **Established precedent:** Commit `f6f1853` (Apr 2) already performed this exact migration for TEMP-TABLE, BUFFER, INITIAL, and EXTENT. This is the continuation of that work.
- **Parser integrity:** A parser should dispatch on token types, not raw text. Every string comparison is a maintenance hazard and a deviation from the architecture.
- **The `is_identifier_text()` helper** (statements.rs:1466) exists solely to support this workaround and should be eliminated.

### Root Cause

The codegen pipeline reads only from `abl_reserved_keywords.txt`. These four keywords are unreserved in ABL (confirmed in `abl_keyword_index.html`), so they require manual entries in `keyword_overrides.toml`. This was done for PROCEDURE but not for the others.

## Key Decisions

1. **VARIABLE with VAR as abbreviation** — ABL documentation defines VARIABLE as the keyword with minimum abbreviation "VAR". This matches the existing pattern (DEFINE/DEF, PROCEDURE/PROCE). The lexer will emit `Kind::Variable` for both `var` and `variable`.

2. **FUNCTION as exact match** — ABL docs show FUNCTION has no abbreviation. Add as `keyword_type = "Statement"` with no `min_abbreviation`.

3. **CATCH and FINALLY as exact matches** — ABL docs show neither has an abbreviation. Add as `keyword_type = "Statement"`.

4. **Remove `is_identifier_text()` helper** — After migration, this method has no remaining callers and should be deleted.

5. **Follow the established migration pattern** — Same approach as commit `f6f1853` (TEMP-TABLE/BUFFER migration): add to overrides, regenerate, update parser, run tests.

## Scope

### In Scope

| Keyword | `keyword_overrides.toml` Entry | Abbreviation | Kind Variant |
|---------|-------------------------------|--------------|--------------|
| VARIABLE | `[[add]]` name = "VARIABLE" | min_abbreviation = "VAR" | `Kind::Variable` |
| FUNCTION | `[[add]]` name = "FUNCTION" | (none) | `Kind::Function` |
| CATCH | `[[add]]` name = "CATCH" | (none) | `Kind::Catch` |
| FINALLY | `[[add]]` name = "FINALLY" | (none) | `Kind::Finally` |

**Parser changes:**
- `parse_statement()`: Replace `Identifier` + string check for VAR/FUNCTION with `Kind::Variable` / `Kind::Function` checks
- `parse_define_statement()`: Replace string check for "variable"/"var" with `Kind::Variable` check
- `parse_block_body()`: Replace `is_identifier_text("catch"/"finally")` with `Kind::Catch` / `Kind::Finally`
- `parse_function()`: Replace string check for "function" with `Kind::Function`
- `parse_catch_block()` / `parse_finally_block()`: Update accordingly
- `can_start_statement()`: Add `Kind::Variable` and `Kind::Function` to the match
- Delete `is_identifier_text()` helper

**Codegen:**
- Run `cargo run -p oxabl_codegen` to regenerate `kind.rs`, `build.rs`, and `callable.rs`

### Out of Scope

- Adding other unreserved keywords not yet needed by the parser
- Changing the codegen pipeline to automatically ingest unreserved keywords
- Parser feature work (new statement types, etc.)

## Risks

- **Low:** These are additive changes to the keyword set. Existing tokens that were `Identifier` will now be their proper `Kind`. Parser tests should catch any regressions.
- **Callable kinds:** FUNCTION, CATCH, FINALLY should NOT be in `is_callable_kind()` (they're statement keywords, not built-in functions). VARIABLE should not be callable either. Verify after regeneration.

## Open Questions

None — all questions resolved during brainstorming.
