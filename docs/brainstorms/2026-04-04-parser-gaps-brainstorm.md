# Brainstorm: Parser Gaps - Streams/Frames, Database Ops, Preprocessor

**Date:** 2026-04-04
**Status:** Ready for planning

## What We're Building

Three major parser feature areas to bring oxabl closer to handling real-world ABL codebases with formatter/linter-ready AST fidelity:

1. **Database manipulation statements** - CREATE, DELETE, RELEASE, VALIDATE, BUFFER-COPY, BUFFER-COMPARE
2. **Streams and frames** - DEFINE STREAM, DEFINE FRAME, INPUT/OUTPUT/CLOSE stream operations, and STREAM/FRAME clause recognition on existing statements
3. **Preprocessor statements** - &IF/&ELSEIF/&ELSE/&ENDIF, &SCOPED-DEFINE, &GLOBAL-DEFINE, &UNDEFINE, &MESSAGE as AST nodes

## Why This Approach

**Goal: Formatter/linter-ready AST.** Every construct needs full structural fidelity so downstream tools can round-trip the code. This means preserving all syntactic elements in the AST, not just enough to "not choke."

**Preprocessor as AST nodes, not evaluation.** Preprocessor directives are stored as nodes in the tree. A formatter needs to see the original `&IF`/`&DEFINE` structure to reproduce it. No condition evaluation or variable substitution happens at parse time. An optional evaluation pass can be added later if needed.

**Preprocessor nodes at multiple AST levels.** In real ABL, preprocessor directives appear mid-statement (e.g., `&IF` inside a DEFINE to conditionally pick a data type). Preprocessor nodes must exist at both the statement level AND inside expressions/type positions. This means preprocessor isn't just a `Statement` variant — it also needs representation in `Expression` (or as a wrapper node) so the formatter can round-trip code like:
```abl
DEFINE VARIABLE x AS
  &IF DEFINED(USE-INT64) &THEN INT64 &ELSE INTEGER &ENDIF
  NO-UNDO.
```

**Frames: DEFINE + references, not full frame phrases.** Frame phrase syntax (`WITH FRAME f COLUMN col ROW row ...`) is extremely complex. Initial scope covers DEFINE FRAME, DEFINE STREAM, and recognizing STREAM/FRAME clauses on existing statements (INPUT/OUTPUT, DISPLAY). Full frame phrase parsing is deferred.

**Database ops: full buffer operations.** Beyond CREATE/DELETE/RELEASE, include VALIDATE (commonly paired with CREATE) and BUFFER-COPY/BUFFER-COMPARE for comprehensive buffer/record handling.

## Key Decisions

- **3 separate PRs**, one per feature area. Easier to review, bisect, and revert independently. Suggested order: DB ops (simplest) -> preprocessor -> streams/frames (most complex).
- **AST nodes for preprocessor** - no evaluation/substitution phase. Preserves directives for formatting. Nodes exist at both statement and expression/type levels to handle mid-statement directives.
- **Frames scoped to DEFINE + clause references** - full frame phrase syntax deferred to a separate effort.
- **DB ops include VALIDATE, BUFFER-COPY, BUFFER-COMPARE** alongside CREATE/DELETE/RELEASE.
- **All work follows established parser patterns**: keyword dispatch in `parse_statement()`, dedicated `parse_*()` functions, `can_start_statement()` updates, comprehensive tests.

## Scope Per PR

### PR 1: Database Manipulation

**Statements:**
- `CREATE buffer-name.` - creates a new record
- `DELETE buffer-name [NO-ERROR].` - deletes a record
- `RELEASE buffer-name [NO-ERROR].` - releases a record from the buffer
- `VALIDATE buffer-name [NO-ERROR].` - validates a record
- `BUFFER-COPY source TO target [ASSIGN field = expr ...] [NO-ERROR].`
- `BUFFER-COMPARE source TO target [SAVE RESULT IN lvar] [NO-ERROR].`

**Files to modify:**
- `crates/oxabl_ast/src/statement.rs` - new enum variants
- `crates/oxabl_parser/src/parser/statements.rs` - dispatch + parser functions
- `crates/oxabl_parser/src/parser/tests.rs` - test cases

**Lexer keywords already exist:** `Create`, `Delete`, `Release`, `Validate`. May need to add `BufferCopy`, `BufferCompare` via `keyword_overrides.toml` + codegen.

### PR 2: Preprocessor Statements

**Statements:**
- `&IF expression &THEN ... [&ELSEIF expression &THEN ...] [&ELSE ...] &ENDIF`
- `&SCOPED-DEFINE name value`
- `&GLOBAL-DEFINE name value`
- `&UNDEFINE name`
- `&MESSAGE "text"` (compile-time message)

**Design notes:**
- Preprocessor conditions may reference preprocessor variables (`{&variable}`) and use DEFINED() function
- `&DEFINE` values are raw text until end-of-line, not parsed expressions
- `&IF` bodies contain regular statements (parsed recursively) at statement level
- Mid-statement `&IF` (e.g., conditional data types) requires preprocessor nodes in `Expression` or as a wrapper that can appear in type/expression positions
- INPUT/OUTPUT keywords already mean parameter direction in the parser — dispatch must disambiguate between `INPUT parameter` and `INPUT FROM file`

**Files to modify:** Same as PR 1 (AST, parser, tests), plus `crates/oxabl_ast/src/expression.rs` for expression-level preprocessor nodes

**Lexer keywords already exist:** All `Preproc*` variants are already tokenized.

### PR 3: Streams and Frames

**Statements:**
- `DEFINE STREAM stream-name.`
- `DEFINE FRAME frame-name [field-list] [WITH ...].` (simplified - no full frame phrase parsing)
- `INPUT FROM file-name.` / `INPUT THROUGH program.` / `INPUT CLOSE.`
- `OUTPUT TO file-name [APPEND].` / `OUTPUT THROUGH program.` / `OUTPUT CLOSE.`
- `INPUT-OUTPUT THROUGH program.` / `INPUT-OUTPUT CLOSE.`

**Clause recognition on existing statements:**
- `DISPLAY ... WITH FRAME frame-name.` - recognize FRAME reference
- `PUT STREAM stream-name ...` - recognize STREAM reference

**Design notes:**
- INPUT/OUTPUT are already used for parameter direction (`DEFINE INPUT PARAMETER`). Parser dispatch must disambiguate: after DEFINE, INPUT means parameter; at statement level, INPUT means stream I/O.

**Files to modify:** Same as PR 1 (AST, parser, tests). May need new Kind variants via codegen for INPUT-OUTPUT, APPEND, THROUGH.

## Open Questions

None - all key decisions resolved during brainstorm.
