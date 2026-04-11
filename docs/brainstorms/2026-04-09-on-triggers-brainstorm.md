# Brainstorm: ON Triggers & TRIGGER PROCEDURE

**Date:** 2026-04-09
**Status:** Complete

## What We're Building

Full parsing support for ABL's ON statement (all 4 forms) and the TRIGGER PROCEDURE statement. These are event handlers for UI events, database events, key remapping, and web notifications, plus schema-level trigger procedure declarations.

### ON Statement Forms

1. **UI/Developer event triggers** -- `ON event-list OF widget-list [ANYWHERE] trigger-block`
2. **Database event triggers** -- `ON CREATE|DELETE|FIND|WRITE|ASSIGN OF table [referencing-phrase] [OVERRIDE] trigger-block`
3. **Key remapping** -- `ON key-label key-function`
4. **SpeedScript web notify** -- `ON "WEB-NOTIFY" ANYWHERE trigger-block`

### TRIGGER PROCEDURE Statement

File-level declaration for schema triggers:
- `TRIGGER PROCEDURE FOR CREATE|DELETE|FIND OF table`
- `TRIGGER PROCEDURE FOR WRITE OF table [NEW [BUFFER] name] [OLD [BUFFER] name]`
- `TRIGGER PROCEDURE FOR ASSIGN OF table.field` (OF form)
- `TRIGGER PROCEDURE FOR ASSIGN NEW [VALUE] var {AS type | LIKE field} ...` (NEW VALUE form)

## Why This Approach

### AST Design: Statement::On + OnKind enum + separate Statement::TriggerProcedure

- **Single `Statement::On` variant** with an `OnKind` enum distinguishing UI event, DB event, key remap, and web notify. Keeps the Statement enum slim while capturing all forms.
- **Separate `Statement::TriggerProcedure`** because it's semantically distinct -- a file-level declaration, not an event handler. Different dispatch keyword (`TRIGGER` vs `ON`).

### Trigger Body: Match IF/THEN pattern

- Check for `DO` keyword -- if present, parse full block via `parse_block_body()`; otherwise parse a single statement + period.
- Reuse `parse_block_body()` as-is so ON trigger DO blocks get CATCH/FINALLY support for free.

### Disambiguation: Lookahead after ON

- `ON` appears both as a trigger statement (`ON CHOOSE OF btn`) and in block headers (`DO ON ERROR UNDO`).
- Peek at next token(s) to distinguish: DB events (CREATE/DELETE/WRITE/FIND/ASSIGN) and known UI event names vs block-header keywords (ERROR/ENDKEY/STOP/QUIT).
- Similar pattern to how `Kind::Input` already uses lookahead for stream I/O vs parameter direction.

## Key Decisions

1. **Scope**: All 4 ON forms + TRIGGER PROCEDURE in one pass
2. **AST**: `Statement::On { kind: OnKind, ... }` + `Statement::TriggerProcedure { ... }` as separate variants
3. **Block parsing**: Reuse `parse_block_body()` for DO blocks (gets CATCH/FINALLY for free)
4. **Trigger body**: Single statement OR DO...END block, matching IF/THEN pattern
5. **Disambiguation**: Lookahead on token(s) after ON to distinguish trigger vs block header
6. **New keywords**: Add event names and clause keywords to `keyword_overrides.toml`, regenerate via codegen

## Implementation Checklist (for planning phase)

- [ ] Add new keywords to `keyword_overrides.toml` (event names, ANYWHERE, OVERRIDE, REVERT, PERSISTENT, TRIGGER, PROCEDURE)
- [ ] Regenerate lexer via `cargo run -p oxabl_codegen`
- [ ] Define `OnKind` enum and `Statement::On` variant in `oxabl_ast`
- [ ] Define `Statement::TriggerProcedure` variant in `oxabl_ast`
- [ ] Add `Kind::On` to `can_start_statement()` and `Kind::Trigger` dispatch
- [ ] Implement `parse_on_statement()` with lookahead disambiguation
- [ ] Implement `parse_trigger_procedure()`
- [ ] Add comprehensive tests for all forms
- [ ] Update CLAUDE.md parser capabilities

## Open Questions

None -- all key decisions resolved during brainstorm.
