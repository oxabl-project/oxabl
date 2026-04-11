---
title: "feat: Add ON trigger and TRIGGER PROCEDURE statement parsing"
type: feat
status: completed
date: 2026-04-09
origin: docs/brainstorms/2026-04-09-on-triggers-brainstorm.md
---

# feat: Add ON Trigger and TRIGGER PROCEDURE Statement Parsing

## Enhancement Summary

**Deepened on:** 2026-04-09
**Sections enhanced:** 6
**Review agents used:** architecture-strategist, code-simplicity-reviewer, pattern-recognition-specialist, performance-oracle, FDM4/ABL syntax validator, spec-flow-analyzer

### Key Improvements
1. Fixed incorrect disambiguation safety claim -- `parse_do_statement()` does NOT consume ON in block headers today; the dispatch is safe trivially because that construct isn't implemented yet
2. Widget references need `IN FRAME`/`IN BROWSE` qualifiers -- `Vec<Identifier>` is insufficient for real ABL code; added `WidgetRef` struct
3. Merged `OnKind::WebNotify` into `UiEvent` (YAGNI -- syntactically identical to UI event with ANYWHERE)
4. Extracted shared `TriggerReferencing` struct for NEW/OLD BUFFER fields used by both `DbEvent` and `TriggerProcedure`
5. Fixed type mismatch: `old_value` on `TriggerProcedure` must be `Option<TriggerAssignParam>`, not `Option<Identifier>`
6. Added REPLICATION-CREATE/DELETE/WRITE to `DbTriggerEvent` for TRIGGER PROCEDURE completeness
7. Key remapping disambiguation: `is_key_label()`/`is_key_function()` should accept any identifier (not restrictive lists), since `ON <ident> <ident> .` with no OF is always key remapping

### New Considerations Discovered
- `Kind::Leave`, `Kind::Create`, `Kind::Delete`, etc. double as event names in ON triggers -- the event parser must accept statement keywords via `can_be_identifier()` or a dedicated helper
- ASSIGN OF targets use `table.field` dotted names -- verify `parse_identifier()` handles this or use expression parsing
- OR keyword serves dual role (logical operator vs event chain separator) -- context resolves this naturally since OR appears between widget-list and next event-list
- ON triggers are valid inside CLASS bodies -- add test for this
- Trigger phrase (inline `TRIGGERS: ... END TRIGGERS.` in widget definitions) is out of scope but noted as future gap

---

## Overview

Add full parsing support for ABL's ON statement (all 4 forms) and the TRIGGER PROCEDURE statement. These are event handlers for UI events, database events, key remapping, and web notifications, plus schema-level trigger procedure declarations.

This completes the last remaining parser gap listed in CLAUDE.md: "ON triggers."

(see brainstorm: `docs/brainstorms/2026-04-09-on-triggers-brainstorm.md`)

## Problem Statement / Motivation

Real-world ABL code uses ON triggers extensively for UI event handling, database triggers, and key remapping. Without parser support, any ABL file containing these constructs fails to parse, blocking downstream tooling (formatting, linting, analysis).

## Proposed Solution

### Phase 1: Keywords and Codegen

Add new keywords to `resources/keyword_overrides.toml` and regenerate.

**New keywords needed:**

| Keyword | `keyword_type` | Notes |
|---------|----------------|-------|
| `CHOOSE` | `Option` | UI event name (not in Kind yet) |
| `ENDKEY` | `Option` | Key function / event name (not in Kind yet) |

**Keywords that already exist in Kind (no codegen changes):**

- `Kind::On` (line 108), `Kind::Trigger` (line 330), `Kind::Anywhere` (line 322)
- `Kind::Persistent` (line 257), `Kind::Override` (line 295), `Kind::Revert` (line 531)
- `Kind::Assign` (line 80), `Kind::Create`, `Kind::Delete`
- `Kind::Find`, `Kind::Write` (line 584), `Kind::GoOn` (line 466)
- `Kind::Entry` (line 96), `Kind::ValueChanged` (line 572), `Kind::ErrorStatus` (line 422)

After adding new keywords: `cargo run -p oxabl_codegen`

### Phase 2: AST Types

**File:** `crates/oxabl_ast/src/statement.rs`

#### 2a. Shared Types

Extract referencing fields shared by `OnKind::DbEvent` and `Statement::TriggerProcedure`:

```rust
/// Referencing phrase for database triggers (NEW/OLD BUFFER for WRITE, OLD VALUE for ASSIGN).
/// Shared between ON db-event triggers and TRIGGER PROCEDURE.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TriggerReferencing {
    /// NEW [BUFFER] alias (WRITE triggers).
    pub new_buffer: Option<Identifier>,
    /// OLD [BUFFER] alias (WRITE triggers).
    pub old_buffer: Option<Identifier>,
    /// OLD [VALUE] alias (ASSIGN triggers in ON statement).
    pub old_value: Option<Identifier>,
}

/// Database trigger event types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DbTriggerEvent {
    Create,
    Delete,
    Find,
    Write,
    Assign,
    /// Replication events (TRIGGER PROCEDURE only).
    ReplicationCreate,
    ReplicationDelete,
    ReplicationWrite,
}

/// Widget reference in an ON trigger, with optional frame/browse qualifier.
///
/// `btnOk IN FRAME main-frame` or `col1 IN BROWSE brw1`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WidgetRef {
    pub name: Identifier,
    pub qualifier: Option<WidgetQualifier>,
}

/// Optional qualification for a widget reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WidgetQualifier {
    /// IN FRAME frame-name
    InFrame(Identifier),
    /// IN BROWSE browse-name
    InBrowse(Identifier),
}
```

#### 2b. ON Statement

Add `Statement::On` with an `OnKind` enum to distinguish the 3 forms (WebNotify merged into UiEvent per simplicity review):

```rust
/// ON trigger statement -- event handlers for UI, database, and key events.
///
/// ```abl
/// ON CHOOSE OF btnOk IN FRAME f1 DO: /* ... */ END.
/// ON WRITE OF Customer NEW BUFFER bNew OLD BUFFER bOld DO: /* ... */ END.
/// ON F1 HELP.
/// ON "WEB-NOTIFY" ANYWHERE DO: /* ... */ END.
/// ```
On {
    kind: OnKind,
},
```

```rust
/// Discriminant for the different forms of the ON statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OnKind {
    /// UI/developer event trigger (includes "WEB-NOTIFY" ANYWHERE form):
    /// ON event-list OF widget-list [OR event-list OF widget-list]... [ANYWHERE]
    ///   { trigger-block | REVERT | PERSISTENT RUN proc [(args)] }
    UiEvent {
        /// Event/widget clauses -- at least one, chained via OR.
        /// Empty when ANYWHERE is used standalone (e.g., ON "WEB-NOTIFY" ANYWHERE).
        clauses: Vec<OnEventClause>,
        /// Whether ANYWHERE was specified.
        anywhere: bool,
        /// The trigger action.
        action: OnAction,
    },
    /// Database event trigger:
    /// ON CREATE|DELETE|FIND|WRITE|ASSIGN OF table [referencing] [OVERRIDE]
    ///   { trigger-block | REVERT }
    DbEvent {
        /// The database event.
        event: DbTriggerEvent,
        /// The table (or table.field for ASSIGN) the trigger is on.
        target: Identifier,
        /// NEW/OLD BUFFER/VALUE referencing phrases.
        referencing: TriggerReferencing,
        /// Whether OVERRIDE was specified.
        is_override: bool,
        /// The trigger action (block or REVERT).
        action: OnAction,
    },
    /// Key remapping: ON key-label key-function.
    KeyRemap {
        /// The key label (e.g., F1, CTRL-X) -- any identifier.
        key_label: Identifier,
        /// The key function (e.g., HELP, ENDKEY, GO) -- any identifier.
        key_function: Identifier,
    },
}
```

```rust
/// A single event/widget-list clause in a UI ON trigger.
///
/// `ON CHOOSE, ENTRY OF btnOk IN FRAME f1, btnCancel`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OnEventClause {
    /// Comma-separated event names (identifiers, including keywords like LEAVE/ENTRY).
    pub events: Vec<Identifier>,
    /// Comma-separated widget references with optional frame/browse qualifiers.
    pub widgets: Vec<WidgetRef>,
}

/// The action taken by an ON trigger.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OnAction {
    /// A trigger block -- either a single statement or DO...END block.
    Block(Box<Statement>),
    /// REVERT -- removes the trigger.
    Revert,
    /// PERSISTENT RUN procedure [(args)].
    PersistentRun {
        procedure: Identifier,
        arguments: Vec<Expression>,
    },
}
```

#### 2c. TRIGGER PROCEDURE Statement

Separate `Statement::TriggerProcedure` variant (see brainstorm: semantically distinct from ON):

```rust
/// TRIGGER PROCEDURE FOR event OF table [NEW/OLD clauses].
///
/// Declares a schema trigger -- always the first statement in a trigger procedure file.
///
/// ```abl
/// TRIGGER PROCEDURE FOR WRITE OF Customer
///     NEW BUFFER bNew OLD BUFFER bOld.
/// ```
TriggerProcedure {
    /// The trigger event (CREATE, DELETE, FIND, WRITE, ASSIGN, or REPLICATION-*).
    event: DbTriggerEvent,
    /// The target table (or table.field for ASSIGN OF form).
    target: Identifier,
    /// NEW/OLD BUFFER referencing (WRITE triggers).
    referencing: TriggerReferencing,
    /// NEW VALUE variable definition (ASSIGN triggers, mutually exclusive with OF form).
    new_value: Option<TriggerAssignParam>,
    /// OLD VALUE variable definition (ASSIGN NEW VALUE form).
    old_value_param: Option<TriggerAssignParam>,
},
```

```rust
/// A variable-like parameter for TRIGGER PROCEDURE FOR ASSIGN NEW VALUE form.
///
/// ```abl
/// TRIGGER PROCEDURE FOR ASSIGN
///     NEW VALUE newVal AS CHARACTER
///     OLD VALUE oldVal AS CHARACTER.
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TriggerAssignParam {
    pub name: Identifier,
    pub data_type: DataType,
}
```

**Simplification note:** `TriggerAssignParam` reduced to just `(name, data_type)` fields. The ABL grammar technically allows INITIAL, NO-UNDO, COLUMN-LABEL, FORMAT, LABEL on these params, but they are virtually never used in real trigger declarations. Promote to a richer struct later if needed.

#### 2d. Import Updates

Add new types to `crates/oxabl_ast/src/lib.rs` re-exports:
`DbTriggerEvent`, `OnAction`, `OnEventClause`, `OnKind`, `TriggerAssignParam`, `TriggerReferencing`, `WidgetQualifier`, `WidgetRef`

### Phase 3: Parser Implementation

**File:** `crates/oxabl_parser/src/parser/statements.rs`

#### 3a. Statement Dispatch

Add to `can_start_statement()`:
```rust
| Kind::On
| Kind::Trigger
```

Add to `parse_statement()` dispatch chain (before the expression/assignment fallback at line 263):
```rust
// ON triggers
if self.check(Kind::On) {
    return self.parse_on_statement();
}

// TRIGGER PROCEDURE
if self.check(Kind::Trigger) {
    return self.parse_trigger_procedure();
}
```

#### 3b. Disambiguation Strategy

The `ON` keyword appears in two contexts:
1. **Trigger statement** (top-level): `ON CHOOSE OF btn DO: ... END.`
2. **Block header phrase**: `DO ON ERROR UNDO: ... END.`

**Current state:** `parse_do_statement()`, `parse_for_each()`, and `parse_repeat_statement()` do NOT currently parse `ON ERROR UNDO` phrases at all. There is no `Kind::On` handling anywhere in the parser today. Therefore, any `ON` that reaches `parse_statement()` is always a trigger statement -- the dispatch is safe because block-header ON simply isn't implemented yet.

**Forward-compatibility invariant:** When `DO ON ERROR UNDO` support is added in the future, it MUST consume the `ON` inside the block header parser (e.g., `parse_do_statement()`) before `parse_statement()` is reached. Add a code comment at the dispatch site documenting this invariant.

Within `parse_on_statement()`, the 3 forms are distinguished by:

```
After consuming ON:
  1. String literal → UiEvent with ANYWHERE (e.g., ON "WEB-NOTIFY" ANYWHERE ...)
  2. DB event keyword (CREATE/DELETE/FIND/WRITE/ASSIGN) + peek(1) is OF → DbEvent
     BUT: if peek(1) is Comma, fall through to UiEvent (e.g., ON CREATE, DELETE OF table)
  3. Any identifier + peek(1) is NOT OF/Comma → KeyRemap (ON <ident> <ident> .)
  4. Everything else → UiEvent (event name list, expects OF for widget list)
```

**Key insight for KeyRemap:** `ON <ident> <ident> .` with no `OF` or comma is always key remapping, because single-statement UI triggers require `OF` between event and widget. No restrictive key-label/key-function lists needed -- accept any identifier pair. This avoids string comparison.

#### 3c. parse_on_statement() Implementation

```
fn parse_on_statement() -> ParseResult<Statement>:
  advance()  // consume ON

  // Check for string literal event name (e.g., "WEB-NOTIFY")
  // Parsed as UiEvent with no clauses and ANYWHERE
  if check(Kind::String):
    // fall through to UI event parsing (string is the event name)
    return parse_on_ui_event_from_string()

  // Check for DB events: CREATE/DELETE/FIND/WRITE/ASSIGN followed by OF (not Comma)
  if is_db_event_kind(current) && check_at(1, Kind::Of):
    return parse_on_db_event()

  // Check for key remapping: identifier followed by identifier, no OF/Comma
  // ON <key-label> <key-function>.
  if !check_at(1, Kind::Of) && !check_at(1, Kind::Comma)
     && check_at(1, /* is identifier-like */) && check_at(2, Kind::Period):
    return parse_on_key_remap()

  // Default: UI/developer event trigger
  return parse_on_ui_event()
```

#### 3d. parse_on_ui_event() -- UI/Developer Events

```
fn parse_on_ui_event() -> ParseResult<Statement>:
  clauses = vec![]
  anywhere = false

  loop:
    events = parse_event_name_list()  // comma-separated, accepts keywords as identifiers
    // Check for ANYWHERE without OF (standalone)
    if check(Kind::Anywhere):
      anywhere = true
      advance()
      break
    expect(Kind::Of)
    widgets = parse_widget_ref_list()  // comma-separated, each with optional IN FRAME/BROWSE
    clauses.push(OnEventClause { events, widgets })

    // Check for OR to chain another clause
    if !check(Kind::KwOr):
      break
    advance()  // consume OR

  // Check for trailing ANYWHERE (after widget list)
  if check(Kind::Anywhere):
    anywhere = true
    advance()

  action = parse_trigger_action()
  return Statement::On { kind: OnKind::UiEvent { clauses, anywhere, action } }
```

**Event name list termination:** Comma-separated identifiers, terminated by `OF`, `ANYWHERE`, or any non-identifier/non-comma token.

**Widget ref list parsing:**
```
fn parse_widget_ref_list() -> ParseResult<Vec<WidgetRef>>:
  refs = vec![]
  loop:
    name = parse_identifier()
    qualifier = None
    if check(Kind::In):
      advance()
      if check(Kind::Frame):
        advance(); qualifier = Some(WidgetQualifier::InFrame(parse_identifier()))
      elif check(Kind::Browse):
        advance(); qualifier = Some(WidgetQualifier::InBrowse(parse_identifier()))
    refs.push(WidgetRef { name, qualifier })
    if !check(Kind::Comma): break
    advance()  // consume comma
  return refs
```

**Widget list termination:** Ends at `OR`, `ANYWHERE`, `DO`, `Colon`, `REVERT`, `PERSISTENT`, or period (i.e., anything that isn't a comma or identifier/IN).

#### 3e. parse_event_name_list() -- Accepts Keywords as Event Names

```
fn parse_event_name_list() -> ParseResult<Vec<Identifier>>:
  // Event names include keywords like LEAVE, ENTRY, CREATE that double as statement keywords.
  // Accept any identifier-like token or known event keyword.
  names = vec![]
  loop:
    names.push(parse_event_name_identifier())  // uses extended can_be_identifier + statement keywords
    if !check(Kind::Comma): break
    advance()
  return names
```

The `parse_event_name_identifier()` helper should accept:
- Any token that passes `can_be_identifier()`
- Plus statement keywords that are also event names: `Kind::Leave`, `Kind::Create`, `Kind::Delete`, `Kind::Find`, `Kind::Close`

#### 3f. parse_on_db_event() -- Database Events

```
fn parse_on_db_event() -> ParseResult<Statement>:
  event = parse_db_event_kind()  // CREATE|DELETE|FIND|WRITE|ASSIGN
  expect(Kind::Of)
  target = parse_identifier()  // table or table.field (dotted name)

  referencing = TriggerReferencing::default()

  // Parse optional referencing phrases
  if event == WRITE:
    if check(Kind::KwNew): advance(); eat optional BUFFER; referencing.new_buffer = Some(parse_identifier())
    if check(Kind::Old):   advance(); eat optional BUFFER; referencing.old_buffer = Some(parse_identifier())
  if event == ASSIGN:
    if check(Kind::Old): advance(); eat optional VALUE; referencing.old_value = Some(parse_identifier())

  is_override = false
  if check(Kind::Override):
    is_override = true
    advance()

  action = parse_trigger_action()
  return Statement::On { kind: OnKind::DbEvent { event, target, referencing, is_override, action } }
```

**ASSIGN OF target:** The target is `table.field` (e.g., `Customer.Name`). This is a dotted name that `parse_identifier()` may not handle. Options:
- Use `parse_expression()` and extract the identifier from the resulting member-access expression
- Or parse the first identifier, check for period + identifier (taking care not to confuse the field separator `.` with the statement terminator `.`), and concatenate

The safest approach is to parse the identifier, then if a period follows and the next token is an identifier-like token (not a keyword that starts a statement), consume the period and next identifier to build a dotted name. This mirrors how CATCH blocks parse dotted class names (lines 2114-2126 of statements.rs).

#### 3g. parse_trigger_action() -- Shared Trigger Body

Follows the IF/THEN pattern (see brainstorm). **Must check REVERT and PERSISTENT before single-statement fallthrough** to avoid misparse:

```
fn parse_trigger_action() -> ParseResult<OnAction>:
  if check(Kind::Revert):
    advance()
    expect(Kind::Period)
    return Ok(OnAction::Revert)

  if check(Kind::Persistent):
    advance()
    expect(Kind::Run)
    procedure = parse_identifier()
    arguments = if check(Kind::LeftParen): parse_argument_list() else vec![]
    expect(Kind::Period)
    return Ok(OnAction::PersistentRun { procedure, arguments })

  // Trigger block: DO...END or single statement
  if check(Kind::Do):
    block = parse_do_statement()  // already handles colon, block body, END, period
    return Ok(OnAction::Block(Box::new(block)))

  // Single statement (terminates with its own period)
  stmt = parse_statement()
  return Ok(OnAction::Block(Box::new(stmt)))
```

#### 3h. parse_trigger_procedure() -- TRIGGER PROCEDURE

```
fn parse_trigger_procedure() -> ParseResult<Statement>:
  advance()  // consume TRIGGER
  expect(Kind::Procedure)
  expect(Kind::KwFor)

  event = parse_db_event_kind()  // CREATE|DELETE|FIND|WRITE|ASSIGN|REPLICATION-*

  // ASSIGN has two mutually exclusive forms
  if event == ASSIGN:
    if check(Kind::Of):
      advance()
      target = parse_identifier()  // table.field (dotted name)
      expect(Kind::Period)
      return Ok(Statement::TriggerProcedure { event, target, referencing: default(), new_value: None, old_value_param: None })
    else:
      // NEW VALUE form
      expect(Kind::KwNew)
      eat optional VALUE
      new_value = parse_trigger_assign_param()
      old_value_param = if check(Kind::Old):
        advance(); eat optional VALUE; Some(parse_trigger_assign_param())
      else None
      expect(Kind::Period)
      // target is empty/placeholder for NEW VALUE form
      return Ok(Statement::TriggerProcedure { event, target: placeholder, new_value: Some(new_value), old_value_param, referencing: default() })

  expect(Kind::Of)
  target = parse_identifier()

  referencing = TriggerReferencing::default()
  if event == WRITE:
    if check(Kind::KwNew): advance(); eat optional BUFFER; referencing.new_buffer = Some(parse_identifier())
    if check(Kind::Old):   advance(); eat optional BUFFER; referencing.old_buffer = Some(parse_identifier())

  expect(Kind::Period)
  return Ok(Statement::TriggerProcedure { event, target, referencing, new_value: None, old_value_param: None })
```

#### 3i. parse_trigger_assign_param()

```
fn parse_trigger_assign_param() -> ParseResult<TriggerAssignParam>:
  name = parse_identifier()
  // AS type or LIKE field
  data_type = parse_data_type()  // reuse existing data type parser
  return Ok(TriggerAssignParam { name, data_type })
```

#### 3j. Import Updates

Add new AST types to the import block at the top of `statements.rs`:
```rust
use oxabl_ast::{
    ..., DbTriggerEvent, OnAction, OnEventClause, OnKind,
    TriggerAssignParam, TriggerReferencing, WidgetQualifier, WidgetRef,
};
```

#### 3k. `can_be_identifier()` Updates

**File:** `crates/oxabl_parser/src/parser/mod.rs`

Add these keywords to `can_be_identifier()` under a new `// ON trigger keywords (unreserved)` section:

```rust
// ON trigger keywords (unreserved)
| Kind::Trigger
| Kind::Anywhere
| Kind::Persistent
| Kind::Revert
| Kind::Override
| Kind::Choose
| Kind::Endkey
```

Note: `Kind::On` is reserved in ABL and should NOT be in `can_be_identifier()`.

### Phase 4: Tests

**File:** `crates/oxabl_parser/src/parser/tests.rs`

Add a new test section using box-drawing header style:
```rust
// ── ON trigger tests ────────────────────────────────────────────────
```

**UI Event form:**
- `parse_on_choose_of_button` -- minimal: `ON CHOOSE OF btnOk DO: MESSAGE "clicked". END.`
- `parse_on_multiple_events` -- comma-separated events: `ON CHOOSE, ENTRY OF btnOk DO: ... END.`
- `parse_on_multiple_widgets` -- comma-separated widgets: `ON CHOOSE OF btn1, btn2 DO: ... END.`
- `parse_on_widget_in_frame` -- frame qualifier: `ON CHOOSE OF btnOk IN FRAME main-frame DO: ... END.`
- `parse_on_or_clause` -- OR chaining: `ON CHOOSE OF btn1 OR ENTRY OF fill1 DO: ... END.`
- `parse_on_anywhere` -- ANYWHERE standalone: `ON CHOOSE ANYWHERE DO: ... END.`
- `parse_on_anywhere_with_widgets` -- ANYWHERE after widget list: `ON CHOOSE OF btn1 ANYWHERE DO: ... END.`
- `parse_on_single_statement` -- no DO block: `ON CHOOSE OF btnOk MESSAGE "clicked".`
- `parse_on_revert` -- REVERT action: `ON CHOOSE OF btnOk REVERT.`
- `parse_on_persistent_run` -- PERSISTENT RUN: `ON CHOOSE OF btnOk PERSISTENT RUN myProc.`
- `parse_on_persistent_run_with_args` -- with arguments: `ON CHOOSE OF btnOk PERSISTENT RUN myProc (INPUT x).`
- `parse_on_leave_event_name` -- statement keyword as event: `ON LEAVE OF fill1 DO: ... END.`
- `parse_on_web_notify` -- string event: `ON "WEB-NOTIFY" ANYWHERE DO: ... END.`

**Database Event form:**
- `parse_on_create_of_table` -- `ON CREATE OF Customer DO: ... END.`
- `parse_on_write_with_buffers` -- NEW/OLD BUFFER: `ON WRITE OF Customer NEW BUFFER bNew OLD BUFFER bOld DO: ... END.`
- `parse_on_assign_of_field` -- dotted name: `ON ASSIGN OF Customer.Name DO: ... END.`
- `parse_on_assign_old_value` -- `ON ASSIGN OF Customer.Name OLD VALUE oldName DO: ... END.`
- `parse_on_write_override` -- OVERRIDE: `ON WRITE OF Customer OVERRIDE DO: ... END.`
- `parse_on_db_revert` -- `ON WRITE OF Customer REVERT.`

**Key Remap form:**
- `parse_on_key_remap` -- `ON F1 HELP.`

**TRIGGER PROCEDURE:**
- `parse_trigger_procedure_create` -- `TRIGGER PROCEDURE FOR CREATE OF Customer.`
- `parse_trigger_procedure_write` -- `TRIGGER PROCEDURE FOR WRITE OF Customer NEW BUFFER bNew OLD BUFFER bOld.`
- `parse_trigger_procedure_write_no_buffer_keyword` -- `TRIGGER PROCEDURE FOR WRITE OF Customer NEW bNew OLD bOld.`
- `parse_trigger_procedure_assign_of` -- `TRIGGER PROCEDURE FOR ASSIGN OF Customer.Name.`
- `parse_trigger_procedure_assign_new_value` -- `TRIGGER PROCEDURE FOR ASSIGN NEW VALUE newVal AS CHARACTER.`
- `parse_trigger_procedure_assign_new_old_value` -- `TRIGGER PROCEDURE FOR ASSIGN NEW VALUE newVal AS CHARACTER OLD VALUE oldVal AS CHARACTER.`

**Integration:**
- `parse_on_trigger_in_procedure` -- ON trigger inside a PROCEDURE body
- `parse_on_trigger_inside_do_block` -- ON trigger nested in a DO block (not confused with DO ON ERROR)
- `parse_on_trigger_in_class_body` -- ON trigger inside a CLASS body (valid per ABL spec)

### Phase 5: Update CLAUDE.md

Add to the parser capabilities list in CLAUDE.md:
- `ON` (UI/developer event triggers with IN FRAME/IN BROWSE, database event triggers, key remapping)
- `TRIGGER PROCEDURE` (schema trigger declarations including REPLICATION events)

Update the "Not yet implemented" line to remove "ON triggers."

## Technical Considerations

### Disambiguation Safety

**Current state:** `parse_do_statement()`, `parse_for_each()`, and `parse_repeat_statement()` do NOT handle `ON ERROR UNDO` block-header phrases. There is no `Kind::On` handling anywhere in the parser crate today. Therefore, the `Kind::On` dispatch in `parse_statement()` is safe -- it will only match trigger statements.

**Forward-compatibility invariant:** When `DO ON ERROR UNDO` support is added, it MUST consume the `ON` inside the block header parser before `parse_statement()` is reached. Add a code comment at the dispatch site documenting this.

Within `parse_on_statement()`, the 3 forms are distinguishable by:
1. **String literal** or event names + OF/ANYWHERE --> UiEvent
2. **DB event keyword + OF** (no comma after) --> DbEvent
3. **Two identifiers + period** (no OF/comma) --> KeyRemap

### Performance

- All dispatch uses `Kind` enum matching (O(1)), no string comparison
- New keywords go through `keyword_overrides.toml` + codegen per CLAUDE.md guidance
- `OnAction::Block` boxes the statement to prevent recursive size inflation
- `Statement::On` will NOT inflate the Statement enum (OnKind ~200 bytes, well under DefineDataset's ~352 bytes)
- `Vec<OnEventClause>` allocation is acceptable on this cold path (parsed once per ON statement)
- For the `"WEB-NOTIFY"` string check: use `atom!("WEB-NOTIFY")` if the atom exists, otherwise byte comparison on this rare cold path is fine

### Error Recovery

`Kind::On` and `Kind::Trigger` in `can_start_statement()` allows the error recovery in `parse_program()` to synchronize on these keywords when a preceding statement is malformed.

### Known Future Gaps

- **Trigger phrase** (`TRIGGERS: ON ... END TRIGGERS.` inline in widget definitions) -- separate construct, not in scope
- **PERSISTENT RUN IN handle** -- the trigger phrase supports `IN handle` on PERSISTENT RUN; standalone ON may also need this
- **TRIGGER PROCEDURE ASSIGN** -- full syntax supports LIKE, COLUMN-LABEL, FORMAT, LABEL on params; deferred to keep TriggerAssignParam simple

## Acceptance Criteria

- [ ] All 3 ON statement forms parse correctly (UiEvent, DbEvent, KeyRemap)
- [ ] "WEB-NOTIFY" ANYWHERE parses as UiEvent with no clauses
- [ ] TRIGGER PROCEDURE parses all 5 standard event types (CREATE/DELETE/FIND/WRITE/ASSIGN)
- [ ] TRIGGER PROCEDURE ASSIGN handles both OF and NEW VALUE forms
- [ ] Widget references support IN FRAME and IN BROWSE qualifiers
- [ ] UI triggers support comma-separated events and widgets
- [ ] UI triggers support OR clause chaining
- [ ] ANYWHERE works both standalone and after widget list
- [ ] REVERT action parses for both UI and DB forms
- [ ] PERSISTENT RUN with optional arguments parses
- [ ] Single-statement triggers (no DO block) work
- [ ] DO block triggers get CATCH/FINALLY for free via `parse_block_body()`
- [ ] Event names that are also statement keywords (LEAVE, CREATE, etc.) parse correctly
- [ ] ASSIGN OF targets with dotted table.field names parse correctly
- [ ] ON inside block headers (DO ON ERROR) is NOT broken by this change
- [ ] ON triggers inside CLASS bodies parse correctly
- [ ] All tests pass (existing + new)
- [ ] `cargo clippy -D warnings` passes
- [ ] CLAUDE.md updated

## Sources & References

- **Origin brainstorm:** [docs/brainstorms/2026-04-09-on-triggers-brainstorm.md](docs/brainstorms/2026-04-09-on-triggers-brainstorm.md) -- key decisions: single On variant + OnKind enum, separate TriggerProcedure, reuse parse_block_body(), IF/THEN pattern for trigger bodies
- **Template pattern:** [docs/plans/2026-04-09-001-feat-publish-subscribe-event-system-plan.md](docs/plans/2026-04-09-001-feat-publish-subscribe-event-system-plan.md) -- same implementation flow (codegen -> AST -> parser -> tests -> CLAUDE.md)
- **ABL ON statement:** https://docs.progress.com/bundle/abl-reference/page/ON-statement.html
- **ABL TRIGGER PROCEDURE:** https://docs.progress.com/bundle/abl-reference/page/TRIGGER-PROCEDURE-statement.html
- **Institutional learning:** `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md` -- always use `keyword_overrides.toml` + codegen for new keywords, never string comparison in parser
