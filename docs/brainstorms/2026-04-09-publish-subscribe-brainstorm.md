# Brainstorm: PUBLISH/SUBSCRIBE Event System

**Date:** 2026-04-09
**Status:** Complete
**Author:** Claude + Evan

## What We're Building

Full parsing support for ABL's event system, covering both:

1. **Named events** (procedure-based statements): `PUBLISH`, `SUBSCRIBE`, `UNSUBSCRIBE`
2. **Class events** (OO-ABL): `DEFINE EVENT` with `SIGNATURE VOID(...)` parameter definitions

Class event method calls (`:Publish()`, `:Subscribe()`, `:Unsubscribe()`) are syntactically regular method calls and are already handled by existing postfix parsing. No new parsing is needed for those.

## Why This Approach

### Unified AST with Shared Event Parameters

Named events and class events both deal with event names and parameter lists. Rather than duplicating parameter representations, we use a shared `EventParameter` type across:

- `PUBLISH` statement parameters
- `DEFINE EVENT SIGNATURE VOID(...)` parameter definitions

This reduces duplication, makes future semantic validation easier (checking that a PUBLISH call matches a DEFINE EVENT signature), and keeps the AST consistent.

### Parse Fully, Validate Later

The parser captures complete syntax structure including SIGNATURE parameter types, modes (INPUT/OUTPUT/INPUT-OUTPUT), and names. Cross-reference validation (e.g., "does this Publish() call match the event's defined signature?") is deferred to a future semantic analysis pass — standard compiler architecture.

## Key Decisions

- **Scope**: Named events (PUBLISH/SUBSCRIBE/UNSUBSCRIBE) + class events (DEFINE EVENT). Full suite.
- **UNSUBSCRIBE**: Included in this effort, not deferred.
- **AST design**: Unified — shared `EventParameter` type across named and class event constructs.
- **Class event methods**: `:Publish()`, `:Subscribe()`, `:Unsubscribe()` are already parseable as method calls via existing postfix parsing. No special-casing needed.
- **Validation**: Full syntax parsed into AST. Semantic cross-reference validation deferred to a future pass.
- **Keywords to add**: `PUBLISH`, `SUBSCRIBE`, `UNSUBSCRIBE`, `ANYWHERE`, `EVENT` (if not present), `SIGNATURE`, `VOID`. Existing keywords reused: `FROM`, `TO`, `IN`, `ALL`, `NO-ERROR`, `PROCEDURE`, `RUN-PROCEDURE`.

## Syntax Reference

### PUBLISH (named event)

```
PUBLISH event-name
  [ FROM publisher-handle ]
  [ ( parameter [ , parameter ] ... ) ].
```

- `event-name`: quoted string or character expression
- `FROM publisher-handle`: procedure/widget handle (defaults to THIS-PROCEDURE)
- Parameters: INPUT, OUTPUT, INPUT-OUTPUT (same syntax as RUN statement)
- Implicit NO-ERROR behavior

### SUBSCRIBE (named event)

```
SUBSCRIBE [ PROCEDURE subscriber-handle ] [ TO ] event-name
  { IN publisher-handle | ANYWHERE }
  [ RUN-PROCEDURE local-internal-procedure ]
  [ NO-ERROR ].
```

- `PROCEDURE subscriber-handle`: optional, defaults to THIS-PROCEDURE
- `TO`: optional noise word
- `IN publisher-handle` or `ANYWHERE`: mutually exclusive, required
- `RUN-PROCEDURE`: names the handler internal procedure (defaults to event name)

### UNSUBSCRIBE (named event)

```
UNSUBSCRIBE [ PROCEDURE subscriber-handle ]
  [ TO ] { event-name | ALL }
  [ IN publisher-handle ].
```

- `event-name` or `ALL`: which subscriptions to cancel
- `IN publisher-handle`: limit to specific publisher
- Implicit NO-ERROR behavior

### DEFINE EVENT (class event)

```
DEFINE [ access-mode ] [ STATIC | ABSTRACT ]
  EVENT event-name
  SIGNATURE VOID ( [ parameter-definition [ , ... ] ] ).
```

- Access modes: PRIVATE, PROTECTED, PUBLIC (etc.)
- STATIC or ABSTRACT modifiers
- SIGNATURE always VOID return type
- Parameter definitions follow standard ABL parameter syntax

## Proposed AST Nodes

### Named Event Statements

```
Statement::Publish {
    event: Expression,              // event name (string literal or expression)
    from_handle: Option<Expression>, // FROM publisher-handle
    arguments: Vec<EventParameter>,  // parameter list
}

Statement::Subscribe {
    subscriber: Option<Expression>,  // PROCEDURE subscriber-handle
    event: Expression,               // event name
    target: SubscribeTarget,         // IN handle | ANYWHERE
    run_procedure: Option<String>,   // RUN-PROCEDURE name
    no_error: bool,
}

Statement::Unsubscribe {
    subscriber: Option<Expression>,  // PROCEDURE subscriber-handle
    event: UnsubscribeEvent,         // event-name | ALL
    in_handle: Option<Expression>,   // IN publisher-handle
}

enum SubscribeTarget {
    InHandle(Expression),
    Anywhere,
}

enum UnsubscribeEvent {
    Named(Expression),
    All,
}
```

### Class Event Definition

```
Statement::DefineEvent {
    name: Identifier,
    access: Option<AccessMode>,
    is_static: bool,
    is_abstract: bool,
    signature: Vec<EventParameter>,  // shared type
}
```

### Shared Type

```
struct EventParameter {
    mode: ParameterMode,        // INPUT, OUTPUT, INPUT-OUTPUT
    name: Identifier,
    data_type: DataType,
}

enum ParameterMode {
    Input,
    Output,
    InputOutput,
}
```

## Implementation Touchpoints

1. **keyword_overrides.toml** — Add PUBLISH, SUBSCRIBE, UNSUBSCRIBE, ANYWHERE, EVENT, SIGNATURE, VOID + run codegen
2. **oxabl_ast/src/statement.rs** — Add Publish, Subscribe, Unsubscribe, DefineEvent variants + supporting types
3. **oxabl_parser/src/parser/statements.rs** — Add to `can_start_statement()` dispatch + implement parse functions
4. **oxabl_parser/src/parser/statements.rs** — Extend `parse_define_statement()` to handle DEFINE EVENT
5. **Tests** — Comprehensive test cases for all syntax variants

## Open Questions

None — all questions resolved during brainstorming.

## Next Steps

Run `/ce:plan` to create a detailed implementation plan.
