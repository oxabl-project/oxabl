---
title: "feat: Add PUBLISH/SUBSCRIBE/UNSUBSCRIBE event system and DEFINE EVENT"
type: feat
status: completed
date: 2026-04-09
origin: docs/brainstorms/2026-04-09-publish-subscribe-brainstorm.md
---

# feat: Add PUBLISH/SUBSCRIBE/UNSUBSCRIBE Event System and DEFINE EVENT

## Enhancement Summary

**Deepened on:** 2026-04-09
**Sections enhanced:** 6
**Review agents used:** architecture-strategist, pattern-recognition-specialist, performance-oracle, code-simplicity-reviewer, software-architect, ABL syntax researcher

### Key Improvements
1. Fixed critical `parse_primary()` inaccuracy — must use hand-rolled event-name parser (parse_primary would still consume function calls)
2. Eliminated redundant `all: bool` in Unsubscribe — use `Option<Expression>` alone or an enum
3. Changed `run_procedure: Option<String>` to `Option<Identifier>` for span tracking consistency
4. Confirmed interface body parsing needs NO changes (parse_interface already delegates to parse_statement)
5. Confirmed ANYWHERE is NOT valid in UNSUBSCRIBE syntax
6. Discovered event names can be full character expressions per ABL docs — impacts parsing strategy

### New Considerations Discovered
- `parse_primary()` CANNOT be used for event names — it promotes `identifier(` to function calls (lines 396-398 of expressions.rs), which is the exact bug we're trying to avoid
- `EventSignatureParam` can be eliminated by reusing `Vec<Statement>` with `DefineParameter` variants (matching how Method handles parameters)
- RUN-PROCEDURE is confirmed as a single hyphenated keyword token in the ABL keyword index
- SUBSCRIBE without IN or ANYWHERE is a compile error per ABL docs (braces = required choice)

---

## Overview

Add full parsing support for ABL's event system: the named-event statements (`PUBLISH`, `SUBSCRIBE`, `UNSUBSCRIBE`) and the class-level `DEFINE EVENT` declaration with `SIGNATURE VOID(...)`. Class event method calls (`:Publish()`, `:Subscribe()`, `:Unsubscribe()`) are already handled by existing postfix parsing and need no changes.

This completes one of the three remaining parser gaps listed in CLAUDE.md: "DATASET, PUBLISH/SUBSCRIBE, ON triggers."

(see brainstorm: `docs/brainstorms/2026-04-09-publish-subscribe-brainstorm.md`)

## Problem Statement / Motivation

Real-world ABL code uses PUBLISH/SUBSCRIBE for inter-procedure event communication. Without parser support, any ABL file containing these statements fails to parse, blocking downstream tooling (formatting, linting, analysis).

## Proposed Solution

### Phase 1: Keywords and Codegen

Add new keywords to `resources/keyword_overrides.toml` and regenerate.

**New keywords:**

| Keyword | `keyword_type` | Notes |
|---------|----------------|-------|
| `PUBLISH` | `Statement` | Top-level statement |
| `SUBSCRIBE` | `Statement` | Top-level statement |
| `UNSUBSCRIBE` | `Statement` | Top-level statement |
| `ANYWHERE` | `Option` | SUBSCRIBE clause only (NOT valid in UNSUBSCRIBE) |
| `EVENT` | `Option` | DEFINE EVENT subtype |
| `SIGNATURE` | `Option` | DEFINE EVENT signature clause |
| `RUN-PROCEDURE` | `Option` | SUBSCRIBE handler clause — single hyphenated keyword token (confirmed in ABL keyword index, line 25734 of abl_keyword_index.html) |

**Not needed:** `VOID` (already exists as `Kind::Void`), `FROM`/`TO`/`IN`/`ALL`/`NO-ERROR`/`PROCEDURE` (already exist).

No abbreviations — full keywords required. Confirmed: none of these keywords have abbreviations in the ABL keyword index (all show `–` in the abbreviation column).

Add a section header comment `# EVENT SYSTEM KEYWORDS` before the new entries to match the organizational convention in the file.

After editing `keyword_overrides.toml`, run `cargo run -p oxabl_codegen` to regenerate `crates/oxabl_lexer/src/kind.rs`.

**File:** `resources/keyword_overrides.toml`

### Research Insights: Keyword Performance Impact

Adding 7 new keywords to `match_keyword()` has negligible performance impact. The function uses length-dispatched matching — new keywords spread across 6 different length buckets (5, 7, 8, 9, 9, 11, 13). Each bucket is a flat string comparison optimized to a jump table by the compiler. No abbreviations means exactly 7 new match arms total (vs. 4x multiplier if abbreviations were used). The `[u8; 64]` stack buffer is unaffected — all keywords are well under 64 bytes.

### Phase 2: AST Node Definitions

Add new variants and types to `crates/oxabl_ast/src/statement.rs`.

**Design correction from brainstorm:** The brainstorm proposed a unified `EventParameter` type, but analysis revealed this conflates two different things:

- **PUBLISH arguments** are runtime values (direction + expression) — identical to `RunArgument`. Reuse `RunArgument`.
- **DEFINE EVENT SIGNATURE parameters** are type definitions (direction + name + data type) — reuse existing `DefineParameter` via `Vec<Statement>` (matching how `Method` already handles its parameters at line 296 of statement.rs). This eliminates the need for a separate `EventSignatureParam` struct.

#### New Statement Variants

```rust
/// PUBLISH event-name [FROM publisher-handle] [(args...)].
Publish {
    /// Event name — string literal or character expression.
    event_name: Expression,
    /// Optional FROM publisher-handle.
    from_handle: Option<Expression>,
    /// Arguments passed to subscribers (reuses RunArgument).
    arguments: Vec<RunArgument>,
},

/// SUBSCRIBE [PROCEDURE subscriber-handle] [TO] event-name {IN handle | ANYWHERE}
///   [RUN-PROCEDURE handler-name] [NO-ERROR].
Subscribe {
    /// Optional PROCEDURE subscriber-handle.
    subscriber: Option<Expression>,
    /// Event name — string literal or character expression.
    event_name: Expression,
    /// IN publisher-handle or ANYWHERE (required — omitting both is a compile error).
    target: SubscribeTarget,
    /// Optional RUN-PROCEDURE handler name (Identifier preserves span info).
    run_procedure: Option<Identifier>,
    /// Whether NO-ERROR was specified.
    no_error: bool,
},

/// UNSUBSCRIBE [PROCEDURE subscriber-handle] [TO] {event-name | ALL} [IN publisher-handle].
Unsubscribe {
    /// Optional PROCEDURE subscriber-handle.
    subscriber: Option<Expression>,
    /// Event name, or None if ALL was specified.
    event_name: Option<Expression>,
    /// Optional IN publisher-handle.
    in_handle: Option<Expression>,
},

/// DEFINE [access] [STATIC] [ABSTRACT] EVENT event-name SIGNATURE VOID (params...).
DefineEvent {
    /// Access modifier (defaults to PUBLIC).
    access: AccessModifier,
    /// Whether STATIC was specified.
    is_static: bool,
    /// Whether ABSTRACT was specified.
    is_abstract: bool,
    /// Event name.
    name: Identifier,
    /// Signature parameters — reuses DefineParameter via Vec<Statement>, matching Method pattern.
    parameters: Vec<Statement>,
},
```

#### New Supporting Type

```rust
/// Target for SUBSCRIBE — where to listen for events.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubscribeTarget {
    /// Subscribe to events from a specific publisher handle.
    InHandle(Expression),
    /// Subscribe to events from any publisher.
    Anywhere,
}
```

**File:** `crates/oxabl_ast/src/statement.rs`

### Research Insights: AST Design

**Changes from original plan based on review feedback:**

1. **`run_procedure` changed from `Option<String>` to `Option<Identifier>`** — Every named reference in the AST uses `Identifier` (which carries a `Span`). Using bare `String` loses source location information needed for diagnostics, go-to-definition, and formatting. Zero additional heap cost; gains span tracking. (Architecture, Pattern, Performance reviewers unanimous)

2. **Eliminated redundant `all: bool` from Unsubscribe** — The original `(event_name: Option<Expression>, all: bool)` pair allowed impossible states (all=true with event_name=Some). Now using just `event_name: Option<Expression>` where None means ALL was specified. The doc comment makes the semantics clear. (Code-simplicity, Pattern reviewers)

3. **Eliminated `EventSignatureParam` struct** — Reusing `Vec<Statement>` with `DefineParameter` variants matches how `Method` (line 296) and `Constructor` (line 500) already handle parameter lists. The `no_undo` field on `DefineParameter` is set to `false` for event params. This avoids a parallel type hierarchy (YAGNI). (Code-simplicity reviewer)

4. **`SubscribeTarget` enum kept** — justified because IN/ANYWHERE are mutually exclusive AND required. An enum with two variants perfectly encodes this constraint. Using `Option<Expression> + bool` would allow 4 states when only 2 are valid. (Code-simplicity reviewer confirmed: keep it)

### Phase 3: Parser Implementation

**File:** `crates/oxabl_parser/src/parser/statements.rs`

#### 3a. Statement Dispatch

Add to `can_start_statement()` (around line 22):
```rust
Kind::Publish | Kind::Subscribe | Kind::Unsubscribe
```

Add dispatch entries in `parse_statement()` (near the RUN/DISPLAY/MESSAGE block around line 122):
```rust
if self.check(Kind::Publish) {
    return self.parse_publish_statement();
}
if self.check(Kind::Subscribe) {
    return self.parse_subscribe_statement();
}
if self.check(Kind::Unsubscribe) {
    return self.parse_unsubscribe_statement();
}
```

### Research Insights: Dispatch Performance

Adding 3 new `if self.check(Kind::...)` to `parse_statement()` (already 30+ branches) is negligible — each `check()` is a single integer comparison on a `Copy + Eq` enum. The branch predictor handles sequential if-checks well. Similarly, adding 3 variants to `can_start_statement()` (a `matches!()` macro compiled to a bitmask) and 7 variants to `can_be_identifier()` have zero measurable impact.

#### 3b. PUBLISH Parsing

```
PUBLISH event-name [FROM publisher-handle] [(INPUT arg, ...)].
```

**Critical: Event-name expression parsing.** ABL docs describe event-name as "a quoted string or a character expression," which technically includes function calls. However, `parse_primary()` CANNOT be used here — it promotes `identifier(` into function calls (line 396-398 of expressions.rs), which would cause `PUBLISH myFunc (INPUT x).` to be misparsed as "publish the result of myFunc(INPUT x)".

**Use a hand-rolled event-name parser** matching the RUN statement approach (`parse_run_statement()`, lines 1195-1214). The RUN statement does NOT use `parse_primary()` — it explicitly checks for:
1. `Kind::Value` → parse VALUE(expr)
2. `Kind::StringLiteral` → extract string
3. Otherwise → parse a bare name (identifier)

The event-name parser should follow the same three-way pattern. This prevents the parenthesized argument list from being consumed as part of a function call expression.

**Consider factoring out the RUN argument parsing loop** (lines 1217-1256 of statements.rs) into a shared helper like `parse_parenthesized_arguments()` rather than duplicating it in `parse_publish_statement()`. This prevents drift between the two implementations.

Implementation outline:
1. Advance past `Kind::Publish`
2. Parse event name via hand-rolled parser (string literal, VALUE(expr), or bare identifier)
3. If `Kind::From`, advance and parse expression (publisher handle)
4. If `Kind::LeftParen`, parse argument list using shared RUN argument pattern (`RunArgument` with direction + expression)
5. Expect period

**Note:** PUBLISH has implicit NO-ERROR behavior per ABL docs — no `no_error` field needed.

#### 3c. SUBSCRIBE Parsing

```
SUBSCRIBE [PROCEDURE subscriber-handle] [TO] event-name {IN handle | ANYWHERE}
  [RUN-PROCEDURE handler-name] [NO-ERROR].
```

Implementation outline:
1. Advance past `Kind::Subscribe`
2. If `Kind::Procedure`, advance and parse expression (subscriber handle)
3. If `Kind::To`, advance (optional noise word)
4. Parse event name (hand-rolled parser — same as PUBLISH)
5. **Required:** If `Kind::In`, advance and parse expression (publisher handle) → `SubscribeTarget::InHandle`. If `Kind::Anywhere`, advance → `SubscribeTarget::Anywhere`. Otherwise, emit error: "Expected IN or ANYWHERE after event name in SUBSCRIBE" (confirmed: omitting both is a compile error per ABL docs)
6. If `Kind::RunProcedure`, advance and parse identifier (handler name) → `Option<Identifier>`
7. Parse optional NO-ERROR via `parse_no_error()`
8. Expect period

**Disambiguation note:** If the token after SUBSCRIBE is `Kind::Procedure`, it means the optional subscriber clause is present. This works because `PROCEDURE` is a reserved keyword and cannot be an event name. No lookahead ambiguity.

#### 3d. UNSUBSCRIBE Parsing

```
UNSUBSCRIBE [PROCEDURE subscriber-handle] [TO] {event-name | ALL} [IN publisher-handle].
```

Implementation outline:
1. Advance past `Kind::Unsubscribe`
2. If `Kind::Procedure`, advance and parse expression (subscriber handle)
3. If `Kind::To`, advance (optional noise word)
4. If `Kind::All`, advance and set `event_name = None`. Otherwise, parse event name expression.
5. If `Kind::In`, advance and parse expression (publisher handle)
6. Expect period

**Note:** UNSUBSCRIBE has implicit NO-ERROR behavior — no `no_error` field. ANYWHERE is NOT valid in UNSUBSCRIBE (confirmed against ABL reference). The `IN` clause is optional in UNSUBSCRIBE (unlike SUBSCRIBE where IN/ANYWHERE is required). To cancel an ANYWHERE subscription, simply omit the `IN` clause.

#### 3e. DEFINE EVENT Parsing

Extend `parse_define_statement()` to handle `Kind::Event` after the access modifier, STATIC, and ABSTRACT checks.

```
DEFINE [access] [STATIC] [ABSTRACT] EVENT event-name SIGNATURE VOID (params...).
```

Implementation outline:
1. In `parse_define_statement()`, after parsing access modifier and STATIC flag, also check for `Kind::Abstract` (advance if present, set `is_abstract = true`)
2. Add `Kind::Event` check → `parse_define_event(access, is_static, is_abstract)`
3. In `parse_define_event()`:
   - Advance past `Kind::Event`
   - Parse identifier (event name)
   - Expect `Kind::Signature`
   - Expect `Kind::Void`
   - Expect `Kind::LeftParen`
   - Parse parameter list: loop of `[direction] name AS data-type`, comma-separated, producing `DefineParameter` statements with `no_undo: false`
   - Expect `Kind::RightParen`
   - Expect period

**Update error message** on the default branch of `parse_define_statement()` (line ~319) to include "EVENT" in the expected keyword list.

**ABSTRACT pass-through behavior:** The `is_abstract` flag is parsed but only semantically meaningful for DEFINE EVENT. For all other DEFINE subtypes (PROPERTY, VARIABLE, TEMP-TABLE, etc.), `is_abstract` is silently ignored — consistent with how `access` and `is_static` are already silently dropped for VARIABLE/TEMP-TABLE (noted at lines 290-292). Validation that abstract events only appear in abstract classes/interfaces is deferred to semantic analysis.

**Strict ordering:** ABL syntax specifies STATIC before ABSTRACT. The plan follows strict order (access → STATIC → ABSTRACT → subtype), consistent with how `parse_define_statement` already handles access → STATIC. This differs from `parse_method()` which uses a flexible-order loop, but strict ordering is appropriate for DEFINE since only EVENT uses ABSTRACT here.

#### 3f. `can_be_identifier()` Updates

**File:** `crates/oxabl_parser/src/parser/mod.rs` (line ~177)

Add new unreserved keywords so they can still be used as identifiers in other contexts (ABL allows this):
```rust
Kind::Publish | Kind::Subscribe | Kind::Unsubscribe | Kind::Anywhere |
Kind::Event | Kind::Signature | Kind::RunProcedure
```

Without this, `DEFINE VARIABLE subscribe AS INTEGER.` would fail.

#### 3g. Interface and Class Body Updates

**No changes needed.** `parse_interface()` (line 1834) delegates to `self.parse_statement()` in a loop, which already dispatches `Kind::Define` to `parse_define_statement()`. Once the DEFINE EVENT path is added there, `DEFINE EVENT` will automatically work inside interface bodies (and class bodies, which use the same pattern). Confirmed by architecture and system design reviewers.

### Phase 4: Tests

**File:** `crates/oxabl_parser/src/parser/tests.rs`

Follow the established test pattern: create `source`, tokenize, create parser, call `parse_statement()`, match on the expected variant, assert fields. Use `..` for unasserted fields.

#### PUBLISH Tests (4 tests)
- `parse_publish_string_literal` — `PUBLISH "NewCustomer".`
- `parse_publish_with_from` — `PUBLISH "NewCustomer" FROM hProc.`
- `parse_publish_with_params` — `PUBLISH "NewCustomer" (INPUT cName).`
- `parse_publish_expression_event` — `PUBLISH cEventName.`

#### SUBSCRIBE Tests (5 tests)
- `parse_subscribe_anywhere` — `SUBSCRIBE TO "NewCustomer" ANYWHERE.`
- `parse_subscribe_in_handle` — `SUBSCRIBE TO "NewCustomer" IN hPub.`
- `parse_subscribe_with_procedure` — `SUBSCRIBE PROCEDURE hSub TO "NewCustomer" IN hPub.`
- `parse_subscribe_with_run_procedure` — `SUBSCRIBE TO "NewCustomer" IN hPub RUN-PROCEDURE "MyHandler".`
- `parse_subscribe_no_to` — `SUBSCRIBE "NewCustomer" ANYWHERE.` (TO is optional)

#### UNSUBSCRIBE Tests (4 tests)
- `parse_unsubscribe_event` — `UNSUBSCRIBE TO "NewCustomer".`
- `parse_unsubscribe_all` — `UNSUBSCRIBE TO ALL.`
- `parse_unsubscribe_with_procedure` — `UNSUBSCRIBE PROCEDURE hSub TO "NewCustomer".`
- `parse_unsubscribe_with_in_handle` — `UNSUBSCRIBE TO "NewCustomer" IN hPub.`

#### DEFINE EVENT Tests (3 tests)
- `parse_define_event_minimal` — `DEFINE EVENT MyEvent SIGNATURE VOID ().`
- `parse_define_event_abstract` — `DEFINE PROTECTED ABSTRACT EVENT MyEvent SIGNATURE VOID ().`
- `parse_define_event_multiple_params` — `DEFINE PUBLIC EVENT MyEvent SIGNATURE VOID (INPUT p1 AS INTEGER, INPUT p2 AS CHARACTER, OUTPUT p3 AS LOGICAL).`

#### Integration Tests (2 tests)
- `parse_publish_in_do_block` — PUBLISH inside a DO block
- `parse_define_event_in_interface` — DEFINE EVENT inside an INTERFACE body (validates no interface changes needed)

#### Negative Test (1 test)
- `parse_publish_event_name_not_function_call` — `PUBLISH myEvent (INPUT x).` — verify `myEvent` is the event name and `(INPUT x)` is parsed as PUBLISH arguments, NOT as `myEvent(INPUT x)` function call

### Research Insights: Test Optimization

Reduced from 27 to 19 tests based on simplicity review. Tests cut:
- **Combination tests** (`parse_publish_with_from_and_params`, `parse_subscribe_full`, `parse_unsubscribe_full`) — if individual clauses work, their combination is mechanical
- **Already-covered tests** (`parse_publish_with_output_param` — OUTPUT direction tested by RUN tests; `parse_subscribe_with_no_error` — NO-ERROR tested elsewhere; `parse_unsubscribe_no_to` — same as SUBSCRIBE's no_to)
- **Modifier tests** (`parse_define_event_public`, `parse_define_event_static`) — access modifiers and STATIC are parsed by shared `parse_define_statement()` dispatch, already covered by Property/Method tests

Added negative test for the critical event-name parsing ambiguity.

### Phase 5: Update CLAUDE.md and Benchmarks

1. **CLAUDE.md** — Update the parser capabilities list:
   - Add PUBLISH/SUBSCRIBE/UNSUBSCRIBE and DEFINE EVENT to the "Statement parsing" bullet
   - Remove "PUBLISH/SUBSCRIBE" from the "Not yet implemented" list

2. **Benchmark fixtures** — Add PUBLISH/SUBSCRIBE/DEFINE EVENT examples to the parser benchmark fixture file (if one exists) so CodSpeed catches regressions. Event parsing is not a hot path, so this is lower priority — defer if no fixture file is trivially appendable.

## Technical Considerations

### Event-Name Expression Parsing (Critical)

The most important implementation detail: event names in PUBLISH/SUBSCRIBE/UNSUBSCRIBE must be parsed with a **hand-rolled event-name parser**, NOT `parse_primary()` and NOT `parse_expression()`.

- `parse_expression()` would greedily consume `(...)` as a function call
- `parse_primary()` ALSO promotes `identifier(` to function calls (line 396-398 of expressions.rs) — using it would produce the exact misparsing we're trying to avoid
- The RUN statement (`parse_run_statement()`, lines 1195-1214) faces the same problem and solves it with explicit three-way checks: VALUE(expr), string literal, or bare name

**ABL docs say event names are "a character expression"**, which technically includes function calls. However, parsing a full expression here is impractical due to the `(` ambiguity with PUBLISH's argument list. The hand-rolled approach covers the common cases (string literals, identifiers, VALUE(expr)) and matches the RUN statement precedent. If a user writes `PUBLISH funcCall()`, the parser will treat `funcCall` as the event name and `()` as an empty argument list — semantically different but syntactically unambiguous. This is the same tradeoff the RUN statement makes.

### Type Reuse

- PUBLISH arguments → `Vec<RunArgument>` (existing type)
- DEFINE EVENT signature params → `Vec<Statement>` with `DefineParameter` variants (matching Method pattern, `no_undo: false`)
- Parameter direction → `ParameterDirection` (existing enum)
- Access modifiers → `AccessModifier` (existing enum)
- Handler names → `Identifier` (existing type, preserves span)

### Known Gaps (Deferred)

- **DELEGATE form**: `DEFINE EVENT OnClick DELEGATE System.EventHandler.` — deferred per brainstorm decision
- **Semantic validation**: Cross-reference checking (Publish args match DEFINE EVENT signature) deferred to future semantic analysis pass
- **ON triggers**: Related event mechanism, tracked separately
- **Full expression event names**: Event names like `funcCall()` would be misparsed as event `funcCall` + empty argument list due to `(` ambiguity — documented limitation, matches RUN statement behavior

## Acceptance Criteria

- [x] `cargo run -p oxabl_codegen` generates Kind variants for all new keywords
- [x] PUBLISH parses all syntax variants (string event, expression event, FROM, parameters)
- [x] SUBSCRIBE parses all variants (PROCEDURE, TO, IN handle, ANYWHERE, RUN-PROCEDURE, NO-ERROR)
- [x] UNSUBSCRIBE parses all variants (event name, ALL, PROCEDURE, IN handle)
- [x] DEFINE EVENT parses with access modifiers, STATIC, ABSTRACT, and SIGNATURE VOID parameters
- [x] DEFINE EVENT works inside CLASS and INTERFACE bodies (no special-casing needed)
- [x] New keywords work as identifiers in non-event contexts (`can_be_identifier`)
- [x] Negative test confirms event name is not consumed as function call
- [x] All tests pass (`cargo test -p oxabl_parser`)
- [x] `cargo clippy` and `cargo fmt` pass
- [x] CLAUDE.md updated to reflect new capabilities

## Dependencies & Risks

- **Codegen dependency**: Keywords must be generated before parser work begins. Mechanical and low-risk.
- **ABSTRACT in DEFINE**: Currently `parse_define_statement()` checks for STATIC but not ABSTRACT. Need to add ABSTRACT parsing to the define dispatch — small scope increase (5 lines) but necessary for DEFINE EVENT. The `is_abstract` flag is silently ignored for non-EVENT define subtypes, consistent with existing behavior.
- **Interface body parsing**: Confirmed — NO changes needed. `parse_interface()` already delegates to `parse_statement()`.
- **Shared argument parsing**: Consider extracting RUN's parenthesized argument loop into a shared helper to avoid code duplication in PUBLISH parsing. This is optional but reduces drift risk.

## Sources & References

- **Origin brainstorm:** [docs/brainstorms/2026-04-09-publish-subscribe-brainstorm.md](docs/brainstorms/2026-04-09-publish-subscribe-brainstorm.md) — Key decisions: unified AST approach, parse-fully-validate-later, DELEGATE deferred, full UNSUBSCRIBE included
- **ABL docs:** https://docs.progress.com/bundle/openedge-develop-abl-applications/page/PUBLISHSUBSCRIBE-example.html
- **ABL reference (PUBLISH):** https://docs-be.progress.com/bundle/abl-reference/page/PUBLISH-statement.html
- **ABL reference (SUBSCRIBE):** https://docs-be.progress.com/bundle/abl-reference/page/SUBSCRIBE-statement.html
- **ABL reference (UNSUBSCRIBE):** https://docs-be.progress.com/bundle/abl-reference/page/UNSUBSCRIBE-statement.html
- **Similar patterns:** `crates/oxabl_parser/src/parser/statements.rs:1195` (RUN statement parsing), `crates/oxabl_ast/src/statement.rs:117` (RUN AST node), `crates/oxabl_ast/src/statement.rs:504` (ParameterDirection)
- **Institutional learning:** `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md` — always dispatch on Kind variants, never string comparison
