---
title: "Parser Completion: RUN, DISPLAY, MESSAGE & Docs"
type: feat
status: active
date: 2026-03-31
deepened: 2026-03-31
---

# Parser Completion: RUN, DISPLAY, MESSAGE & Docs

## Enhancement Summary

**Deepened on:** 2026-03-31
**Agents used:** Architecture Strategist, Performance Oracle, Pattern Recognition, Code Simplicity, Spec Flow Analyzer, Best Practices Researcher

### Key Improvements
1. Added **Phase 0** — restore green build, fix typo rename, remove all debug printlns, wire RUN into dispatch
2. Extended **RUN AST** with `in_handle` and `no_error` fields (too common in production ABL to skip)
3. **Simplified DISPLAY/MESSAGE** — merged into one branch, kept `DisplayItem` minimal (expression + optional WHEN condition for variable detection)
4. Added **`.p` extension ambiguity** handling to `parse_procedure_name`
5. Added research insights on spans, snapshot testing, and Expression refactoring

### New Considerations Discovered
- `Statement` nodes have no `Span` — high risk for formatter/linter. Should be addressed before or during this work.
- `parse_run_statement` is defined but **never called** from `parse_statement` dispatch
- `finish_expression()` in `statements.rs` duplicates precedence logic from `expressions.rs` — fragile coupling
- The `Expression` enum has 14 binary op variants that could collapse into `Binary(BinaryOp, Box, Box)` — consider for a future refactor

---

## Current State

### What's Built
- **Lexer**: MVP complete, benchmarked at ~11 MiB/s
- **AST**: Covers all current statement types including `Run`, `RunTarget`, `RunArgument`
- **Parser** (142 tests): Expressions, `DEFINE VARIABLE`, `VAR`, assignments, `DO` (counting loops), `IF/THEN/ELSE`, `REPEAT`, `FOR EACH`, `FIND`, `CASE`, `PROCEDURE`, `DEFINE PARAMETER`, `LEAVE`, `NEXT`, `RETURN`

### What's Broken / In-Progress
- `parse_run_statement()` has a `match.self` typo on `statements.rs:766` (should be `match self`)
- `parse_run_statement()` is **never called** from `parse_statement` — no `Kind::Run` dispatch branch exists
- `parse_procedure_name()` is `todo!()` at `statements.rs:831`
- Unused imports: `std::fmt::Arguments`, `RunArgument`
- ~15 debug `println!` calls across `statements.rs` and `expressions.rs` (lines 14, 93, 98, 423, 424, 445, 459, 579, 581 in statements; lines 58, 81, 85, 89, 91, 329-330 in expressions)
- Error message on line 789 says "Expected ','" but should say "Expected '.'"
- Comment typos: "exist" → "exit" (line 104), "intial" → "initial" (line 203), "collor" → "colon" (line 530)

### What's Stale
- **README.md**: Says "91 tests" (actually 142), missing CASE/FIND/FOR EACH/PROCEDURE from feature list
- **NEXT_STEPS.md**: Checklist shows FIND/CASE/PROCEDURE as "needed" (all implemented), codegen section says files need manual redirect (they don't anymore per CLAUDE.md)

---

## Execution Plan

### Phase 0: Restore Green Build (main branch, before anything else)

The project does not compile on `master`. This must be fixed first.

- [x] Rename `ParamterDirection` → `ParameterDirection` across workspace (3 files, 6 occurrences: `statement.rs`, `statements.rs`, `tests.rs`)
- [x] Rename `DefineParamter` → `DefineParameter` in `statement.rs` and all references
- [x] Fix `match.self` → `match self` on `statements.rs:766`
- [x] Fix `ParameterDirection` reference in `parse_run_statement` (now correct after rename)
- [x] Remove **all** `println!` debug statements from `statements.rs` and `expressions.rs` (~15 calls)
- [x] Remove unused import `std::fmt::Arguments` on `statements.rs:3`
- [x] Fix error message `statements.rs:789`: "Expected ','" → "Expected '.'"
- [x] Fix comment typos (exit, initial, colon)
- [x] Add `Kind::Run` dispatch branch in `parse_statement()` to wire up `parse_run_statement`
- [x] Verify `cargo check --workspace` passes

### Research Insights (Phase 0)

**Why fix the typo now (not later):** The broken `parse_run_statement` already references the correct spelling `ParameterDirection`. Every phase in this plan touches code that imports this type. The rename is 6 occurrences across 3 files — the cost only grows with time. (Architecture Strategist, Pattern Recognition)

**Why remove printlns now:** These are I/O syscalls in the parser's hottest loops. The Performance Oracle estimates they could reduce throughput by 10-50x. They also produce noise in test output. If runtime diagnostics are needed later, use `tracing` with compile-time level filtering.

---

### Phase 1: Documentation Cleanup (main branch)

#### 1.1 Update README.md
- [x] Fix test count: 91 → 147
- [x] Add CASE, FIND, FOR EACH, PROCEDURE to parser feature list
- [x] Update "Current Work" to reflect RUN in-progress, DISPLAY/MESSAGE next
- [x] Fix codegen section — commands now write directly to target locations per CLAUDE.md
- [ ] Verify benchmark numbers are still accurate (or note they haven't been re-run)

#### 1.2 Move NEXT_STEPS.md to docs/plans/
- [x] Move `NEXT_STEPS.md` → `docs/plans/2026-03-31-parser-next-steps.md`
- [x] Update the checklist to reflect actual state (FIND/CASE/PROCEDURE are done)
- [x] Mark RUN as "in progress"
- [x] Keep DISPLAY, MESSAGE, OO-ABL, Error Handling, Temp-Table sections as-is (still future work)
- [x] Remove old `NEXT_STEPS.md` from repo root

#### 1.3 Cargo Doc Pass
- [ ] Add `#![warn(missing_docs)]` to `oxabl_ast` and `oxabl_parser` crate roots (deferred)
- [x] `crates/oxabl_ast/src/lib.rs` — add module-level `//!` doc with overview of AST structure
- [x] `crates/oxabl_ast/src/statement.rs` — ensure each `Statement` variant has a doc comment. Added docs to `ParameterDirection`, `RunTarget`, `RunArgument`, `WhenBranch`
- [x] `crates/oxabl_ast/src/expression.rs` — doc comments on `Expression` variants and `Identifier`
- [x] `crates/oxabl_ast/src/literal.rs` — doc comments on literal types
- [x] `crates/oxabl_ast/src/span.rs` — doc comment on `Span`
- [x] `crates/oxabl_parser/src/parser/mod.rs` — module doc explaining parser architecture, `ParseError`, `Parser`
- [x] `crates/oxabl_parser/src/parser/statements.rs` — doc comment listing all supported statement types
- [x] `crates/oxabl_parser/src/parser/expressions.rs` — doc comment explaining precedence levels
- [x] `crates/oxabl_parser/src/lib.rs` — crate-level doc
- [x] `crates/oxabl_lexer/src/lib.rs` — verified adequate
- [x] `crates/oxabl_common/src/source_map.rs` — verified adequate
- [ ] Use inter-item linking between types in rustdoc (deferred)
- [x] Run `cargo doc --no-deps --workspace` to verify docs build cleanly
- [x] Fix any doc warnings

**Execution note:** Phase 1 tasks (1.1, 1.2, 1.3) can be run as parallel sub-agents since they touch different files. Commit results to `main` before creating worktrees.

### Research Insights (Phase 1)

**Cargo doc best practices (Best Practices Researcher):**
- Every public item needs a one-line summary + `# Examples` section
- Functions returning `Result` should have `# Errors` section
- Functions that can panic should have `# Panics` section
- Doc examples are compiled and run by `cargo test` — free regression tests
- Use `#` to hide boilerplate lines in doc examples

**`Span::from_token` helper (Pattern Recognition):**
The pattern `Span { start: token.start as u32, end: token.end as u32 }` appears 14+ times across the parser. A `From` impl centralizes the `usize → u32` cast assumption and eliminates repetition.

---

### Phase 2: Finish RUN Statement (worktree)

**Worktree:** `~/code/oxabl-worktrees/feat/RUN-STATEMENT-PARSING`
**Branch:** `feat/run-statement-parsing`

#### 2.1 Extend RUN AST

The current `Statement::Run` only has `target` and `arguments`. Two additional fields are needed because they are extremely common in production ABL and reference variables a linter must track:

- [ ] Add `in_handle: Option<Expression>` — for `RUN myProc IN hServer.` (persistent procedure handles)
- [ ] Add `no_error: bool` — for `RUN myProc NO-ERROR.`
- [ ] Change `RunTarget::Literal(String)` → `RunTarget::Literal(Identifier)` to preserve source span

```rust
Run {
    target: RunTarget,
    arguments: Vec<RunArgument>,
    in_handle: Option<Expression>,  // RUN ... IN handle
    no_error: bool,                 // RUN ... NO-ERROR
}
```

Deferred (uncommon, can be added later): `ON SERVER`, `PERSISTENT SET`, `ASYNCHRONOUS`, `SINGLE-RUN`.

#### 2.2 Implement `parse_procedure_name()`

This is the trickiest part of RUN parsing due to the `.p` extension vs statement-terminating period ambiguity.

- [ ] Parse identifiers that may contain hyphens (lexer already supports hyphenated identifiers)
- [ ] Handle dotted names: after consuming a `.`, peek ahead — if next token is an identifier matching a known ABL file extension (`p`, `w`, `r`, `i`, `cls`), treat it as part of the name; otherwise, it's the statement terminator
- [ ] Handle string literal targets: `RUN "my-proc.p".` — if current token is a string literal, use it directly (no ambiguity)
- [ ] Return an `Identifier` (not `String`) to preserve span

#### 2.3 Complete RUN Parser Logic
- [ ] Parse `IN handle` clause after arguments: `if self.check(Kind::KwIn) { ... }`
- [ ] Parse `NO-ERROR` flag: `if self.check(Kind::NoError) { ... }`
- [ ] Verify `RunTarget::Literal` path works for simple names (`RUN calculate-total.`)
- [ ] Verify `RunTarget::Dynamic` path works for `VALUE(expr)` syntax
- [ ] Verify argument parsing with `INPUT`, `OUTPUT`, `INPUT-OUTPUT` directions
- [ ] Handle RUN with no arguments (`RUN my-proc.`)
- [ ] Handle RUN with dotted names (`RUN external-prog.p`)
- [ ] Ensure RUN with no arguments works correctly (`RUN myProc.` — no parens allowed)

#### 2.4 Add RUN Tests
- [ ] `RUN simple-proc.` — no args
- [ ] `RUN calculate-total (INPUT 100, INPUT 5, OUTPUT result).` — mixed directions
- [ ] `RUN VALUE(procName).` — dynamic dispatch
- [ ] `RUN external-prog.p (INPUT "data").` — dotted filename with args
- [ ] `RUN my-proc.` — hyphenated name
- [ ] `RUN some-proc (INPUT 1 + 2, OUTPUT x).` — expressions as args
- [ ] `RUN "my-proc.p".` — string literal target
- [ ] `RUN myProc IN hServer.` — IN handle
- [ ] `RUN myProc NO-ERROR.` — no-error flag
- [ ] `RUN myProc (OUTPUT result) NO-ERROR.` — args + no-error
- [ ] Error case: missing period after RUN

#### 2.5 Validate
- [ ] `cargo test -p oxabl_parser` — all tests pass
- [ ] `cargo test` — full workspace passes
- [ ] `cargo check --workspace` — no warnings

### Research Insights (Phase 2)

**`.p` extension ambiguity (Spec Flow Analyzer):**
In `RUN calculate-total.p.` — the first period is part of the filename, the second terminates the statement. ABL resolves this by treating known extensions (`.p`, `.w`, `.r`, `.i`, `.cls`) specially. The safest approach: check for known extensions after the dot, and require quotes for non-standard extensions. This covers 99% of production usage.

**`RUN IN handle` is critical (Spec Flow Analyzer):**
Any ABL codebase using persistent procedures or application servers uses `RUN ... IN handle`. A linter that cannot parse this will fail on a large percentage of production code. The `in_handle` field is `Option<Expression>` because the handle is a variable reference the linter must track.

**`RunTarget::Literal` should use `Identifier` not `String` (Pattern Recognition):**
A procedure name has a source location. Discarding the span (as `String` does) means error messages and future tooling (go-to-definition) cannot point back to it. Same issue exists with `DataType::Class(String)` — fix that too when convenient.

---

### Phase 2.5: Lexer Keyword Verification (before DISPLAY/MESSAGE)

Before implementing DISPLAY or MESSAGE, verify which `Kind` variants already exist and which need to be added via codegen.

- [ ] Grep `kind.rs` for: `Display`, `Message`, `ViewAs`, `AlertBox`, `Skip`, `Buttons`, `Frame`, `Except`, `Columns`, `Down`, `Update`, `Format`, `With`, `Set`
- [ ] For any missing keywords, add them to `resources/keyword_overrides.toml` and run `cargo run -p oxabl_codegen`
- [ ] Verify `cargo check -p oxabl_lexer` passes after additions

### Research Insights (Phase 2.5)

**Keyword ambiguity (Spec Flow Analyzer):**
Several keywords serve dual purposes — `ERROR` is both an alert-box type and a statement/expression keyword. `IN` appears in RUN, FOR EACH, and other contexts. These must be resolved contextually in the parser, not at the lexer level. The lexer should emit the same `Kind` variant regardless of context.

---

### Phase 3: DISPLAY + MESSAGE Statements (single worktree)

**Worktree:** `~/code/oxabl-worktrees/feat/DISPLAY-MESSAGE-PARSING`
**Branch:** `feat/display-message-parsing`

**Goal:** Parse DISPLAY and MESSAGE statements well enough to identify variable/field references for linting. These follow the same "parse items until terminator" pattern, so they belong in one branch.

#### 3.1 DISPLAY AST (`oxabl_ast/src/statement.rs`)

```rust
Display {
    items: Vec<DisplayItem>,
    except: Vec<Identifier>,
    frame: Option<Identifier>,  // just the frame name
}
```

`DisplayItem` needs to be a struct (not just `Expression`) because per-item `WHEN` conditions contain variable references a linter must track:

```rust
pub struct DisplayItem {
    pub expression: Expression,
    pub when_condition: Option<Expression>,  // DISPLAY x WHEN avail.
}
```

Deferred from `DisplayItem`: FORMAT (string literal, no variable refs), LABEL (string literal), AT/COLUMN positioning, NO-LABEL, BGCOLOR/FGCOLOR.

Deferred from `Display`: frame modifiers (COLUMNS, DOWN, CENTERED, SIDE-LABELS). Just capture the frame name.

#### 3.2 DISPLAY Parser
- [ ] `parse_display_statement()` — consume DISPLAY, parse items until `WITH`, `EXCEPT`, or `.`
- [ ] For each item: parse expression, then check for `WHEN` and parse its condition
- [ ] Handle `EXCEPT field-list` — parse identifiers until next clause or terminator
- [ ] Handle `WITH FRAME name` — parse frame name, skip remaining frame options until `.`
- [ ] Handle bare `DISPLAY expr expr.` without frame
- [ ] Handle `DISPLAY STREAM streamName ...` — parse and skip stream prefix (captures stream var reference)

#### 3.3 DISPLAY Tests
- [ ] `DISPLAY Customer.Name Customer.Balance.`
- [ ] `DISPLAY "Total:" total WITH FRAME f1.`
- [ ] `DISPLAY x y z WITH FRAME results 2 COLUMNS.`
- [ ] `DISPLAY Customer EXCEPT CustNum WITH FRAME cust-frame.`
- [ ] `DISPLAY 1 + 2.` — expression as display item
- [ ] `DISPLAY x WHEN available.` — per-item WHEN condition

#### 3.4 MESSAGE AST (`oxabl_ast/src/statement.rs`)

```rust
Message {
    items: Vec<Expression>,
    set_targets: Vec<Identifier>,    // MESSAGE ... SET/UPDATE var1 var2.
}
```

For linting variable references, what matters is: (1) the expressions in the message body, and (2) the variables bound by SET/UPDATE. The VIEW-AS ALERT-BOX clause, alert type, and button type are all keywords/string-literals with no variable references — the parser should recognize and skip over them without failing, but they don't need AST representation.

SKIP and SKIP(n) are formatting directives with no variable references. The parser should recognize them (to avoid treating them as identifiers) but they don't need AST representation for linting.

#### 3.5 MESSAGE Parser
- [ ] `parse_message_statement()` — consume MESSAGE, parse expressions until `VIEW-AS`, `SET`, `UPDATE`, or `.`
- [ ] Recognize and skip `SKIP` / `SKIP(n)` tokens (don't treat as identifiers)
- [ ] Handle `VIEW-AS ALERT-BOX [type] [BUTTONS btn-type] [TITLE "str"]` — skip over without failing
- [ ] Handle `SET varlist` and `UPDATE varlist` — parse identifiers into `set_targets`
- [ ] Distinguish SET vs UPDATE if needed (both create variable bindings)

#### 3.6 MESSAGE Tests
- [ ] `MESSAGE "Hello, World!".`
- [ ] `MESSAGE "Error:" errMsg VIEW-AS ALERT-BOX ERROR.`
- [ ] `MESSAGE "Confirm?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice.`
- [ ] `MESSAGE Customer.Name SKIP Customer.Balance.`
- [ ] `MESSAGE "Line 1" SKIP(2) "Line 4".`
- [ ] `MESSAGE "Enter name:" UPDATE cName.` — UPDATE without VIEW-AS

#### 3.7 Validate
- [ ] `cargo test -p oxabl_parser` — all tests pass
- [ ] `cargo test` — full workspace passes
- [ ] `cargo check --workspace` — no warnings

### Research Insights (Phase 3)

**Why `DisplayItem` is a struct, not just `Expression` (Spec Flow vs Simplicity tension):**
The Simplicity Reviewer recommended `Vec<Expression>`. The Spec Flow Analyzer correctly noted that `DISPLAY x WHEN available.` has a per-item WHEN condition that contains a variable reference. A flat `Vec<Expression>` would miss it. Compromise: `DisplayItem` with just `expression` + `when_condition`. All other per-item modifiers (FORMAT, LABEL, AT) are string literals or positional data with no variable references — defer them.

**Why MESSAGE is simpler than DISPLAY (Simplicity Reviewer):**
For linting, MESSAGE needs only: (1) the body expressions, (2) the SET/UPDATE variable bindings. The AlertType/ButtonType enums are closed keyword sets that a linter will never inspect for variable references. The parser must *recognize* VIEW-AS ALERT-BOX syntax to avoid choking on it, but the AST doesn't need to model it. ~40 lines of enum definitions avoided.

**MESSAGE SET vs UPDATE (Spec Flow Analyzer):**
Both SET and UPDATE in MESSAGE create variable bindings. SET initializes the variable; UPDATE preserves its current value. Both matter equally to a linter — the variable is being written to either way. Model both as `set_targets`.

---

### Phase 4: Error Recovery (separate plan + worktree)

See [`docs/plans/2026-03-31-feat-parser-error-recovery-plan.md`](2026-03-31-feat-parser-error-recovery-plan.md) for the full plan. Executed in its own worktree after Phase 3 merges.

### Research Insights (Error Recovery)

**The matklad approach (Best Practices Researcher):**
rust-analyzer uses event-based parsing that decouples parsing from tree construction. This is the gold standard but a significant architectural change. A pragmatic intermediate: add recovery sets to `expect`-style functions, accumulate errors in a `Vec<ParseError>` on the Parser struct, and use ABL's `.` (period) as the natural synchronization point.

**Fuel/progress guarantee (Best Practices Researcher):**
Every loop must guarantee forward progress. Consider a "fuel" counter that decrements on lookahead, resets on `advance()`, and panics at 0. This turns infinite-loop bugs into immediate panics during development.

**Diagnostic location (Architecture Strategist):**
`ParseError` should stay in `oxabl_parser`. If a `Program` top-level node needs to live in `oxabl_ast`, define a generic diagnostic trait in `oxabl_common` to avoid the AST depending on the parser.

---

## Execution Strategy

| Phase | Dependencies | Parallelizable? | Notes |
|-------|-------------|-----------------|-------|
| 0 Green build | None | No | Fix compilation, rename typo, remove printlns |
| 1.1 README | Phase 0 | Yes (with 1.2, 1.3) | Sub-agent |
| 1.2 NEXT_STEPS move | Phase 0 | Yes (with 1.1, 1.3) | Sub-agent |
| 1.3 Cargo docs | Phase 0 | Yes (with 1.1, 1.2) | Sub-agent |
| 2 RUN statement | Phase 1 committed | No | Worktree at `~/code/oxabl-worktrees/feat/RUN-STATEMENT-PARSING` |
| 2.5 Lexer keywords | Phase 2 merged | No | Verify/add Kind variants for DISPLAY/MESSAGE |
| 3 DISPLAY + MESSAGE | Phase 2.5 done | No | Single worktree at `~/code/oxabl-worktrees/feat/DISPLAY-MESSAGE-PARSING` |
| 4 Error Recovery | Phase 3 merged | Independent | Separate plan + worktree |

## Acceptance Criteria

- [ ] README.md accurately reflects project state (test counts, features, codegen usage)
- [ ] NEXT_STEPS content lives in `docs/plans/` with accurate checklist
- [ ] `cargo doc --no-deps --workspace` builds without warnings and produces useful output
- [ ] RUN statement parses all documented syntax forms with tests, including IN handle and NO-ERROR
- [ ] DISPLAY statement parses with variable/field reference extraction (including WHEN conditions)
- [ ] MESSAGE statement parses with variable reference extraction (including SET/UPDATE targets)
- [ ] All existing tests continue to pass after each phase
- [ ] Error recovery plan exists as a separate document
- [ ] `ParamterDirection` typo is fixed across the workspace

## Success Metrics

- Parser test count increases by ~25-35 (RUN ~11, DISPLAY ~6, MESSAGE ~6, plus edge cases)
- `cargo doc` output is navigable and useful for understanding the crate APIs
- Zero compilation warnings across workspace after each phase

## Dependencies & Risks

- **Lexer keyword coverage:** DISPLAY, MESSAGE, VIEW-AS, ALERT-BOX, SKIP, BUTTONS, etc. need to be valid `Kind` variants. May need codegen additions if missing. Phase 2.5 addresses this explicitly.
- **`.p` extension ambiguity:** `RUN myproc.p.` — first period is filename, second is terminator. Handled by checking known ABL file extensions after the dot.
- **Keyword ambiguity:** `ERROR`, `IN`, `SET` serve dual purposes in different contexts. Parser resolves contextually.
- **Frame clause complexity:** DISPLAY's `WITH FRAME` has many modifiers. This plan captures only the frame name. Full frame semantics are deferred.

## Future Considerations (not in scope, but informed by research)

These items were surfaced by the research agents. They are **not part of this plan** but should be tracked:

- **Spans on all Statement nodes:** Currently `Statement` has no `Span`. A `Spanned<Statement>` wrapper or per-variant `span` field will be needed for the formatter. High risk to retrofit later. (Architecture Strategist)
- **Expression enum refactor:** Collapse 14 binary op variants into `Binary(BinaryOp, Box<Expression>, Box<Expression>)`. Keeps enum small, adds spans, prepares for arena allocation. (Best Practices Researcher — pattern from oxc, swc, rust-analyzer)
- **`Identifier.name` as `&'a str`:** Eliminates ~80K heap allocations per MiB of source. Most impactful single performance change. (Performance Oracle)
- **`insta` snapshot testing:** Replace verbose manual AST assertions with `assert_debug_snapshot!`. Makes adding tests trivial. (Best Practices Researcher)
- **`Vec::with_capacity()` hints:** Use `with_capacity(4)` for block bodies, argument lists. Trivial change, eliminates reallocations. (Performance Oracle)
- **`SmallVec` for argument lists:** Most calls have 1-3 args. `SmallVec<[Expression; 3]>` avoids heap allocation. (Performance Oracle)
- **Arena allocation:** Defer until AST stabilizes. The structural refactors above prepare for it. (Best Practices Researcher, Performance Oracle)
- **`DataType::Class(String)` → `DataType::Class(Identifier)`:** Preserve span. (Pattern Recognition)
