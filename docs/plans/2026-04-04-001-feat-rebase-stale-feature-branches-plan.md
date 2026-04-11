---
title: "Rebase Three Stale Feature Branches onto Master"
type: feat
status: completed
date: 2026-04-04
---

# Rebase Three Stale Feature Branches onto Master

## Overview

Three feature branches (`feat/parser-error-recovery`, `feat/catch-finally`, `feat/assign-and-function`) all diverged from commit `a24e213`, which is 40 commits behind master. Master has since added: RUN statements, DISPLAY/MESSAGE, TEMP-TABLE/BUFFER support, CASE/FOR EACH/FIND, PROCEDURE definitions, and formatting changes. All three branches need rebasing onto current master in a specific order to minimize conflicts and maximize coherence.

## Problem Statement

The branches are stale and cannot be merged cleanly. Master's parser has grown significantly (tests.rs went from 3,112 to 4,293 lines; statements.rs grew to 1,433 lines with a `can_start_statement()` function that's directly relevant to error recovery). Rebasing in the wrong order would create unnecessary rework — error recovery should land first since the other two branches add statement types that benefit from it.

## Proposed Solution

Rebase in three phases, sequentially:

1. **`feat/parser-error-recovery`** → onto master
2. **`feat/catch-finally`** → onto master (after phase 1 is merged)
3. **`feat/assign-and-function`** → onto master (after phase 2 is merged)

## Phase 1: `feat/parser-error-recovery` (1 commit, 167 lines)

**Branch commit:** `1e9e623` — adds `Program` type, `parse_program()`, `synchronize()`, and 6 tests.

### Files changed and expected conflicts

| File | Conflict? | Resolution |
|------|-----------|------------|
| `crates/oxabl_parser/src/lib.rs` | **Yes** — branch adds `Program` to the `pub use` line which hasn't changed on master | Trivial: add `, Program` to master's existing export line |
| `crates/oxabl_parser/src/parser/mod.rs` | **Yes** — branch adds `Statement` to imports (master still imports `DataType, Identifier, Span`), adds `Program` struct + `parse_program()` + `synchronize()` methods | Moderate: (1) Add `Statement` to master's import line. (2) Insert `Program` struct after `ParseResult` type alias. (3) Insert `parse_program()` and `synchronize()` methods into `impl Parser`. The `synchronize()` keyword list should be updated to include master's new keywords: `Kind::Find`, `Kind::Case`, `Kind::Procedure`, `Kind::Display`, `Kind::Message` — and master already has a `can_start_statement()` in statements.rs that should be consulted for alignment. |
| `crates/oxabl_parser/src/parser/tests.rs` | **Yes** — branch appends tests at line 3,112 (old EOF); master's EOF is now line 4,293 | Easy: the branch's tests append at the old EOF. After rebase, they'll go at the new EOF (line 4,293+). No semantic conflict, just position. |

### Key decisions during rebase

1. **`synchronize()` keyword set must match `can_start_statement()`** — Master already defines `can_start_statement()` in `statements.rs:20-36` with the full set of statement-starting keywords. The branch's `synchronize()` should either call `can_start_statement()` or mirror its match arms exactly. Recommendation: have `synchronize()` call `can_start_statement()` to avoid drift.

2. **`Program` struct stays in `oxabl_parser`** — The branch puts `Program` in `parser/mod.rs` (not `oxabl_ast`). This is fine for now; the existing plan noted option (b) as viable.

3. **Tests use `parse_program()`** — The branch adds new tests that use `parse_program()`. Existing tests stay using `parse_statement()` — no migration needed.

### Steps

```bash
# 1. Create local branch from remote
git checkout -b feat/parser-error-recovery origin/feat/parser-error-recovery

# 2. Rebase onto master
git rebase master

# 3. Resolve conflicts in each file (see table above)

# 4. Update synchronize() to use can_start_statement() from statements.rs
#    or ensure keyword set matches

# 5. Run tests
cargo test -p oxabl_parser

# 6. Run CI checks locally
cargo check && cargo clippy -D warnings && cargo fmt --check

# 7. Force-push rebased branch
git push --force-with-lease origin feat/parser-error-recovery

# 8. Merge PR (or create one if needed)
```

### Post-merge verification

- [ ] `cargo test` passes (all 4,293+ lines of tests)
- [ ] `parse_program()` correctly recovers past errors on master's new statement types
- [ ] `Program` is exported from `oxabl_parser`

---

## Phase 2: `feat/catch-finally` (1 commit, 241 lines)

**Branch commit:** `dc46b2d` — adds CATCH, FINALLY, THROW statement types and parsing.

### Files changed and expected conflicts

| File | Conflict? | Resolution |
|------|-----------|------------|
| `crates/oxabl_ast/src/statement.rs` | **Yes** — branch inserts `Catch`, `Finally`, `Throw` variants after the `Run` statement variant (line ~101 on base). On master, this position has shifted due to new statement types (TEMP-TABLE, BUFFER, DISPLAY, MESSAGE, CASE, FIND, ForEach, Procedure are all new). | Moderate: insert the three new variants at the appropriate position in master's enum (after the RUN-related variants, before Leave). |
| `crates/oxabl_parser/src/parser/statements.rs` | **Yes** — branch adds CATCH/FINALLY/THROW parsing. Master has rewritten statements.rs significantly (1,433 lines with new dispatch arms). | Moderate: the branch adds ~93 lines of new parsing functions. These need to be inserted into master's `parse_statement()` dispatch and potentially into block-parsing code (DO blocks should accept CATCH/FINALLY clauses). |
| `crates/oxabl_parser/src/parser/tests.rs` | **Same as Phase 1** — append-at-EOF conflict, resolved by moving tests to new EOF. |

### Key decisions during rebase

1. **CATCH/FINALLY attach to DO blocks** — Master now has a more complex DO block parser. The branch's CATCH/FINALLY parsing needs to integrate with master's DO block code, not just append as standalone statements.

2. **Update `can_start_statement()`** — Add `Kind::Catch`, `Kind::Finally` (or equivalent) if they exist as lexer kinds. If not, check whether CATCH/FINALLY/THROW are lexed as identifiers that need text matching.

3. **Error recovery should handle CATCH/FINALLY** — After Phase 1, `synchronize()` exists. Consider whether CATCH/FINALLY should be sync points.

### Steps

```bash
# 1. After Phase 1 is merged to master, pull master
git checkout master && git pull

# 2. Create local branch
git checkout -b feat/catch-finally origin/feat/catch-finally

# 3. Rebase onto master
git rebase master

# 4. Resolve conflicts (see table)
# 5. Integrate CATCH/FINALLY into DO block parsing
# 6. Verify lexer has Kind variants for CATCH/FINALLY/THROW (check Kind enum)

# 7. Run tests
cargo test -p oxabl_parser

# 8. CI checks
cargo check && cargo clippy -D warnings && cargo fmt --check

# 9. Force-push and merge
git push --force-with-lease origin feat/catch-finally
```

---

## Phase 3: `feat/assign-and-function` (1 commit, 292 lines)

**Branch commit:** `0a3617d` — adds ASSIGN statement, FUNCTION definition, and `AssignPair` type.

### Files changed and expected conflicts

| File | Conflict? | Resolution |
|------|-----------|------------|
| `crates/oxabl_ast/src/statement.rs` | **Yes** — branch inserts `Assign` and `Function` variants + `AssignPair` struct. Same conflict zone as Phase 2. After Phase 2 merge, the insertion point will have CATCH/FINALLY/THROW immediately before it. | Moderate: insert `Assign` and `Function` variants, add `AssignPair` struct at bottom of file. |
| `crates/oxabl_parser/src/parser/statements.rs` | **Yes** — branch adds ~107 lines of ASSIGN/FUNCTION parsing. On master + Phase 2, the dispatch chain has grown significantly. | Moderate: add ASSIGN dispatch (keyword-based) and FUNCTION dispatch. The branch modifies 9 existing lines (likely in the dispatch chain) which will conflict. |
| `crates/oxabl_parser/src/parser/tests.rs` | **Same pattern** — append at EOF. |

### Key decisions during rebase

1. **ASSIGN vs Assignment** — Master already has `Statement::Assignment` for single `x = 1.` syntax. The branch adds `Statement::Assign` for multi-target `ASSIGN x = 1 y = 2.` syntax. These are distinct ABL constructs and should coexist.

2. **FUNCTION parameters** — The branch parses FUNCTION body as statements (parameters as DEFINE PARAMETER). Verify this works with master's expanded DEFINE parsing.

3. **Update `can_start_statement()`** — Add `Kind::Assign` and `Kind::Function` (or equivalent) to the statement-starting keyword set.

### Steps

```bash
# 1. After Phase 2 is merged, pull master
git checkout master && git pull

# 2. Create local branch
git checkout -b feat/assign-and-function origin/feat/assign-and-function

# 3. Rebase onto master
git rebase master

# 4. Resolve conflicts
# 5. Ensure AssignPair struct doesn't conflict with existing types
# 6. Add ASSIGN/FUNCTION to can_start_statement() and synchronize() sync set

# 7. Run tests
cargo test -p oxabl_parser

# 8. CI checks
cargo check && cargo clippy -D warnings && cargo fmt --check

# 9. Force-push and merge
git push --force-with-lease origin feat/assign-and-function
```

---

## Acceptance Criteria

- [ ] All three branches are rebased onto master with no merge commits
- [ ] `cargo test` passes after each phase (no regressions)
- [ ] `cargo clippy -D warnings` and `cargo fmt --check` pass after each phase
- [ ] `synchronize()` keyword set stays aligned with `can_start_statement()`
- [ ] New statement types (CATCH/FINALLY/THROW, ASSIGN, FUNCTION) work with error recovery
- [ ] No duplicate or conflicting AST types

## Risks

- **CATCH/FINALLY integration with DO blocks** — The branch may have been written against the simpler DO block parser. Master's DO block parser is more complex (loop variables, WHILE conditions). CATCH/FINALLY clause integration may need rethinking, not just mechanical conflict resolution.
- **Lexer Kind variants** — If CATCH/FINALLY/THROW/ASSIGN/FUNCTION aren't in the Kind enum on master, they'll need to be added (via codegen or manually). Check `resources/keyword_overrides.toml` and the codegen pipeline.
- **Test assumptions** — Branch tests assume the old parser state. Tests that check exact error messages or specific parse behavior may need updating after rebase if master changed how certain constructs are parsed.

## Sources

- **Existing plan:** [docs/plans/2026-03-31-feat-parser-error-recovery-plan.md](docs/plans/2026-03-31-feat-parser-error-recovery-plan.md) — detailed design for error recovery (Phase 1)
- **`can_start_statement()`:** `crates/oxabl_parser/src/parser/statements.rs:20-36` — master's existing statement-boundary function
- **Branch base:** commit `a24e213` (40 commits behind master as of 2026-04-04)
