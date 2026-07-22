---
name: refine-oxabl-parser
description: Iteratively improve the oxabl ABL parser by identifying parse failures, locating the fix site, applying it, validating, and committing. Use when you want to run a fix loop against subdirectories of a local ABL test corpus.
argument-hint: "[optional: max iterations, default unlimited]"
allowed-tools: Bash, Read, Edit, Write, Glob, Grep, Agent
---

# Refine Oxabl Parser

Runs an iterative fix loop against a local ABL corpus whose path is set via the `ABL_CORPUS` environment variable (the corpus is kept outside the repo).

**Directory strategy:** List the subdirectories of `$ABL_CORPUS` at the start. Begin with the first subdirectory. If the corpus check produces no errors for that directory, move on to the next subdirectory and continue. Work through them in order until errors are found, then run the full fix loop against that directory.

Each iteration:
1. Run the check command for the current directory, collect the error breakdown
2. Pick the highest-value error pattern to fix (Haiku sub-agent)
3. Find the exact parser location to change (Explore sub-agent)
4. Apply the fix in this context
5. Validate: `cargo fmt`, `cargo clippy`, `cargo test` — all must pass with zero warnings/errors
6. Commit and push to master

Repeat until no further progress can be made or you are told to stop.

> **Preprocessor note:** The `oxabl_preprocessor` crate is implemented and can be enabled via `--preprocess`. When running with preprocessing, errors tagged `[in include]` indicate failures inside expanded include file content — these are real parser gaps exposed by expansion. Errors tagged `[preprocess]` indicate preprocessor-level failures (missing includes, etc.) — skip those.

## Corpus Check Command

**Without preprocessing** (default — tests parser against raw source):
```bash
cargo run --bin oxabl check $ABL_CORPUS/<directory>
```

**With preprocessing** (tests parser against preprocessor-expanded source):
```bash
cargo run --bin oxabl check $ABL_CORPUS/<directory> --preprocess -I $ABL_CORPUS
```

The `-I` flag sets the include search path (PROPATH equivalent). Use `$ABL_CORPUS` as the root since include references in the corpus are relative to it (e.g. `{gl/global-input.i}`).

Replace `<directory>` with the current subdirectory being analysed.

The output reports: total files, pass/fail counts, success %, and a ranked list of top error messages with counts.

**When to use each mode:**
- Use **without preprocessing** when fixing core parser gaps (raw ABL syntax the parser doesn't handle)
- Use **with preprocessing** when the raw-mode pass rate is high (>98%) and you want to find parser gaps revealed by include expansion — these are real-world code paths that only appear after includes are resolved

## Parser Source Layout

```
crates/oxabl_parser/src/parser/
  mod.rs          — shared helpers: can_be_identifier(), parse_identifier(), parse_data_type(), parse_lock_type()
  statements.rs   — all statement parsers (parse_for_each, parse_find_statement, parse_assign_statement, etc.)
  expressions.rs  — expression parsers (parse_postfix, parse_primary, parse_function_call, parse_additive, etc.)
  tests.rs        — parser test suite

crates/oxabl_ast/src/
  statement.rs    — AST Statement enum, struct definitions
  expression.rs   — AST Expression enum

crates/oxabl_lexer/src/
  kind.rs         — Kind enum + match_keyword() — all token type names
```

Key helpers:
- `can_be_identifier(Kind) -> bool` — controls which keywords can appear as identifiers/names
- `parse_lock_type()` — consumes NO-LOCK / SHARE-LOCK / EXCLUSIVE-LOCK
- `skip_to_period()` — error recovery: skip tokens until next `.`
- `synchronize()` — skip to next line

## Step 1 — Triage (Haiku sub-agent)

Spawn a Haiku agent with the following information:

- The raw output of the corpus check command (pass %, error breakdown table)
- The current git log (last 5 commits) so it knows what has already been fixed

Ask it to pick **one** error pattern that:
- Has the most occurrences (or is the most actionable)
- Is NOT "Expected '.' to end statement" (too broad — only target it if it's the only one left)
- Returns: the exact error message, the file path, and the line number of one representative example

## Step 2 — Locate the Fix (Explore sub-agent)

Spawn an Explore sub-agent with:
- The error message
- The representative file + line number
- The source snippet around that line (±10 lines)
- The parser source layout above

Ask it to:
1. Read the failing ABL code around the error line
2. Identify what ABL construct is present that the parser doesn't handle
3. Search the parser source for the right function to modify
4. Return: file path + start/end line numbers of the function to change, and a plain-English description of what needs to be added

The sub-agent should NOT make any edits — only locate and describe.

## Step 3 — Apply the Fix (main context)

Read the function identified by the Explore sub-agent. Understand the existing pattern, then add the missing handling. Follow these principles from the codebase:

- Dispatch on `Kind` enum (O(1) integer comparison), never on string comparison
- If a keyword needs to be usable as an identifier, add it to `can_be_identifier()` in `mod.rs`
- For unknown statement types with no deeper structure needed, use `skip_to_period()` and return `Statement::Empty`
- For expression-position keywords, check `parse_primary()` in `expressions.rs`
- For block-header clauses (like `TRANSACTION`, `WITH FRAME name`), add consumption before the `:` expectation
- Test the fix mentally against the error example before committing

## Step 4 — Validate

Run in sequence — stop and report if any step fails:

```bash
cargo fmt --check  # or cargo fmt then re-check
cargo clippy       # zero warnings
cargo test         # all tests pass
```

If validation fails:
- Read the error output carefully
- Fix the issue (borrow checker, unused variables, etc.)
- Re-run validation from the top

## Step 5 — Measure and Commit

Re-run the corpus check to confirm improvement (success rate must not regress).

Then commit with a conventional commit message:

```
fix(parser): <short description of what was fixed>

- <bullet for each specific change>
```

Then push to master:

```bash
git push origin master
```

This triggers CI (cargo check, test, fmt, clippy) and CodSpeed benchmarks. If CI fails, investigate and fix before the next iteration.

## Loop Control

After each successful commit+push, start the next iteration from Step 1. Stop when:
- The user asks to stop
- No error pattern can be fixed (all remaining errors require deeper investigation)
- The success rate is 100%

If an error pattern is too ambiguous (e.g. "Expected '.' to end statement" with 400+ occurrences), sample 3–5 representative files to find a common sub-pattern before attempting a fix.
