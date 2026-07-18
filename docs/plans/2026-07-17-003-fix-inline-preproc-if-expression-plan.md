---
title: "fix: Inline &IF mid-line expansion drops branch body (#65)"
type: fix
status: ready
date: 2026-07-17
origin: GitHub #65 (comment 2 reframes root cause)
branch: fix/inline-preproc-if-expression
---

# fix: Inline `&IF` mid-line expansion drops branch body (#65)

## Problem (updated framing)

Stock OpenEdge WebSpeed/ADM2 code aborts preprocessing (exit 5) on forms like:

```abl
IF NOT {fnarg setOpenQuery cQ} THEN RETURN.
```

Issue #65 originally blamed unresolvable `{set}`/`{fn}`/`{fnarg}` includes.
A follow-up comment **reframes the root cause**: those names **are** real
extensionless ADE includes under `$DLC/tty` / `$DLC/gui`. Putting them on `-I`
stops the `PREPROC007` cascade but **does not** stop the parse abort.

### Minimal repro (no DLC, no includes)

```abl
DEFINE VARIABLE i AS INTEGER NO-UNDO.
i = &IF "":U = "":U &THEN 5 &ELSE 6 &ENDIF .
```

**Actual:** `parse error … Unexpected token Eof` (exit 5) — the whole
`&IF … &ENDIF` is dropped, leaving `i = .`.

**Expected:** preprocesses to `i = 5 .` (or equivalent) and parses.

Statement-position multi-line `&IF` already works:

```abl
&IF TRUE &THEN
i = 5.
&ELSE
i = 6.
&ENDIF
```

### Why ADM2 hits this

Real `$DLC/tty/fnarg` is a **single-line inline conditional**:

```
&IF "{3}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE, {2}) &ELSE dynamic-function("{1}":U IN {3}, {2}) &ENDIF
```

`{fn}` is the same shape. Expression-position uses
(`IF NOT {fnarg …} THEN`, `x = {fn …}`) therefore depend on mid-line `&IF`
splicing the selected branch into the token stream. Multi-line statement
includes like `set` can appear to “work” once resolved because their
`&IF`/`&THEN` bodies live on following lines.

## Root cause (code)

### Primary bug — `skip_to_eol` after `&THEN` / `&ELSE` / `&ENDIF`

In `crates/oxabl_preprocessor/src/preprocessor.rs`:

**`parse_if_condition`** (≈953–969):

```rust
if let Some(then_pos) = find_keyword(&upper, "&THEN") {
    let condition = rest[..then_pos].trim().to_string();
    let after_then = start + then_pos + 5; // len("&THEN")
    let end = skip_to_eol(source, after_then);  // ← BUG for inline form
    return (condition, end);
}
```

**`DirectiveKind::Else` / `EndIf`** (≈666–677):

```rust
let end = skip_to_eol(source, i + j);  // ← same class of bug
```

For multi-line form, `skip_to_eol` after `&THEN` only eats the rest of an
otherwise-empty directive line — fine. For inline form:

```
i = &IF TRUE &THEN 5 &ELSE 6 &ENDIF .
                 ^^^^^^^^^^^^^^^^^^^^^^^^
                 swallowed as part of the &IF directive end
```

The selected branch is never scanned as content, so nothing is emitted even
when the condition is true. Confirmed: after `&THEN`,
`skip_to_eol` consumes `' 5 &ELSE 6 &ENDIF .'`.

### Secondary gaps (needed for real `fnarg`, not the pure-`TRUE` minimal repro)

Once branch bodies are no longer swallowed, the real `fnarg` body still needs
ABL-faithful argument / qualifier handling:

| Gap | Why it matters for `fnarg` |
|-----|----------------------------|
| **Positional `{N}` inside string literals** | Body has `"{1}":U`; strings today only expand `{&name}`, not `{1}` |
| **Positional `{N}` inside `&IF` condition text** | Condition is `"{3}":U = "":U`; condition is sliced out and evaluated without a `{N}` pass |
| **`:U` (and friends) in conditions** | After expansion, `"":U = "":U` must be true; today `:` is skipped and `U` becomes an Ident, so comparison never runs and empty-string truthiness yields false → wrong branch |

These are in-scope for #65’s “done-when” (real `$DLC/tty` + ADM2 forms), not
drive-by refactors.

## Goals

1. Mid-line / expression-position
   `&IF … &THEN … &ELSE … &ENDIF` emits the selected branch and elides the
   directives (and non-selected branches).
2. Existing multi-line statement-position `&IF` tests keep passing.
3. With extensionless `fn`/`fnarg` stubs that **preserve the real inline `&IF`
   shape** (not simplified bare `DYNAMIC-FUNCTION`),
   `IF NOT {fnarg setOpenQuery cQ} THEN RETURN.` preprocesses and parses
   (exit ≠ 5).
4. With real `$DLC/tty` on `-I` (when available), stock ADM2 forms no longer
   abort preprocessing solely for this reason.

## Non-goals

- Auto-discovering `$DLC` or changing default include_paths.
- Shipping Progress ADE sources (use original minimal stubs that mirror
  structure only).
- Bare-name unresolvable-include → `?` recovery (prior plan idea) — useful
  resilience, **not** the #65 fix; optional follow-up.
- Full xp-property buffer-field fast path in real `get`/`set` (needs ADM
  property includes); statement-form includes are secondary once inline `&IF`
  works.
- #64 undefined `{&macro}` empty expand (separate).

## Design

### 1. Directive end offsets for the `&IF` family

**Rule:** A directive’s `end` is the first byte **after the directive keyword
(and for `&IF`/`&ELSEIF`, after `&THEN`)**. Do **not** call `skip_to_eol` for
`If`, `ElseIf`, `Else`, or `EndIf`.

| Directive | `end` after fix |
|-----------|-----------------|
| `&IF cond &THEN` | immediately after `&THEN` |
| `&ELSEIF cond &THEN` | immediately after `&THEN` |
| `&ELSE` | immediately after `ELSE` keyword |
| `&ENDIF` | immediately after `ENDIF` keyword |

`&SCOPED-DEFINE` / `&GLOBAL-DEFINE` / `&UNDEFINE` / `&MESSAGE` keep EOL-based
ends (their payloads are line-oriented).

**Effect on multi-line sources:** trailing horizontal whitespace and a single
newline after the keyword are consumed by `skip_after_if_keyword` so classic
forms still do not emit a blank directive line. Any *non-whitespace* same-line
content after `&THEN`/`&ELSE`/`&ENDIF` is scanned as code (not discarded). That
is more AVM-correct than the old “rest of line is gone” behavior, but it is a
**global** semantic change: every `&IF`/`&ELSE`/`&ENDIF` in every file is
affected. Micro-tests cannot prove “no net-new parse errors on a real tree” —
that is why **acceptance criterion 7 (corpus A/B gate)** is mandatory.

**Comments:** real corpora almost exclusively use `/* … */` block comments, not
`//`. Block comments are already tracked via `comment_depth` and never need a
special “same-line after `&THEN`” story. Do not document this as “handled by
the `//` skip.”

**Effect on `i = &IF … &ENDIF .`:** the final `.` remains in the stream after
`&ENDIF` is consumed — required for a valid assignment.

**`:U` scope:** string-attribute stripping applies only after a *quoted string
literal* token in the condition tokenizer. ADM2 always writes `"{N}":U` /
`"{&name}":U`. A bare `{&macro}:U` that expands to an unquoted value is out of
scope for #65 (silent assumption called out, not implemented).

### 2. Positional args in conditions

When evaluating `DirectiveKind::If` / `ElseIf`, expand `{N}` against the
current `positional_args` **before** `evaluate_with_defined`.

Sketch:

```rust
let condition = expand_positional_in_text(condition, positional_args);
// also existing {&name} via evaluate_with_defined / expand_refs
let cond_result = evaluate_with_defined(&condition, &self.vars);
```

`expand_positional_in_text` replaces `{0}`, `{1}`, … with arg values (missing
→ empty string, matching include-arg semantics). Prefer sharing logic with the
main `{N}` scanner rather than a one-off.

Pass `positional_args` into the place that evaluates conditions (already in
`process_source`).

### 3. Positional `{N}` inside string literals

In the string-scan arm of `process_source` (≈484–515), next to the existing
`{&name}` expansion, also expand `{digits}` positional references when
`emitting`, using the same flush/chunk/synthetic-node pattern as outside
strings (or a shared helper).

ABL expands include args inside quotes; literal `{` in strings uses `~{`.

### 4. Ignore ABL string attributes in preprocessor conditions

In `condition.rs` `tokenize`, after a string literal, optionally consume a
trailing `:` + attribute letters (`U`, `L`, `T`, `R`, … — at least `U`, the
one ADM2 uses everywhere). Do not emit tokens for the attribute.

So `"":U = "":U` → `StringLit("")`, `Eq`, `StringLit("")` → true.

Add unit tests for `:U` / `:L` after quoted strings and after `"{&x}":U` style
conditions once refs expand.

### 5. Regression stubs (must keep inline `&IF` shape)

Per the issue comment: a stub that is only

```
DYNAMIC-FUNCTION("{1}":U IN TARGET-PROCEDURE, {2})
```

**will pass against a still-buggy preprocessor.** Stubs used to gate #65 must
mirror the real one-liner:

```
&IF "{3}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE, {2}) &ELSE dynamic-function("{1}":U IN {3}, {2}) &ENDIF
```

Place under e.g. `resources/openedge-stubs/tty/fnarg` (and a similar `fn`) for
CLI tests; use `InMemoryFileSystem` for unit tests.

## Test coverage (required — gate merge)

Test coverage is the quality bar for this fix. Every behavioral change below
must have a **failing test before the production change**, then go green.
Do not ship the EOL fix without the secondary tests: a half-fix selects the
wrong `fnarg` branch and is easy to mistake for success.

### Coverage matrix

| Area | Cases (minimum) | Where |
|------|-----------------|--------|
| Inline `&IF` true/false/else | true→THEN, false→ELSE, false no ELSE→empty, only-THEN no ELSE | `preprocessor.rs` |
| Expression position | `i = &IF…&ENDIF.`, `IF NOT &IF…&ENDIF THEN`, assign RHS | `preprocessor.rs` |
| Mid-line + trailing code | period after `&ENDIF`, code after `&ENDIF` on same line | `preprocessor.rs` |
| Multi-line regression | existing tests unchanged; add one mixed file with both styles | `preprocessor.rs` |
| Nested inline | `&IF TRUE &THEN &IF FALSE &THEN A &ELSE B &ENDIF &ENDIF` | `preprocessor.rs` |
| `&ELSEIF` inline | `&IF F &THEN A &ELSEIF T &THEN B &ELSE C &ENDIF` one line | `preprocessor.rs` |
| String attributes in cond | `"":U = "":U`, `"x":U = "x":U`, `"x":U = "y":U`, `:L` smoke | `condition.rs` |
| Positional in condition | include body `&IF "{1}":U = "A":U &THEN yes &ELSE no &ENDIF` with arg `A` | `preprocessor.rs` |
| Positional in strings | include body `"x{1}y"` → `xAy`; `"{1}":U` in output | `preprocessor.rs` |
| Missing positional → empty | `{3}` absent → empty; `fnarg` 2-arg form takes THEN branch | `preprocessor.rs` |
| Real-shape `fnarg` / `fn` | full one-liner stub; 2-arg and 3-arg (handle) forms | `preprocessor.rs` (+ optional CLI) |
| Non-selected branch elided | false THEN body must not appear in `to_text()` | `preprocessor.rs` |
| Directives elided | output must not contain raw `&IF` / `&THEN` / `&ELSE` / `&ENDIF` | assert on key tests |

### Named tests (Step 0 — write these first)

**A. Core inline `&IF` (`preprocessor` unit tests)**

1. `inline_if_then_else_emits_true_branch`  
   `i = &IF TRUE &THEN 5 &ELSE 6 &ENDIF.` → contains `5`, not `6`; no
   `PREPROC002` unclosed-`&IF`.

2. `inline_if_false_emits_else_branch`  
   `i = &IF FALSE &THEN 5 &ELSE 6 &ENDIF.` → `6`, not `5`.

3. `inline_if_false_without_else_emits_nothing`  
   `i = &IF FALSE &THEN 5 &ENDIF.` → no `5` between `=` and `.` (or equivalent
   empty expansion).

4. `inline_if_empty_string_eq_with_u_qualifier`  
   `i = &IF "":U = "":U &THEN 5 &ELSE 6 &ENDIF.` → `5`.

5. `inline_if_in_if_not_expression`  
   `IF NOT &IF TRUE &THEN YES &ELSE NO &ENDIF THEN RETURN.`  
   → contains `YES` between `NOT` and `THEN`; **not** `IF NOT THEN`.

6. `inline_if_trailing_period_after_endif`  
   Same as (1); assert statement terminator survives after `&ENDIF`.

7. `inline_elseif_chain_one_line`  
   `&IF FALSE &THEN A &ELSEIF TRUE &THEN B &ELSE C &ENDIF` → `B` only.

8. `inline_nested_if`  
   Nested true/false combination → only the live branch text.

9. `multiline_if_still_works`  
   Re-assert (or rely on existing tests) classic newline form still emits
   correctly — must stay green.

**B. Positional args + include integration**

10. `positional_expanded_inside_string_in_include`  
    Include content `"pre{1}post"` with arg `MID` → `preMIDpost`.

11. `positional_expanded_in_if_condition`  
    Include: `&IF "{1}":U = "ok":U &THEN HIT &ELSE MISS &ENDIF`  
    Call `{inc.i ok}` → `HIT`; call `{inc.i no}` → `MISS`.

12. `missing_positional_is_empty_in_condition`  
    `&IF "{3}":U = "":U &THEN EMPTY &ELSE HAS &ENDIF` with only args 1–2 →
    `EMPTY`.

13. `fnarg_shaped_include_two_args` (**must use real inline `&IF` stub**)  
    Source: `IF NOT {fnarg setOpenQuery cQ} THEN RETURN.`  
    Stub = Progress-shaped one-liner.  
    Assert: no `PREPROC007`; output has `dynamic-function` / `setOpenQuery` /
    `cQ`; `TARGET-PROCEDURE` branch (THEN), not `IN ,` broken ELSE; not
    `IF NOT THEN`.

14. `fnarg_shaped_include_three_args_uses_else_handle`  
    `{fnarg setOpenQuery cQ hProc}` → ELSE branch with `IN hProc` (or stub’s
    `{3}` expansion). Locks the condition `"{3}":U = "":U` false path.

15. `fn_shaped_include_expression`  
    Minimal `{fn foo}` real-shape stub if distinct from `fnarg`; expression
    position smoke test.

**C. Condition tokenizer (`condition.rs` unit tests)**

16. `string_attribute_u_empty_eq` → `"":U = "":U` true  
17. `string_attribute_u_equal` / `unequal`  
18. `string_attribute_does_not_break_plain_string_eq` → `"a" = "a"` still true  
19. Optional: `:L` ignored same as `:U`

**D. Optional CLI / fixture (high value, not a substitute for A–C)**

20. `resources/openedge-stubs/tty/fnarg` (+ `fn`) with real-shape body  
21. `oxabl analyze` on a tiny host `.p` with `-I` stubs → exit 0 (no parse
    abort)

### Assertions style

- Prefer exact or tightly constrained `to_text()` checks (contains + does-not-
  contain), not only “process Ok”.
- Where the bug was parse abort, an integration-style assert that the
  preprocessed text is parseable (`Parser::new(...).parse_statements()`) is
  encouraged for tests 5, 13, 14 — catches “emits junk that still dies later.”
- Keep tests hermetic (`InMemoryFileSystem`); no DLC required in CI.

### What “good coverage” means for review

- [ ] Matrix rows A–C all have at least the named tests above  
- [ ] Real-shape `fnarg` stub is used (not simplified `DYNAMIC-FUNCTION` only)  
- [ ] Both THEN and ELSE paths of `fnarg` condition are tested (2-arg + 3-arg)  
- [ ] Existing multi-line `&IF` suite still green  
- [ ] No new clippy/fmt debt in test-only code

### Step 1 — Fix directive ends

- `parse_if_condition`: return `end = after_then` (no `skip_to_eol`).
- `Else` / `EndIf` arms: `end = i + j` (end of keyword). Optionally skip only
  horizontal whitespace if needed for cleanliness — **do not** skip past
  non-whitespace or newline in a way that drops code.
- Re-read `ElseIf` path (uses `parse_if_condition`) — fixed for free.

### Step 2 — Positional expand in conditions + strings

- Helper: `expand_positional_refs(text: &str, args: &[String]) -> String`
  (missing index → empty).
- Call from `If` / `ElseIf` evaluation.
- String arm: expand `{N}` when emitting (same as `{&name}` path).

### Step 3 — Condition `:U` (string attributes)

- After reading a string lit in `tokenize`, if next is `:` then consume ASCII
  letters as attribute; ignore.
- Tests as above.

### Step 4 — Optional CLI fixture

- `resources/openedge-stubs/tty/{fn,fnarg}` with real inline `&IF` shape.
- `crates/oxabl/tests/…` or preprocessor-only is enough if unit coverage is
  solid; CLI test is nice for exit-code confidence.

### Step 5 — Validate

```
cargo test -p oxabl_preprocessor
cargo test
cargo fmt --check && cargo clippy -D warnings
```

Manual with DLC (when present):

```
oxabl analyze $DLC/src/adm2/query.i --preprocess \
  -I $DLC/tty -I $DLC/gui -I $DLC/src -I $DLC/src/adm2
```

Expect: no exit 5 from `IF NOT {fnarg …} THEN` / empty inline `&IF`.

## Acceptance criteria

| # | Criterion |
|---|-----------|
| 1 | `i = &IF TRUE &THEN 5 &ELSE 6 &ENDIF.` → true branch in output, parses |
| 2 | `i = &IF FALSE &THEN 5 &ELSE 6 &ENDIF.` → else branch |
| 3 | `i = &IF "":U = "":U &THEN 5 &ELSE 6 &ENDIF.` → true branch |
| 4 | Real-shape `fnarg` stub + `IF NOT {fnarg setOpenQuery cQ} THEN RETURN.` preprocesses without exit 5 |
| 5 | `fnarg` 3-arg form selects ELSE/handle branch (condition false path) |
| 6 | Positional `{N}` expands inside strings and inside `&IF` conditions |
| 7 | **Corpus A/B gate (merge-blocking):** no net-new PARSE001 / preproc abort signal on the 9-module erp-5899 sample (same harness as #58). Expect PREPROC002 (“unclosed &IF”) to drop toward 0; PARSE001 must not climb. See [Corpus A/B gate](#corpus-ab-gate) below. |
| 8 | Existing multi-line `&IF` unit tests still pass |
| 9 | Test coverage matrix (section above) complete |
| 10 | Issue “done-when”: with `$DLC/tty` on `-I`, ADM2 `{fn}`/`{fnarg}` forms no longer abort solely due to empty inline `&IF` |

## Corpus A/B gate

Micro-tests (including the new inline suite) only cover synthetic shapes. The
existing multi-line suite at `preprocessor.rs` (~`if_true_branch` et al.) does
**not** catch trailing-same-line regressions. Criterion 7 is the only gate that
protects real check numbers.

### Procedure (same pattern as #58)

1. **Baseline (A):** on `master` (or the pre-fix pin), run
   `scripts/corpus-ab-gate.sh` against the 9-module erp-5899 sample with
   `--preprocess` and the workspace’s usual `-I` paths. Save the JSON summary
   as `ab-baseline.json`.
2. **Candidate (B):** on `fix/inline-preproc-if-expression`, rebuild oxabl,
   re-run the same modules / same `-I`, save `ab-candidate.json`.
3. **Compare** (script prints a diff table):
   - **pass rate / fail count** — must not regress
   - **PARSE001 / parse-error message totals** — must not climb
   - **PREPROC002** (unclosed &IF, if collected) — should drop toward 0
   - **PREPROC007** — may move with include path differences; not the #65 signal
4. **Downstream pin (when pushing):** after oxabl is on a pushable rev, bump
   pivot’s oxabl `rev` in `Cargo.toml` the same way as #58 and re-run the
   consumer-side sample check; expect no net-new parse noise.

### Pass / fail rule

| Signal | Pass |
|--------|------|
| Parse fail count (B − A) | ≤ 0 |
| Top parse-error patterns newly introduced | empty (or explained + fixed) |
| PREPROC002 count (B vs A) | B ≤ A (prefer large drop) |

Fail the PR if PARSE001 climbs even when micro-tests are green.

### Harness

`scripts/corpus-ab-gate.sh` — see script header for env vars:

```bash
# Example (paths are machine-local; erp-5899 sample is not in-repo)
export CORPUS_ROOT=/path/to/erp-5899/erp/code   # or pcna-erp root
export INCLUDE_PATHS="-I $CORPUS_ROOT -I $DLC/src -I $DLC/tty"
export MODULES="mod1 mod2 …"   # 9 modules used for #58; override as needed
./scripts/corpus-ab-gate.sh baseline   # → ab-baseline.json
# checkout fix branch, cargo build --release -p oxabl
./scripts/corpus-ab-gate.sh candidate  # → ab-candidate.json
./scripts/corpus-ab-gate.sh diff
```

The script is hermetic with respect to oxabl’s git tree; it only requires a
local ABL tree. If `CORPUS_ROOT` is missing, it exits non-zero with instructions
(do not silent-skip criterion 7).

## Key decisions

1. **Root cause is inline `&IF` end-of-line swallowing, not include classification.**
   Do not build ADM2-specific macro magic.
2. **`end` for `&IF` family stops at the keyword/`&THEN`, not EOL** — one rule
   for multi-line and inline.
3. **Regression stubs must keep the inline `&IF` shape** or they false-pass.
4. **Positional-in-strings + `:U` + positional-in-conditions** are part of the
   same fix for real `fnarg`, not optional polish.

## Alternatives considered

| Approach | Why rejected |
|----------|--------------|
| Special-case only `{fn}`/`{fnarg}` names | Misses general ABL inline `&IF`; whack-a-mole |
| Pre-expand entire include text with args before scanning | Larger rewrite; directive-end fix is still required |
| Emit `?` for unresolvable includes (old plan) | Does not fix resolved-fnarg abort |
| Keep `skip_to_eol` but re-scan line | More complex, easy to get wrong |

## Risks

- **Rest-of-line after `&ENDIF` now parses as code** — intended for inline
  form; could surface new parse errors on code that relied on “junk after
  `&ENDIF` is discarded.” Uncommon; closer to AVM.
- **Wrong branch if `:U`/positional gaps lag the EOL fix** — land condition +
  positional tests in the same PR as the EOL fix so `fnarg` is not half-fixed.
- **Condition strings with nested quotes / `~`** — stick to ADM2 patterns
  first; extend if corpus demands.

## PR plan

### Single PR (recommended)

**Title:** `fix: expand inline &IF branches mid-line (#65)`

**Files:**

- `crates/oxabl_preprocessor/src/preprocessor.rs` — directive ends; positional
  in conditions/strings
- `crates/oxabl_preprocessor/src/condition.rs` — string attributes `:U` etc.
- Tests in both modules
- Optional: `resources/openedge-stubs/tty/*`, short README note on `$DLC/tty`

**Deps:** none  

**Closes:** #65  

Split only if the PR grows: (1) EOL/`end` fix + pure `TRUE`/`FALSE` tests,
(2) positional + `:U` + `fnarg`-shaped stub. Prefer one PR — the done-when
needs both.

## Related

| Item | Relation |
|------|----------|
| #65 comment 2 | Authoritative reframing |
| Prior session plan (include paths / `?` recovery) | Superseded as primary fix; optional follow-ups only |
| #64 undefined `{&name}` | Adjacent preprocessor fidelity; separate |
| PREPROC007 design | Unrelated once includes resolve; abort was never PREPROC007 severity |

## Supersedes

The previous plan for this session (extensionless path docs + bare-name `?`
recovery) addressed a **red herring** relative to the abort. Keep only:

- Docs note that `$DLC/tty` / `$DLC/gui` belong on PROPATH for ADE includes
  (operability; does not fix the abort alone).

Do **not** implement bare-name `?` recovery as part of closing #65 unless
time remains after the inline-`&IF` fix is green.
