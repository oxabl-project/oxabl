---
title: "fix: ADM2 xp-property BUFFER-FIELD fast path (#66)"
type: fix
status: active
date: 2026-07-18
origin: GitHub #66 (follow-up from #65 round-4 corpus A/B)
branch: fix/inline-preproc-if-expression
---

# fix: ADM2 xp-property BUFFER-FIELD fast path (#66)

## Context

#65 closed the preprocessor / mid-line `&IF` / companion parser stack
(`da7fa4d`). PREPROC002 is **0** on the 9-module sample. Remaining PARSE001
(~306 deduped) is almost entirely the ADM2 **xp-property `BUFFER-FIELD` fast
path** inside real `$DLC/tty/get` / `$DLC/tty/set` when `DEFINED(xp{1})` is
true — deferred non-goal of #65, now tracked as #66.

| Signal | baseline `1e997d2` | post-#65 `da7fa4d` |
|--------|--------------------|--------------------|
| PREPROC002 | 596 | **0** |
| PARSE001 (deduped) | 9 | ~306 |

Dedup skew: baseline errors concentrated in shared includes that aborted early;
after expansion the same construct appears once per WebSpeed file. Measurement
artifact of progress, not a regression of the four #65 surface fixes.

## Failure modes (confirmed locally + corpus)

### 1. ~299× `Unexpected token Comma` (ASSIGN) / `Expected '.'` (standalone)

Expanded form:

```abl
ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending,buildDataRequest
```

Root cause chain:

1. Host include call often passes a comma-list as one positional arg
   (`{set DataSourceEvents dataAvailable,confirmContinue,…}` — unquoted, no
   spaces; or quoted `"a,b,c"`).
2. `read_arg_value` strips outer quotes on positional args (Progress-like
   delimiter semantics).
3. Real/stub `set` templates substitute bare `{2}` into the BUFFER-FIELD
   assignment RHS (expression position — correct for variable names).
4. Resulting RHS is **unquoted comma-separated identifiers**.
5. Parser: `parse_or()` takes the first identifier; `,` is not part of any
   expression production → under `ASSIGN`, `Unexpected token Comma`; under
   standalone assignment, `Expected '.' to end statement`.

Local repros (current tip):

| Source | Result |
|--------|--------|
| `h:BUFFER-FIELD('x':U):BUFFER-VALUE = a,b,c.` | fail (Expected '.') |
| `ASSIGN h:BUFFER-FIELD('x':U):BUFFER-VALUE = a,b,c.` | fail (Unexpected Comma) |
| same with quoted `"a,b,c"` | **pass** |
| `ASSIGN` multi-pair with quoted values | **pass** |

### 2. ~4× `Expected '.' to end statement` — bare consecutive accessor assigns

```abl
ghProp:BUFFER-FIELD('ObjectType':U):BUFFER-VALUE = 'Procedure':U
ghProp:BUFFER-FIELD('ContainerType':U):BUFFER-VALUE = '':U.
```

The `ASSIGN` keyword is gated in real `set`/`get` on xp-property defines
(`xp-assign`, `xp{1}`, …). When the BUFFER-FIELD body emits without a leading
`ASSIGN`, consecutive `handle:…:BUFFER-VALUE = …` lines run as standalone
assignments; the first requires a terminating `.` before the second starts.

Local repro: two chained `BUFFER-FIELD` assigns without a period between them
→ fail on line 2.

Chained lvalue `handle:BUFFER-FIELD(...):BUFFER-VALUE` already parses (postfix
member/method); no gap there.

## Ownership decision

| Symptom | Owner | Why |
|---------|-------|-----|
| Unquoted comma-list RHS | **Parser** | Already-expanded text must parse; preproc quote-restore is incomplete (hosts also pass unquoted lists). |
| Bare multi-assign without `ASSIGN` / missing `.` | **Parser** (primary) | Corpus still hits this when xp-assign gates omit `ASSIGN`; accepting multi-pair without the keyword (or continuing pairs before `.`) matches the emitted shape. Preproc xp-define fidelity is optional hardening. |
| Faithful xp-* DEFINE gating / full ADE `set` body | **Preproc fixtures** | Unit stubs force `DEFINED(xp…)` true so BUFFER-FIELD branch is tested; no need to ship full ADE sources. |

**Not** the primary fix: stop stripping quotes on positional include args. That
helps only the quoted-host case, breaks no known tests today, but does not cover
unquoted comma-lists and is a broader preproc semantic change (defer unless a
follow-up wants Progress parity review).

## Goal

On the same 9-module private-corpus sample, schema-loaded, oxabl ≥ this fix:

| Signal | Pass |
|--------|------|
| PREPROC002 | stay **0** |
| PARSE001 (deduped) | ≤ baseline **9** |
| Net parse fails | ≤ 0 |

Unit-level (this repo, always runnable):

- Unquoted comma-list BUFFER-VALUE (standalone + `ASSIGN`) parses.
- Bare consecutive BUFFER-FIELD assignments (no `ASSIGN`, one trailing `.`) parse.
- xp-path stub include (`DEFINED(xp{1})` true) expands and parses end-to-end.
- Existing DYNAMIC-FUNC / mid-line `&IF` / `IN SUPER` tests stay green.
- `cargo test`, `cargo clippy -D warnings`, `cargo fmt` green.

## Non-goals

- Full semantic typing of comma-list values as CHARACTER (parse-faithful AST is enough; synthetic string literal is fine).
- Shipping real `$DLC/tty/get`/`set` binaries/sources.
- Perfect xp-assign / xp-no-repository / repository branch coverage for every ADE gate.
- Merging to `master` as part of the code change (PR can open; merge after
  consumer A/B if required by process).
- Changing include-arg quote-stripping policy (separate discussion).

## Approach

### Slice A — Comma-separated identifier list as assignment value (parser)

**Where:** `crates/oxabl_parser/src/parser/statements.rs`

- Add `parse_assignment_value()`, used by:
  - standalone assignment (`if self.check(Kind::Equals)` path ~L723)
  - `parse_assign_pairs()` value path (~L838)
  - Slice B additional pairs (must share the helper)

- **Lookahead-first fold** (no speculative parse / no discarded NodeIds):

  At the start of a non-IF value, if tokens are `ident-like ',' ident-like`
  (and the post-comma ident is not followed by `=`), consume
  `ident (',' ident)+` and emit a single `Literal::String` whose span covers
  the full list and whose `value` is the source slice
  (e.g. `dataAvailable,confirmContinue,…`).

- Guard post-comma tokens: must be `can_be_identifier`; **exclude**
  `NoError`, `When`, `KwIn`, period/EOF; **do not** continue when the
  post-comma ident is followed by `Equals` (avoids swallowing a following
  ASSIGN pair target if a comma ever abuts `t1 = a, b = c`).

- **Do not** invent a new `ExpressionKind` variant. String literal is the
  right semantic stand-in for a character list written without quotes.

- **Do not** fold when the first token is not a bare list start
  (`f(1), x` — next is `(` not `,` — stays out of scope).

- Safety invariant: the fold only fires on token sequences that are **parse
  errors at current tip** (`Unexpected token Comma` / `Expected '.'`).

### Slice B — Multi-pair assignment without `ASSIGN` keyword (parser)

**Where:** same file, standalone assignment path after first `target = value`.

- After first pair is parsed (via `parse_assignment_value`), **before**
  requiring `.`:
  - If more bare pairs ahead → collect into `SmallVec<AssignPair>` and emit
    `StatementKind::Assign { assignments }`.
  - Else → existing single `StatementKind::Assignment` + optional `NO-ERROR` + `.`.

- **Detection must be non-allocating** (Fable amendment / ast-invariants.md §2
  NodeIdAllocator dense-contiguous: no speculative `parse_additive` + rollback):
  - Next token is identifier-like AND not `can_start_statement` / preproc
    directive / `End` / `KwElse` / `Leave` / `Next` / `NoError` / period
    (reuse the guard set at statements.rs ~777–789).
  - Scan forward for `Equals` at paren/bracket-depth 0 before
    `Period` / EOF / statement boundary (bounded scan, ~64 tokens for
    chained `BUFFER-FIELD(…):BUFFER-VALUE` forms).
  - Only then parse the pair for real; additional pairs also use
    `parse_assignment_value` so comma-lists fold inside bare groups.

- Single-pair path remains `StatementKind::Assignment`.

This accepts:

```abl
ghProp:BUFFER-FIELD('ObjectType':U):BUFFER-VALUE = 'Procedure':U
ghProp:BUFFER-FIELD('ContainerType':U):BUFFER-VALUE = '':U.
```

as one grouped assign.

### Slice C — Fixtures + tests

**Parser tests** (`crates/oxabl_parser/src/parser/tests.rs`) — all required:

1. `buffer_value_unquoted_comma_list_standalone` — single statement, period.
2. `buffer_value_unquoted_comma_list_under_assign` — `ASSIGN … = a,b,c.`
3. `buffer_field_chained_lvalue_quoted_still_works` — regression guard.
4. `bare_consecutive_buffer_field_assigns` — two pairs, one trailing period,
   no `ASSIGN` keyword; assert `Assign` with 2 pairs.
5. `comma_list_then_no_error` under ASSIGN — `ASSIGN … = a,b,c NO-ERROR.`
6. `comma_list_then_second_assign_pair` — `ASSIGN t1 = a,b t2 = c.` (fold/loop
   interplay is the highest-risk spot).
7. `bare_multi_with_comma_list_value` — combined A+B.
8. `bare_assign_does_not_swallow_run` — negative: `x = 1 RUN foo.` still fails
   / does not treat `RUN` as a second pair.

**Preprocessor** (`crates/oxabl_preprocessor`):

9. Stub `$DLC/tty/set` xp branch end-to-end (BUFFER-FIELD + comma list host).
10. **Characterization:** single-quoted host arg `{set X 'a,b,c':U}` — pin
    current `read_arg_value` strip behavior (quotes stripped; trailing `:U` may
    become a separate positional). No quote-policy change this PR.
11. Keep existing DYNAMIC-FUNC branch tests green (xp **undefined**).

### Slice D — Docs / issue hygiene

- Short comment on #66 with pin SHA, smoke commands for downstream.
- Optional HANDOFF.md refresh after push (not required for green CI).

## Implementation order

1. Slice A + unit tests (unlocks ~299 corpus failures).
2. Slice B + unit tests (unlocks ~4).
3. Slice C end-to-end stub (guards both branches of get/set).
4. `cargo test`, `clippy -D warnings`, `fmt`.
5. Push branch; comment on #66.

## Files touched (expected)

| Path | Change |
|------|--------|
| `crates/oxabl_parser/src/parser/statements.rs` | `parse_assignment_value`; multi-pair bare assign |
| `crates/oxabl_parser/src/parser/tests.rs` | regression tests |
| `crates/oxabl_preprocessor/src/preprocessor.rs` | xp-path include stub test (optional if check is parser-only) |
| `docs/plans/2026-07-18-004-fix-xp-property-buffer-field-plan.md` | this plan |

No lexer/codegen changes. No new AST variants. No CLI changes.

## Risks & mitigations

| Risk | Mitigation |
|------|------------|
| Comma-fold steals real syntax | Lookahead-only when first value tokens are `ident ',' ident`; never after calls/binops. Both folds fire only on sequences that are parse errors today. |
| Multi-pair without ASSIGN swallows next statement | Non-allocating scan + `can_start_statement` guard; negative test `x = 1 RUN foo.`. |
| Speculative parse NodeId gaps | Slice B uses token scan only; Slice A parses the list in one pass as a string. |
| Semantic/lint treat synthetic string oddly | Acceptable; event lists are character data, not var refs. |
| Corpus bar needs `$DLC` + the private corpus | Unit tests are merge gate; full A/B is downstream-owned when `CORPUS_ROOT` unavailable. |
| ~3 of ~306 PARSE001 unattributed | ≤ 9 bar leaves slack. |

## Success criteria

- [ ] Slices A–C implemented with tests green
- [ ] `cargo test` workspace green
- [ ] `cargo clippy --workspace --all-targets -- -D warnings` green
- [ ] `cargo fmt --check` green
- [ ] Branch pushed; #66 updated with summary + smoke
- [ ] Downstream A/B (when available): PARSE001 ≤ 9, PREPROC002 = 0

## Smoke for downstream consumer

```bash
# 1. Unquoted comma-list BUFFER-VALUE
cat > /tmp/bf_comma.p << 'EOF'
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
ASSIGN ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending,buildDataRequest.
EOF
oxabl check /tmp/bf_comma.p
# expect exit 0 (was: Unexpected token Comma)

# 2. Bare consecutive BUFFER-FIELD assigns
cat > /tmp/bf_bare.p << 'EOF'
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
ghProp:BUFFER-FIELD('ObjectType':U):BUFFER-VALUE = 'Procedure':U
ghProp:BUFFER-FIELD('ContainerType':U):BUFFER-VALUE = '':U.
EOF
oxabl check /tmp/bf_bare.p
# expect exit 0 (was: Expected '.' to end statement)

# 3. xp-path stub include (no real DLC required)
mkdir -p /tmp/tty
cat > /tmp/tty/set << 'EOF'
&IF DEFINED(xp{1}) <> 0 &THEN
ASSIGN ghProp:BUFFER-FIELD('{1}':U):BUFFER-VALUE = {2}.
&ELSE
DYNAMIC-FUNC("set{1}":U IN TARGET-PROCEDURE, {2})
&ENDIF
EOF
cat > /tmp/xp_host.p << 'EOF'
&GLOBAL-DEFINE xpDataSourceEvents yes
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
{set DataSourceEvents dataAvailable,confirmContinue,isUpdatePending,buildDataRequest}
EOF
oxabl check /tmp/xp_host.p --preprocess -I /tmp/tty
# expect exit 0

# 4. Prior #65 wins still hold (fnarg / DYNAMIC-FUNC / IN SUPER) — see HANDOFF.md
```

With real OpenEdge: put `$DLC/tty` (and `gui`/`src`/`adm2` as needed) on `-I`
and re-run `scripts/corpus-ab-gate.sh` against the 9-module sample.
