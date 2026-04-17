---
title: Fix string backslash regression and IF/THEN/END. ELSE gap
type: fix
status: completed
date: 2026-04-16
---

# Fix string backslash regression and IF/THEN/END. ELSE gap

## Overview

Corpus runs against `~/legacy-fdm4/pcna-erp` surface two real parser defects that
masquerade as ~40 distinct failures. Both are regressions/gaps, not user syntax
errors, and both have sharply scoped fix sites. This plan addresses:

1. **Lexer**: string-literal termination regresses on real ABL strings that end
   in a literal backslash (`"\\"`, `"..\\\\"`) because of the
   backslash-escapes-quote tolerance added in commit `40579e7`.
2. **Parser**: `KwElse` is rejected at statement-dispatch in several files where
   `IF … THEN DO: … END. ELSE …` is used. Either `parse_if_statement` is never
   reached for the outer IF, or an inner construct is swallowing the IF scope.

Template files (`refsingle*.cls`, `tempwp.cls`, `transwp.cls`), I/O (non-UTF-8)
files, and the single real typo in `erp_gl_bank_rec_host.p` are **out of scope** —
confirmed unparseable or legitimate source bugs.

## Problem Statement

### Bug 1 — Backslash string regression

`crates/oxabl_lexer/src/lib.rs:601-606`:

```rust
Some('\\') => {
    self.advance(); // consume backslash
    if matches!(self.peek(), Some('"') | Some('\'')) {
        self.advance(); // consume escaped quote
    }
}
```

For source `"\\"` (two backslashes between quotes — a common Windows path
fragment in legacy ABL) the loop:

1. Consumes the opening `"`.
2. Sees `\`, takes the backslash arm, peeks `\` (not a quote) — no extra advance.
3. Next iteration, sees the second `\`, takes the arm, peeks `"` (quote!),
   advances past the **closing** quote as if it were an escape.
4. Keeps consuming until the next `"` in the file, producing one giant spurious
   string that eats the intervening code.

This is a direct regression of commit `40579e7` ("accept backslash-escaped
quotes inside string literals") against the prior behaviour established in
`95cfc04` ("backslash is not an escape character in ABL strings"). ABL's
official string escape is tilde (`~`); the lexer already handles that
correctly at lines 590-593.

**Representative corpus hits** (all cascade from this single bug):

| File | Reported location | Pattern |
| --- | --- | --- |
| `b2c/b2cStoreLocatorAjax.p:85` | `Expected ')'` | `replace(s, "\\", "&#92;")` |
| `oe/comprehensive-rpt-async.p:353` | `Unexpected token Invalid` | `"\\" + cFilename.` |
| `ms/stamper.p:77` | `Expected ')'` | `Replace(Entry(ndx, MergeList), "\\", "/")` |
| `ms/mf_print_docket.p:805` | `Expected ')'` | same shape |
| `ms/excel_host.p:319` | `Expected ')'` | same shape |
| `ms/recordattachbl.p:47` | `Unexpected token Invalid` | `"\\" + order.cust-number` |
| `forms/deco_proof_form.cls:207` | `Expected ')'` | same shape |
| `dotnetrm/erp_rm_path_control_host.p:80` | `Expected ')'` | path concatenation |
| `edi/xml_get.p:660`, `zf/easi-get.p:1212`, `zf/xfer-get.p:781` | `Expected ')'` | Windows path strings |
| `oe/*-q.p`, `oe/csm_*.p`, `oe/workorderx-q.p`, `oe/mfworkorderx-q.p`, `oe/proofx-q.p`, `oe/storyboardx-q.p`, `oe/masterproofx-q.p`, `oe/gui_batch_report_async.p`, `oe/oe944-excel-a.p`, `oe/oe987*.p` | `Expected ')'` / `Invalid` | all contain `"\\"` path literals |
| `po/po991-q.p`, `po/wspo991-q_batch.p`, `rm/rm444-a.p`, `rm/wsrm601.p`, `mf/mf_prod_step_form.p`, `ms/compile10.w`, `ms/fedexReturnLabelSendEmail.p`, `ms/upsReturnlabelSendEmail.p`, `xml/wsaGIPMCP.p` | various | same |

### Bug 2 — `IF … THEN DO: … END. ELSE …` treated as stray `KwElse`

The ABL idiom

```abl
IF cond THEN DO:
   ...
END.
ELSE DO:
   ...
END.
```

is legal: the `.` ends the inner DO's `END` statement, but the `IF` scope stays
open and binds the following `ELSE`. Our `parse_if_statement`
(`crates/oxabl_parser/src/parser/statements.rs:2321-2371`) already tolerates a
trailing period before `ELSE` (the `while self.check(Kind::Period)` loop at
2343), and `parse_block_body` at `statements.rs:3888` consumes `END .` cleanly
before returning — so the happy path should work.

Yet the corpus reports `Unexpected token KwElse` at statement-dispatch level
(i.e. `parse_statement` is being called with `KwElse` as the leading token,
meaning no outer `parse_if_statement` frame is on the stack). That implies one
of:

- The IF is parsed inside a context that returns control to
  `parse_block_body`/`parse_program` **before** the IF's else branch is
  considered (e.g. an error-recovery synchronise on period loses the IF frame).
- The IF's then-branch is something other than `parse_do_statement` /
  `parse_statement` (e.g. a `{&misc-keys}` preprocessor expansion whose last
  token is a period), leaving `parse_if_statement` to not see the real block.
- A stray preprocessor reference / include boundary emits a `Period` that the
  enclosing caller consumes as the statement terminator.

Fix-site investigation must enumerate the real call stack before writing code;
the candidate regions are `parse_if_statement` (2321), `parse_statement`
dispatch (the KwElse arm does not exist — it falls through to "unexpected"),
and the error-recovery synchronise path in `parse_program` / `parse_block_body`.

**Representative corpus hits**:

| Location | Shape |
| --- | --- |
| `wam_tmpl/manage_store.p:3890` (preprocessed 11655) | `end. else do:` after `if prod_property Then do:` |
| `ad/ad100.p` (in include, line 1400) | `{&misc-keys}` then `Else do:` |
| `ms/secco.p`, `secem.p`, `secss.p` (includes) | `end. else do:` after `{&…}` block |
| `static/cleanm.p`, `ld-mnuprc.p`, `menu_hed.p`, `menu_prc.p`, `mnuprc-diff.p`, `table-de.p` (includes) | same |

## Proposed Solution

### Fix 1 — Remove backslash escape tolerance

ABL has **one** string escape character: tilde. The safest fix is to revert the
backslash branch in `read_string_literal` so `\` is always a literal byte. The
HTML/JS snippets motivating `40579e7` (`"<td style=\"x\">"`) are uncommon in the
legacy corpus compared to Windows paths; when they do appear, the correct ABL
spelling is `"<td style=~"x~">"`, which the lexer already handles.

Target change (`crates/oxabl_lexer/src/lib.rs:594-606`):

```rust
// Previously consumed `\"` / `\'` as an escaped quote — reverted because
// it broke `"\\"` (legitimate two-backslash literal) by swallowing the
// closing quote. Backslash is a literal byte in ABL strings; the official
// escape is tilde (`~`), handled above.
Some('\\') => {
    self.advance(); // consume backslash as a regular character
}
```

If corpus re-run shows regressions from HTML-embedded strings that relied on
the tolerance, the follow-up is to migrate those fixtures to the tilde form
and document the expectation in `CLAUDE.md`. Do **not** reintroduce
disambiguation heuristics — there is no way to distinguish `"\\"` (two
backslashes) from `"\\\""` (escaped backslash-quote) without a proper escape
scheme, which ABL lacks for `\`.

### Fix 2 — Make `IF … THEN DO: END. ELSE` always bind

Two-step approach:

1. **Reproduce in isolation.** Add a minimal failing unit test in
   `crates/oxabl_parser/src/parser/tests.rs` covering:
   - `if x then do: end. else do: end.`
   - the same pattern nested two levels deep inside a PROCEDURE body
   - `if x then do: end. else y = 1.`
   - `if x then run p. else do: end.`
   Run under `cargo test -p oxabl_parser` and confirm which cases fail. If any
   of the synthetic forms pass, the defect is entirely tied to preprocessor /
   include expansion (investigate `ad100.p`-style `{&misc-keys}` cases first).
2. **Fix the identified site.** Based on where the failure reproduces:
   - If the period-skip loop at `statements.rs:2343` is never reached, the
     fix belongs in `parse_statement`'s dispatch (allow a trailing `KwElse`
     to rebind to the most recent open `IF` via parser state), OR the
     then-branch parser is consuming the `ELSE` before returning.
   - If the error-recovery path is eating the IF frame, relax the
     synchronise boundary so a lone `KwElse` re-enters the pending IF.
   - If preprocessor include boundaries are generating spurious periods,
     fix the preprocessor expansion in `oxabl_parser/src/preprocessor/*`
     (see `git log --oneline -- crates/oxabl_parser/src/preprocessor/` —
     commits `57652c3`, `d47812d` touched comment-skipping and dynamic
     include resolution respectively, both are adjacent areas).

The plan intentionally does **not** prescribe the exact edit for Fix 2 until
the reproducer narrows the site. Guessing the fix before having the minimal
failing test risks the same mis-targeted patch that caused Bug 1.

## Technical Considerations

- **Performance.** Neither fix touches hot paths in a way that regresses
  CodSpeed. Bug 1's fix strictly removes branches. Bug 2's fix is either a
  tiny state-machine tweak in `parse_if_statement` or a single-branch addition
  in dispatch.
- **Test corpus.** The success criterion is the corpus run command from the
  user's report:
  `cargo run --bin oxabl check ~/legacy-fdm4/pcna-erp/<mod>/<file> --include-path /psc/dlc/ --include-path ~/legacy-fdm4/pcna-erp/ --preprocess`.
  Expect the ~30 backslash-regression files to become clean and the 11
  `KwElse` files to parse successfully.
- **Benchmarks.** No new benchmark warranted — the fixes are correctness only,
  covered by existing `lexer_bench` and `parser_bench` fixtures. If the
  `IF … END. ELSE` pattern is newly covered, extend an existing parser
  fixture rather than adding a new benchmark.

## System-Wide Impact

- **Interaction graph.** `read_string_literal` feeds every downstream token
  consumer. Loosening the backslash arm affects every string literal in the
  codebase, including preprocessor `&SCOPED-DEFINE` values (which are raw text
  to end-of-line, not lexed as strings — unaffected).
- **Error propagation.** Today, `Kind::Invalid` from an unterminated string
  bubbles up as a "Unexpected token Invalid" parser error. After Fix 1 these
  strings terminate naturally, so diagnostics revert to specific-position
  parser errors only when the source is genuinely malformed.
- **State lifecycle.** None — both fixes are pure functions over token state.
- **API surface parity.** `oxabl_lexer` is the sole tokenizer; no other crate
  reproduces this logic.
- **Integration test scenarios.**
  1. Lex `"\\"` in isolation — one `StringLiteral` whose span covers exactly 4
     bytes.
  2. Lex `"a\"b"` — one `StringLiteral` spanning 6 bytes **and** the assertion
     that downstream parsing treats `\"` as two literal bytes (backslash +
     quote), matching ABL semantics. Document that tilde escape
     (`"a~"b"`) is the supported form.
  3. Parse `if x then do: end. else do: end.` — one `Statement::If` with
     populated `then_branch` and `else_branch`.
  4. Parse the same pattern across preprocessor include boundaries to pin down
     whether include expansion preserves enough context for the parser to
     rebind ELSE.
  5. Re-run the full pcna-erp corpus post-fix and compare remaining failures
     against the baseline in the user's report.

## Acceptance Criteria

- [ ] Revert the backslash-escape tolerance in
      `crates/oxabl_lexer/src/lib.rs:601-606` so that backslash is always a
      literal byte inside string literals.
- [ ] Update the two tests touched by commit `40579e7`
      (`string_with_backslash_escaped_quotes`,
      `string_with_backslash_not_before_quote_is_literal`) to match the new
      behaviour; add a positive test for `"\\"` tokenising to a single literal
      of length 4.
- [ ] Add a reproducer test in
      `crates/oxabl_parser/src/parser/tests.rs` for
      `if x then do: end. else do: end.` at top-level and nested inside a
      `PROCEDURE`. Both must parse without error.
- [ ] Identify the real Fix-2 site via reproducer — document the site in the
      PR description and patch the minimum code needed.
- [ ] Re-run the corpus command from the user's report against all ~40 files
      listed under "Real parser bugs" in the session notes. Every file either
      parses cleanly or fails with a **different** error than the ones
      enumerated (a new error is acceptable; it signifies a separate latent
      bug rather than a regression of this fix).
- [ ] `cargo fmt --check`, `cargo clippy -D warnings`, `cargo test` all pass.
- [ ] CodSpeed shows no regression on `lexer_bench` and `parser_bench`.

## Success Metrics

- 0 regressions in existing lexer/parser test suite.
- ≥ 30 previously-failing corpus files now parse (the backslash bucket).
- ≥ 10 previously-failing corpus files now parse (the KwElse bucket).
- No new failures introduced in files that previously parsed clean.

## Dependencies & Risks

- **Risk — HTML/JS embedded strings regress.** Files relying on the `\"`
  tolerance may regain lexer errors. Mitigation: scan the corpus for `\"`
  occurrences before merging; if any legitimately rely on the tolerance,
  convert them in-repo (they are legacy source owned by the user) or document
  that the lexer follows official ABL and require a preprocessing pass.
- **Risk — Fix-2 scope creep.** If the reproducer fails to trigger in
  isolation, the defect is in preprocessor expansion. That is a larger change
  and should be split into a follow-up PR rather than expanded in-place.
- **No upstream dependencies.** Fixes live entirely in `oxabl_lexer` and
  `oxabl_parser`.

## Sources & References

### Internal references

- Lexer string termination: `crates/oxabl_lexer/src/lib.rs:558-616`
- IF parser: `crates/oxabl_parser/src/parser/statements.rs:2321-2371`
- DO parser: `crates/oxabl_parser/src/parser/statements.rs:2119-2320`
- Block body parser: `crates/oxabl_parser/src/parser/statements.rs:3888-3940`
- Regression commit: `40579e7 fix(lexer): accept backslash-escaped quotes inside string literals`
- Pre-regression baseline: `95cfc04 fix(lexer): backslash is not an escape character in ABL strings`
- Recent preprocessor-adjacent work (context for Fix-2 investigation):
  `57662c3` (comment bodies), `d47812d` (dynamic include names),
  `a1eff0b`, `5e2b811` (preprocessor diagnostics).

### Corpus

- Failing-file inventory captured in session: see user report of
  2026-04-16 against `~/legacy-fdm4/pcna-erp/`.
