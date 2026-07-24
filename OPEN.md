# Open questions — #128 (credit reads and writes inside parser-skipped statements)

Planning for [issue #128](https://github.com/oxabl-project/oxabl/issues/128) is complete and the plan is implementation-ready. These are the questions the review surfaced that planning deliberately did **not** close. None of them blocks starting the work; each one changes a detail an implementer would otherwise have to guess at, or names a follow-up worth a decision before this ships.

The plan lives at `docs/plans/2026-07-24-001-fix-credit-reads-in-skipped-statements-plan.md`. That directory is gitignored by design, so the plan is local to this worktree only — this file carries what a reader needs without it.

## What was decided, for context

The parser recognizes ~30 ABL statement forms it does not model, skips them, and returns `StatementKind::Empty`. The resolve pass has a no-op arm for `Empty`, so they credit no reads and no writes, and three count-gated lint rules false-positive as a result (LINT0002, LINT0005, LINT0006).

The fix: a new `StatementKind::Skipped { names: Vec<Identifier> }` variant carrying identifiers the parser harvested from the skipped token range; a best-effort resolve through a lookup path that cannot write the `references` side table; and a new `SymbolFlags` bit `TOUCHED_BY_UNMODELLED_STATEMENT` (bit 21) that the three rules treat as a reason not to fire. `read_count` and `write_count` stay exact — the bit is a separate uncertainty channel, deliberately not count inflation. Over-crediting is intentional: it can only lose diagnostics, never invent them.

Deferred to a follow-up issue: the same defect inside *modelled* statements that discard a tail (`DISPLAY ... WITH FRAME`, `RETURN ERROR expr`, stream-I/O `VALUE(x.y)`, `ASSIGN FRAME f`, and eight more — twelve skip-helper callers in total). Different carrier problem: the statement node is already occupied, so the harvest payload cannot ride on it.

## Open questions

### 1. A modelled write plus a skipped mention permanently loses a genuine dead store

The suppression is per-symbol and file-wide. If a variable has a real dead store *and* is mentioned anywhere by a skipped statement, LINT0006 goes quiet for it permanently — the flag wins over the modelled evidence. That is the safe direction, but it is also the single largest coverage cost of this design, and it is unbounded in real code.

Should LINT0006 keep firing when a modelled write exists and the skipped mention is only a keyword-shaped match (see question 2), or is blanket suppression correct until #126's CFG def-use records land? Planning chose blanket suppression as the conservative default.

### 2. No signal tells us if the flag goes net-negative before #126 lands

R14 adds a coverage-retention gate on a synthetic UI-heavy fixture — the three rules must still report seeded defects on variables that appear in no skipped statement, and at least one declared variable must carry no flag. That catches a grossly over-greedy harvest.

It does not tell us whether, on real ABL, the flag suppresses more true positives than false ones. What observable signal would? Nothing in the plan answers this, and the honest position is that we will not know.

Related: this is the **third** stopgap suppression bit (`PASSED_AS_OUTPUT_ARG`, `PARAM_TABLE_LIKE`, now `TOUCHED_BY_UNMODELLED_STATEMENT`) whose retirement is gated on the single unscheduled issue #126. Each bit widens the silently-suppressed population and nothing signals when one has outlived its cause. Worth deciding whether #126 gets scheduled before a fourth bit is added.

### 3. Should suppression be opt-out?

A user who would rather accept the false positives in exchange for coverage currently has only one control: turn the whole rule off via `[workspace.lint]`. Is a finer control warranted — and if so, is it per-rule or global?

### 4. Keyword-collision channel is accepted; confirm that is right

`Parser::can_be_identifier` is deliberately broad, admitting ~150 keyword kinds. So a variable legitimately named `value`, `format`, `label`, `frame`, `title`, `input`, or `date` gets flagged by any skipped statement merely using that word as an option keyword — e.g. `GET-KEY-VALUE SECTION "s" KEY "k" VALUE v-out.` harvests the bare `VALUE` token.

This was examined and accepted. The reasoning: the resolve lookup is the real gate, so a keyword token suppresses nothing unless a same-named variable is actually in scope — and when one is, the ambiguity is irreducible, because ABL lexes the user's variable `value` as `Kind::Value` everywhere. Narrowing to `Kind::Identifier` would reintroduce this issue's false-positive class for exactly those variables, and per-form keyword blacklists would mean modelling the grammar of statements we skip because we do not model it.

Flagged here because it is the decision most likely to be revisited, and because `value` / `format` / `frame` are genuinely common ABL variable names.

### 5. Should `is_hazard` consult the shared `is_skipped` predicate?

LINT0002 and LINT0006 get their exemption from `unused_symbol_shared::is_skipped`, which exists specifically so an exemption cannot be lost to a drifting copy. LINT0005 does not call it, so the plan adds a separate clause to `is_hazard`. Two suppression paths can now drift as future exemptions are added. Refactoring `is_hazard` to consult `is_skipped` is small; it was left out to keep the diff scoped.

### 6. Does the analyze symbols section need a version bump for a new flag string?

`symbol_flags_list` in `crates/oxabl_analyze/src/lib.rs` is a hand-maintained flag-to-string table that will not fail to compile when a bit is added. The plan adds the entry (R15) but does not settle whether flag-list growth is additive or needs a section version bump. Whoever owns the analyze section-versioning contract should call it.

### 7. Should the harvest credit the `Streams` namespace?

The plan credits `NamespaceId::Values` only, because `unused_symbol_shared::is_candidate` restricts all three rules to `Variable` and `Parameter`. Streams go uncredited, which costs nothing today. If a stream-usage rule ever lands, it will want the same signal.

### 8. Convergence with the deferred follow-up

Once the modelled-statement-tail class is fixed, do the two mechanisms converge on one flag, or does the repo carry two suppression carriers indefinitely? Worth deciding when the follow-up issue is written, since it shapes that issue's design.
