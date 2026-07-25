# Open questions — #128 (credit reads and writes inside parser-skipped statements)

Planning for [issue #128](https://github.com/oxabl-project/oxabl/issues/128) is complete and the plan is implementation-ready. These are the questions the review surfaced that planning deliberately did **not** close. None of them blocks starting the work; each one changes a detail an implementer would otherwise have to guess at, or names a follow-up worth a decision before this ships.

The plan lives at `docs/plans/2026-07-24-001-fix-credit-reads-in-skipped-statements-plan.md`. That directory is gitignored by design, so the plan is local to this worktree only — this file carries what a reader needs without it.

**Since first draft:** a second review challenged whether this fix serves the project's direction — a tool whose authority comes from understanding ABL, rather than from staying quiet when unsure. Verdict: right triage, wrong ending. The plan now ships bounded, and questions 2, 5, and 6 below are closed as a result. The mechanism is unchanged; what changed is that the flag now has an owned, incremental retirement path instead of waiting on unscheduled work.

## What was decided, for context

The parser recognizes ~30 ABL statement forms it does not model, skips them, and returns `StatementKind::Empty`. The resolve pass has a no-op arm for `Empty`, so they credit no reads and no writes, and three count-gated lint rules false-positive as a result (LINT0002, LINT0005, LINT0006).

The fix: a new `StatementKind::Skipped { names: Vec<Identifier> }` variant carrying identifiers the parser harvested from the skipped token range; a best-effort resolve through a lookup path that cannot write the `references` side table; and a new `SymbolFlags` bit `TOUCHED_BY_UNMODELLED_STATEMENT` (bit 21) that the three rules treat as a reason not to fire. `read_count` and `write_count` stay exact — the bit is a separate uncertainty channel, deliberately not count inflation. Over-crediting is intentional: it can only lose diagnostics, never invent them.

Deferred to a follow-up issue: the same defect inside *modelled* statements that discard a tail (`DISPLAY ... WITH FRAME`, `RETURN ERROR expr`, stream-I/O `VALUE(x.y)`, `ASSIGN FRAME f`, and eight more — twelve skip-helper callers in total). Different carrier problem: the statement node is already occupied, so the harvest payload cannot ride on it.

## Open questions

### 1. A modelled write plus a skipped mention permanently loses a genuine dead store

The suppression is per-symbol and file-wide. If a variable has a real dead store *and* is mentioned anywhere by a skipped statement, LINT0006 goes quiet for it permanently — the flag wins over the modelled evidence. That is the safe direction, but it is also the single largest coverage cost of this design, and it is unbounded in real code.

Should LINT0006 keep firing when a modelled write exists and the skipped mention is only a keyword-shaped match (see question 2), or is blanket suppression correct until #126's CFG def-use records land? Planning chose blanket suppression as the conservative default.

### 2. ~~No signal tells us if the flag goes net-negative~~ — CLOSED

Closed by the second review, which pointed out the signal was sitting there unused: run the rules over the out-of-repo ABL corpus on both builds, diff the diagnostics, and classify a sample of what disappeared as killed-false-positive versus lost-true-positive. That is now R17/U10, and it gates the merge. If lost true positives win the sample, that inverts the plan's premise and stops the work.

The related worry — that this is the third stopgap bit waiting on unscheduled #126 — is also closed, but by a distinction rather than a measurement. The two existing bits need one monolithic piece of work to retire. This one drains incrementally: head-parse a form, its dispatch site stops emitting `Skipped`, and the flag's population drops with no change to the semantic pass or the rules. R18/U11 files and schedules that head-parsing work, and the flag's doc comment points at it rather than at #126.

### 3. Should suppression be opt-out?

A user who would rather accept the false positives in exchange for coverage currently has only one control: turn the whole rule off via `[workspace.lint]`. Is a finer control warranted — and if so, is it per-rule or global?

Leaning no, and the plan now says so explicitly: a knob for a mechanism we intend to drain is a knob we then have to keep. Left open because it is a product call, not a technical one.

### 4. Keyword-collision channel is accepted; confirm that is right

`Parser::can_be_identifier` is deliberately broad, admitting ~150 keyword kinds. So a variable legitimately named `value`, `format`, `label`, `frame`, `title`, `input`, or `date` gets flagged by any skipped statement merely using that word as an option keyword — e.g. `GET-KEY-VALUE SECTION "s" KEY "k" VALUE v-out.` harvests the bare `VALUE` token.

This was examined and accepted. The reasoning: the resolve lookup is the real gate, so a keyword token suppresses nothing unless a same-named variable is actually in scope — and when one is, the ambiguity is irreducible, because ABL lexes the user's variable `value` as `Kind::Value` everywhere. Narrowing to `Kind::Identifier` would reintroduce this issue's false-positive class for exactly those variables, and per-form keyword blacklists would mean modelling the grammar of statements we skip because we do not model it.

Flagged here because it is the decision most likely to be revisited, and because `value` / `format` / `frame` are genuinely common ABL variable names.

### 5. ~~Should `is_hazard` consult the shared `is_skipped` predicate?~~ — CLOSED

Yes. Keeping the diff narrow was too timid: two suppression paths that can drift apart as future exemptions are added is exactly the bug class the shared predicate exists to prevent. U4 now refactors `is_hazard` to go through `is_skipped` rather than adding a parallel clause, and "all three rules reach the exemption through one shared predicate" is a done-criterion.

### 6. ~~Does the analyze symbols section need a version bump for a new flag string?~~ — SUPERSEDED

Still unsettled as a versioning question, but no longer load-bearing. The reason it mattered was that the analyze dump was the only way a user could discover the flag — and R16/U9 now reports the count of unjudged symbols in `check` output directly, which is where someone would actually notice. Whoever owns the analyze section-versioning contract can still make the call, but nothing waits on it.

### 7. Should the harvest credit the `Streams` namespace?

The plan credits `NamespaceId::Values` only, because `unused_symbol_shared::is_candidate` restricts all three rules to `Variable` and `Parameter`. Streams go uncredited, which costs nothing today. If a stream-usage rule ever lands, it will want the same signal.

### 8. Convergence with the deferred follow-up

Once the modelled-statement-tail class is fixed, do the two mechanisms converge on one flag, or does the repo carry two suppression carriers indefinitely? Worth deciding when the follow-up issue is written, since it shapes that issue's design.
