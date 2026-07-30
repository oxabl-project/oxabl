# Handoff: one shared lint and format run behind every client (#120); dogfooded at zero drift

**Date:** 2026-07-30
**Branch:** `master` — **PR #140 is merged** (`58d961e`). The branch it came from (`evanbrobertson/rework-check-analyze-into-shared-lint-format-pip`) is spent.
**This session:** Ran the A/B that decides whether #140 can merge, and it came back clean: **zero lint drift** against a large real-world ABL codebase kept outside this repo. Also fixed a silent defect the reshape introduced in `scripts/lint-ab-diff.sh`. Details under **Dogfood A/B** below.
**Prior context:** #140 implements #120 — the eleven-unit plan at `docs/plans/2026-07-24-001-refactor-shared-lint-format-pipelines-plan.md` (local; `docs/plans/` is gitignored). #133 (browser playground), #135 (panic-safe entry points), #137 (#128's crediting) and #130 (table-use crediting) all shipped before it. The lint-accuracy map from #129/#128/#130 is carried forward below unchanged — it is still the thing to read before triaging a "LINT0006 is wrong" report.

---

## Current state

| Item | Status |
|------|--------|
| #120 CLI reshape onto shared pipelines | **Done — merged as PR #140 (`58d961e`).** `oxabl_pipeline` owns config resolution, both runs, and one result model; CLI, LSP and WASM are renderers. Dogfooded at zero drift. |
| #130 | Done — merged. Table-use forms credit a read on the table they name. |
| #128 / #137 | Done — merged. Standalone unmodelled forms credited (`StatementKind::Skipped` + `TOUCHED_BY_UNMODELLED_STATEMENT`). #136 is the scheduled drain. |
| #133 browser WASM adapter | Done — merged; now a transport adapter over `oxabl_pipeline` rather than over the umbrella directly. |
| #119 panic-safe parse/format | Done — shipped as #135. |
| #134 | Open — the last uncredited-read half: skipped *tails* inside modelled statements. |
| #136 | Open and **scheduled** (`hermes`) — head-parse the unmodelled forms; retires #128's flag and #130's query approximation. |
| #108 unresolvable-include-as-argument | Open — the fully-wired re-dogfood it was waiting on **has now run**; still unconfirmed. See **Next** item 4. |
| #142 nested unresolvable include is silent | Open — filed this session. A nested unresolvable include drops its `PREPROC007` in every shared-pipeline client while the `undefined-symbol` findings still fire. Sequenced after #102. |
| #144 `oxabl check --watch` | Open — filed this session. The only live-feedback path for developers who cannot host a language server (Progress editor, plain vim, notepad++). Ships separately but **constrains #102**: the cross-file index must serve two incremental callers, so it must not assume editor-specific state. |
| #131 / #132 | Open — LINT0006 write-site span breadth; `oxabl_lint` benchmark coverage. |
| #125 | Open and **unblocked** — small, template is fresh. |
| #124 / #126 | Open — the rest of the flow-analysis cluster. |
| #102 / #103 cross-file resolution | Open — the **top strategic thread**, and now the clear next move: #120 built the seam it needs. |
| #57 public lint-rule API | Open — blocked on #102. |

---

## What shipped on this branch — #120

The case for this work was never diagnostic-mapping dedup. The clients' final hops genuinely differ and still do. The layer *above* those hops was not merely duplicated but **divergent**: three config resolutions that disagreed, two file walkers that disagreed on both extensions and case, and a flagship `check` that never entered the shared analysis at all while a source comment claimed it did. Those were wrong answers.

**The new crate.** `oxabl_pipeline` sits *beneath* `oxabl_lsp`, `oxabl_wasm` and the umbrella — it had to be a new crate, not a module in `oxabl`, because `oxabl` optionally depends on `oxabl_lsp`, so the LSP cannot depend back on the umbrella. It **must never gain a `salsa` dependency**: the umbrella re-exports it unconditionally and the browser bundle is built through the umbrella.

**Decisions / gotchas future sessions should know:**

- **`PipelineConfig::resolve` reads `oxabl.toml` exactly once** into include paths, lint severities, style and schema. Non-fatal problems come back as `ConfigWarning` **data**, so whether to surface them is the client's choice rather than an accident of which surface you are on. An inner `resolve_from_config` over an already-parsed value makes a second parse impossible by construction — don't add a convenience wrapper that re-reads the file.
- **`LintPipeline` exposes `expand`/`collect` as separate phases**, plus a composed `run`. The split exists because the LSP needs the intermediate for watcher matching and salsa early cutoff. **Only `run` is guarded.** The two phases are deliberately unguarded because salsa's `Cancelled` travels as a panic payload — a `catch_panic` inside them would swallow cancellation and publish stale diagnostics. This is the single most invertible-looking decision in the crate; leave it alone.
- **`FormatPipeline` takes a `StyleGuide` alone** — no filesystem, no include paths, and therefore **nowhere to put a preprocess flag**. "The formatter never sees expanded macros" is structural here, not documented-and-hoped-for. Its refusal variant carries the formatter's own `FormatFailure`, so #135's bail-versus-panic split survives without anyone string-matching a message.
- **`LintResult` distinguishes a run that computed zero diagnostics from one that never got to look**, keeps `labels`/`help`, and stays **byte-span-only**. Byte spans are the contract because the LSP's rope is the only correct position oracle under a negotiated encoding.
- **`position` gives byte-offset clients one line/column derivation** and documents why the LSP must *not* use it: a byte column is a different number from a UTF-16 column. The CLI's text output and the WASM wire shape both go through it, so those two cannot drift.
- **`check` is now the lint-and-format gate**, reporting lint findings and format drift in **two channels** that are never merged — a finding is span-anchored, drift is a per-file boolean, and merging them means synthesizing spans that do not exist. Plus a coverage line that never moves the exit code. A per-file internal panic is reported and the walk **continues** (exit 1, not `analyze`'s 4), with those failures under their own `--json` key so an oxabl bug stays distinguishable from an unused variable.
- **`check` preprocesses by default**, unlike `conformance` and `analyze`. A gate that does not expand includes reports every include-declared symbol as `undefined-symbol` — a flood about the caller's own correct code — and the LSP always preprocesses, so the default had to match or the gate and the editor would disagree on any project using an include.
- **Visible CLI is exactly `check`, `format`, `lsp`, `schema`.** `conformance` (the parse-conformance walk `check` used to be) and `analyze` are **hidden but fully supported and documented in the README** — a hidden undocumented command is an undiscoverable one. Exit codes are **not** uniformly 0/1/2: `analyze` also uses 4 (contained panic), 6 (serialize failure) and 7 (unsupported `--format`), and the whole contract is pinned by tests.
- **The analyze envelope now emits seven versioned sections.** `preproc` and `coverage` used to be keys the CLI spliced into the returned `Value` after the library handed it back; they are library-emitted now, which also made them visible in `--format text` for the first time. `coverage` is an **object** so the next coverage fact is an added key rather than an eighth section. The version map is one private helper because it had two call sites that could drift.
- **One root-file policy.** `discovery` owns it — `p`/`w`/`cls`/`v` matched case-insensitively, `.i` never a root — replacing two private walkers that disagreed on both the extension set and case sensitivity. The per-surface config helpers the shared resolver replaced were deleted, not left as deprecated shims.
- **The cross-client parity suite earned itself on its first run.** It asserts one source yields identical codes, severities, byte spans and sources through four entry points — composed vs two-phase run, the CLI binary, the LSP's salsa queries over a rope, and the WASM exports. It immediately caught a real shipped divergence: two default severity tables meant `unknown-table-or-field` and `type-mismatch-assignment` came back `error` in the browser and `warning` everywhere else under the same empty environment. One derived table now. Spans are compared as **bytes**, not rendered positions, so encoding conversion is never mistaken for a pipeline difference; and where a client is deliberately less capable the suite asserts the **capability is unavailable** rather than a different answer.

**Preprocessor include resolution — one settled decision and one real gap (#142).** Both verified against the binary this session, not inferred.

*Settled, do not "fix" it.* An unresolvable `{include}` emits a loud `PREPROC007` spanned on the include itself, and because the body is elided, each reference to a symbol it declared becomes an `undefined-symbol` error. Earlier revisions of this file called the unsuppressed findings a second gap with an unclaimed fix. **That was wrong and the framing is now retracted.** The findings are correct — the symbols are genuinely not declared in anything oxabl can see, exactly as unimported names are in any other language — and the `PREPROC007` is what explains them. Suppression would have to be per-file and coarse, i.e. the same evidence destruction as `TOUCHED_BY_UNMODELLED_STATEMENT`, which #136 exists to drain. Owner-confirmed 2026-07-30.

*The real gap, filed as #142.* A **nested** unresolvable include — reached through an include that resolves — emits no `PREPROC007` in `check`, `analyze`, the LSP, or WASM, while its `undefined-symbol` findings still fire. So those errors arrive with nothing naming the cause. Cause is `expand_source`'s `d.span.file == root` filter (`crates/oxabl_analyze/src/collect.rs:231`, and the fatal path at `:245`). Not an oversight: a nested span points into a file the client may have no text for. `conformance --preprocess` prints it because it never anchors the span, and `render_diagnostics` already falls back to `(in included file)` (`oxabl_common/src/diagnostic.rs:190-192`), so relaxing the filter serves the byte-offset clients; the LSP needs the primary span re-anchored to the include site (`expand_include` already has it as `site: FileSpan`) with the true location as a `Label` — a shape `Diagnostic.labels` already supports. Sequenced **after** #102, which rewrites this expansion path.

**Verification:** `cargo test --workspace` green — **1731 passed, 0 failed, 2 ignored** — plus `cargo clippy --workspace --all-targets -- -D warnings` and `cargo fmt --check`. All fixtures synthetic.

---

## Dogfood A/B — the merge evidence for #140

Run this session over a large real-world ABL codebase kept outside this repo, fully wired (include paths and a `.df` schema via `oxabl.toml`), comparing a `master` build against this branch. Counts stay out of this file; the shapes of the results are what matter.

**1. With identical explicit CLI inputs (`-I` + `--schema`), there is zero drift.** Every one of the six rules came back at an identical count, and the comparison was not on totals — it was on exact `(file, code, byte-span, message)` identity, where **nothing appeared and nothing disappeared**, no file had a differing per-rule count, and the unjudged-symbol totals matched exactly. That is the result a refactor introducing no rule behavior is supposed to produce, and it is worth knowing it was actually measured rather than assumed.

**2. The parity claim holds outside the test suite.** Per-file `analyze` and whole-tree `check` produced **identical sets** of `(file, code, byte-span, message)` tuples — zero on either side only, zero multiplicity mismatches. Separately, `check`'s format-drift channel named exactly the same file set as `oxabl format --check`. So the two channels and the two commands agree in the field, not just in `fixtures`.

**3. The config-resolution fix is the measurable payoff, and it is large.** Driven by `oxabl.toml` alone with no CLI flags — the realistic way a project runs this — `master` never loaded `[workspace.schema]` at all (`schema_revision=0`). Consequences: `undefined-symbol` was roughly **double** what it should be, because every schema table and field became a false positive, and `unknown-table-or-field` was **entirely dead**. On this branch the schema loads, those false positives are gone, that rule is live, and `type-mismatch-assignment` finds slightly more because schema-typed fields make more assignments checkable. Most importantly: **on this branch a config-driven run and an explicit-flag run are byte-identical.** Config is no longer a variable in the answer either — which is the commitment `STRATEGY.md` makes, now true of the environment and not only of the client.

**4. R24 verified in the field.** A handful of source files are not valid UTF-8. Both builds fail them identically (`exit 2`, pre-existing, not a regression), and `check` lists them under its own `failures` key and **keeps walking** — exactly the behavior R24 asked for, confirmed on real input rather than a fixture.

**Method note for whoever repeats this:** the in-repo script is serial and one process per file. Driving it at `-P 16` with batched files cuts a full pass to a couple of minutes, and whole-tree `check --json` is faster still — but see the script's own comment on why an A/B whose baseline predates #120 cannot use `check` on both sides.

**Found and fixed this session:** `scripts/lint-ab-diff.sh` read the coverage count as `d.get("unjudged_symbols", 0)`, but #120 moved it into the `coverage` section — so it silently reported **zero unjudged symbols on every file, forever**, with no error. This is the same defaulted-key failure class the plan caught for `scripts/corpus-ab-gate.sh` (R22/U11), just missed on the other consumer; `corpus-ab-gate.sh` and `.claude/skills/refine-oxabl-parser/SKILL.md` were both correctly migrated to `conformance`. The script now reads both envelope shapes — an A/B straddles the change by construction, since the two sides are different builds — and **exits 4 loudly** from the `diff` step if it recognizes neither, rather than defaulting. A stale comment claiming `check` "runs no lint at all" was corrected in the same pass.

**The class worth remembering:** a JSON consumer with defaulted key lookups fails *silently*, not loudly. When a `--json` shape changes, grep for consumers by **key name**, not just by command name — and prefer a loud failure over a plausible default in any tool whose whole job is to detect change.

---

## Lint-accuracy map, carried forward from #129/#128/#130

Untouched by this session; read this before triaging any "LINT0006 is wrong" report.

**Standalone unmodelled forms are credited (#128/#137).** Around thirty ABL statement forms are recognized by the parser and then discarded — `PUT`, `EXPORT`, `UPDATE`, `SET`, `PROMPT-FOR`, `ENABLE`, `GET-KEY-VALUE`, `IMPORT`, `COPY-LOB`, `HIDE`, embedded SQL and more (the skip list in `oxabl_parser/src/parser/statements.rs` is authoritative). They used to reach `StatementKind::Empty` and credit nothing, so a variable whose only read lived in one of them looked write-only:

```abl
v-total = 42.
PUT v-total.        /* real read the model could not see → false LINT0006 */
```

Those forms emit `StatementKind::Skipped { names }` carrying the identifiers the skip passed over. The resolve pass best-effort-resolves them in `NamespaceId::Values` through `lookup_statement_ident`, a lookup that writes no side table, and records hits as `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT`. All three count-gated rules — LINT0002, LINT0005, LINT0006 — consult that flag through the one shared `is_skipped` predicate. `read_count` and `write_count` stay exact; the flag says the counts are *incomplete*, it does not fabricate an access.

**Know what this cost.** The mark is per-symbol and file-wide, so a genuine dead store on line 40 becomes unjudgeable because of an `ENABLE` mention on line 900. That is real evidence destruction, not just caution, and it is why the coverage count exists rather than letting a partly-checked file look clean. As of #120 that count reaches the audience that runs the gate: `check` carries it too, not just `analyze`. It is also why the retirement path is **scheduled**, not wished for.

**#136 is the actual fix, and it is filed and labelled.** Variable traffic lives in each statement's *head*; the awkwardness lives in its *tail*. One shared parse-head/skip-tail combinator covers the large majority, and head-parsing a form buys exact lint crediting, formatter coverage (`tree.rs` routes these through pass-through arms today) and future LSP fidelity from one piece of work — where the flag buys only the first, once. `StatementKind::Skipped` doubles as the instrument for prioritizing: count which dispatch keywords actually produce `Skipped` nodes on real code and work the list in frequency order.

**#130 closed the table half.** Five forms name a table without reading a field of it and now credit a read on it: `DEFINE BUFFER b FOR tt` and `DEFINE PARAMETER BUFFER b FOR tt` by direct AST resolution, `EMPTY TEMP-TABLE tt` via a `Skipped` node carrying its exactly-parsed table name, and `DEFINE QUERY` / `OPEN QUERY` via lexical harvest. Durable gotchas:

- **`StatementKind::Skipped` carries `may_reference_tables`.** A marked node keeps #128's value-namespace treatment unchanged and *additionally* resolves the same names in `[Buffers, Tables]` as `AccessMode::Read`. The two paths are independent on purpose: a token can resolve in both namespaces under shadowing, and they record different facts. Only three forms set the marker.
- **The same-name guard is load-bearing.** `DEFINE BUFFER Customer FOR Customer.` is the standard block-scoping idiom, and the declare pass has already bound the new buffer under that folded name. An unguarded lookup credits the buffer a read *for existing*, silencing every count-gated rule for that symbol.
- **Both credit paths go through `resolve_statement_ident`**, which is silent on a miss — that is what lets the over-inclusive query harvest run without inventing a `references` entry or an `undefined-symbol` per stray token. Over-crediting can silence a diagnostic but cannot invent one.
- **A bare *schema* table is credited by nobody.** `synth_table_buffer_symbol` inserts into the `SymbolTable` without binding into the `ScopeTree`, and nothing declares into `NamespaceId::Tables` — so `DEFINE BUFFER bCust FOR Customer.` under a loaded schema credits nothing, exactly as `FIND` and `FOR EACH` leave it. Deliberate; pinned by `schema_only_targets_retain_current_no_credit_behavior`.
- **A new lookahead in this parser should assume a comment can sit in the gap.** A comment between `OPEN` and `QUERY` defeated the marker in the first cut because the split used a raw one-token lookahead; it uses `peek_nth_non_comment` now.

**One sibling remains.** **#134** — skipped *tails* inside modelled statements (`DISPLAY … WITH FRAME` options, `DEFINE VARIABLE … VIEW-AS`, `RUN` trailing content, and ten more): same root cause, but the statement node is already occupied so neither #128's nor #130's payload shape transfers. Pinned by a deliberately-failing-later test in `crates/oxabl_lint/tests/issue128_skipped_statement_reads.rs`.

Other facts worth keeping:

- **`FOR EACH tt:` declares a fresh block-scoped buffer symbol and credits its reads there**, not to the `DEFINE TEMP-TABLE`. Those block scopes are *descendants* of a parameter's scope and invisible to an ancestor walk, so `backing_read_count` sums reads across ancestor-or-self **and** descendant `Buffers` bindings. Any change here must keep the descendant half.
- The backing-table matching is **name-keyed, not identity-keyed, deliberately**. The imprecision only ever produces silence, never a false claim. The shadowing case still has no test.
- **`is_table_like_param` is deliberately outside the shared `is_skipped` predicate** in `rules/unused_symbol_shared.rs` — LINT0006 skips those symbols, LINT0002 must still report a genuinely-unused one. A future rule in this family must call both. `is_skipped` has three callers, so there is exactly one suppression path to keep correct.
- **`NodeId::DUMMY`**: `Expression::new` carries `DUMMY`, which the `references` side table silently drops. Any test asserting a write-site span must use the `ident_expr` helper that allocates real ids.
- **`ParameterType::Buffer` records the wrong target in an inline procedure signature.** `crates/oxabl_parser/src/parser/mod.rs:1567` — the `PROCEDURE p (BUFFER b FOR tt)` path parses the table name then discards it with `.ok()`, setting `target` to the *buffer's own name*. So that shape never links its schema table, and #130's credit is skipped by the same-name guard. Filed, pre-existing, not fixed.
- **Process lesson:** for any count-gated rule, audit **both** sides of the predicate. #129's audit covered the write side it incremented; the false positives came from the unaudited read side.

### Cheap test additions still worth picking up

| Test | Why it matters |
|------|----------------|
| A same-named `DEFINE BUFFER tt FOR <other-table>` shadowing the parameter's name | Pins the name-keyed imprecision as a deliberate choice; currently unpinned either way. |
| `unused-variable = "off"` with `assigned-but-never-read` left at default | The exact config that regressed when LINT0006 landed, and the one combination with no test. |
| A dead store inside an `ON` trigger or `TRIGGER PROCEDURE` body | LINT0006's walk doesn't descend those bodies, so the span falls back to the declaration. Span quality only. Related to #131. |

---

## Carried forward: the browser client (#133), now over the shared pipeline

`crates/oxabl_wasm` exposes three `#[wasm_bindgen]` exports — `analyze_source`, `format_source`, and `version()` (crate version plus a build identifier, so a crash report names the exact artifact a hand-vendored copy is running). It contains **no ABL behavior**; it is a transport adapter over `oxabl_pipeline` and must stay one. A refusal collapses to one `error` string because the wire shape has one field for it, while the pipeline keeps bail and contained panic apart for clients that can tell them apart.

- **The umbrella's `cli` feature is on by default.** `clap`, `walkdir`, `indicatif`, `schemars`, `serde_json` and `oxabl_lsp` are optional behind it. **Any new native-only dependency added to `crates/oxabl` must go behind `cli` or the wasm build breaks** — the CI `WebAssembly client` job is what catches this.
- **`wasm-bindgen` is pinned exactly (`=0.2.126`), not as a caret range**, because browser crash recovery rides on `__wbg_reset_state` — generated glue, not a semver-stable API. Three pin sites move together: the crate, `scripts/build-wasm.sh`, and `.github/workflows/release.yml`. A CLI/crate mismatch fails at bindgen time, *after* `cargo build` succeeds, so CI will not catch it — only the script or a release will.
- **`catch_panic` is a documented pass-through on `wasm32-unknown-unknown`** (stable Rust builds `-Cpanic=abort` there), so routing the browser through a "guarded" entry point buys it nothing. Recovery is the panic hook plus `__wbg_reset_state`, enforced only by `scripts/build-wasm.sh`'s two assertions — that the export exists, and that no exception-handling instructions were injected (which would raise the browser floor to roughly Chrome 128 / Firefox 131 / Safari 18.4, where the module fails to *instantiate* rather than degrading). **No CI job runs `wasm-bindgen` at all.**
- **`--verify` adds a `debug_panic()` export** for manual crash-path checks and must never ship; the release workflow does not use it.
- **The MVP's absent capabilities are absent on purpose, not stubbed** — no include resolution, no `.df` schema, no `oxabl.toml`. A capability the browser cannot honestly provide stays *unavailable* rather than getting a second, divergent implementation. The parity suite asserts unavailability, not a different answer.
- **The wire shape is not a stable contract**, same as `--json` on `check`/`analyze`. Don't let a website expectation freeze it.

---

## Next

#120 was the last item blocking a clean run at the strategic thread, and the A/B discharged the "don't reshape delivery while diagnostics are still wrong" risk for good.

1. **#102 — workspace-wide cross-file semantic resolution** is now the top item, not just the top *strategic* one (with #103 as the fast-follow). **Requirements are settled and every open question is closed** — the corrected plan is `docs/plans/2026-07-23-007-feat-workspace-resolution-plan.md` (13 requirements, 4 acceptance examples, 14 session-settled decisions), and a ready-to-paste goal prompt with the eight hard constraints is at `docs/plans/2026-07-30-001-goal-workspace-resolution.md`. Both are local; `docs/plans/` is gitignored. Next step is `/ce-plan` on the plan, not implementation. The engine analyses one file at a time, so inherited members from a parent `.cls`, `USING`-imported types, `RUN` targets and cross-file `SHARED` vars never resolve. **Correct the justification before planning it:** #102's issue body and the existing requirements plan both claim this false-positives on every inherited member. It does not. Cross-file names are deliberately soft-resolved to `UnresolvedReason::External` (`oxabl_semantic/src/resolve.rs:1920-1933`, `:2032`, `:2052`) and `External` is skip-listed by every rule (`oxabl_lint/src/rules/undefined_symbol.rs:6-8`, and the same in LINT0003/LINT0004). **The cost today is silence, not noise** — oxabl cannot check a whole class of real code and says nothing about it. That still makes it the ceiling on lint value and it still **blocks #57**, but it is a capability gap, not trust repair, and the difference changes what success looks like: the old success criterion ("`undefined-symbol` no longer fires on inherited members") is already vacuously true. #120 built the seam it needs: one config resolution, one root-file policy, one result model. Take it through `/ce-brainstorm` → `/ce-plan` before building — it is weeks of architecture.
2. **#134 — skipped tails inside modelled statements.** The one uncredited-read half left.
3. **#136 — head-parse the unmodelled forms.** Scheduled (`hermes`). The real drain: retires #128's file-wide flag *and* #130's query approximation, and picks up formatter and LSP coverage on the way.
4. **#108 — confirm or close it.** The fully-wired re-dogfood it was deferred pending has now run, and the collected `PREPROC007` evidence is the input; the check itself was not done this session. Related: **#142** (nested-include silence, described above). The "suppress the downstream flood" item that used to sit here is **retracted, not unclaimed** — see the settled decision above.
5. **#125 — OUTPUT dead-store advisory.** Unblocked, small, and LINT0006's two-stage shape is a working template.
6. **#126 — CFG + dataflow scaffolding** absorbs and retires `PASSED_AS_OUTPUT_ARG` and `PARAM_TABLE_LIKE`; #124 waits on it. Check #126 before starting #131 — widening LINT0006's write-site walk form-by-form is exactly the per-shape treadmill def-use records exist to end.
7. **#132 — `oxabl_lint` benchmarks.** Still the only crate with no bench target, so no rule's cost is measured and CodSpeed cannot catch a regression in any of them.
8. **Playground follow-ups worth filing** (none filed yet): TypeScript typings for the wasm package (`--no-typescript` today); a wasm bundle-size budget in CI; and whether the second browser slice adds schema upload / a synthetic include map or stays deliberately single-file.
9. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#120 / PR #140** | **This branch** — one shared lint and format run behind every client; `oxabl_pipeline` beneath CLI/LSP/WASM, `check` becomes the gate, cross-client parity suite, dogfooded at zero drift |
| **#130** | Merged — table-use forms credit a read on the table they name; `Skipped` gained `may_reference_tables` |
| **#128 / #137** | Merged — standalone unmodelled forms credited; `StatementKind::Skipped` + `TOUCHED_BY_UNMODELLED_STATEMENT` |
| **#134** | Open — the one remaining uncredited-read half; skipped tails inside *modelled* statements |
| **#136** | Open, **scheduled** — head-parse the unmodelled forms; the drain that retires #128's flag and #130's query approximation |
| **#102 / #103** | Open — cross-file resolution + background index; **now the top item**, and #120 built its seam. Requirements plan exists at `docs/plans/2026-07-23-007-feat-workspace-resolution-plan.md` (local, gitignored) but predates #140 and carries the retracted false-positive framing |
| #119 | Merged as #135 — panic-safe parse/analyze/format plus browser crash recovery |
| #133 | Merged — browser WASM adapter + playground; now a renderer of `oxabl_pipeline` |
| #131 | Open — widen LINT0006's write-site walk beyond assignment and `ASSIGN` targets |
| #132 | Open — `oxabl_lint` has no benchmark coverage at all |
| #125 | Open — **unblocked**; callee-written dead-store advisory |
| #124 / #126 | Open — path-aware LINT0005 and the CFG scaffolding that retires the two older stopgap flags |
| #129 | Merged — table-parameter FP + LINT0006 dead-store split |
| #127 | Merged — LINT0002 OUTPUT-argument FP |
| #121 / #122 / #123 | Merged — preprocessor define-time refs, routine-scoped `DEFINE VARIABLE`, LINT0005 |
| #113 / #114 / #115 / #116 | Merged — the four #55 public-API waves |
| #55 | Improve the public API — done across the four waves; can be closed |
| #117 / #118 | Filed — deferred #55 follow-ups |
| #104 | Merged — VS Code extension + `oxabl schema` + CI (the dogfood loop) |
| #57 | Open — public lint-rule API; blocked on #102 |
| #108 | Open — unresolvable-include-as-argument; the re-dogfood it waited on has run |
| **#142** | Open — filed this session; a nested unresolvable include drops its `PREPROC007` in every shared-pipeline client. Sequenced after #102 |
| **#144** | Open — filed this session; `oxabl check --watch`. Ships separately, constrains #102's index design |
| #56 | Open — dependency-extraction fidelity vs AVM (converges with #102) |
| `STRATEGY.md` | The *Public API & client architecture* track carries the third top-level commitment (*one shared pipeline behind every client*) and the settled visible-CLI surface; both are now delivered rather than planned |
