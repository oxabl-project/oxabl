# Handoff: the cross-file population is judged (#102 follow-up) — valves opened, suppression drained

**Date:** 2026-07-31
**Branch:** `feat/judge-cross-file-population` — 10 commits on top of `7ba07dd`.
**This session:** Turned the lint rules onto the population cross-file resolution already reached, and drained the top of the unmodelled-statement suppression. The three type valves the previous session installed are **deleted**, `undefined-symbol` reports a name absent from every configured search path, and `DELETE OBJECT` / `COMPILE` no longer blind the count-gated rules. Four corpus A/Bs gate it; read **What this session changed** below, then **What is still deliberately closed** before you "finish the job" again.
**Previous session:** built #102 itself — the index consulted during resolve, adding no new diagnostic on purpose. That framing is now historical: the sections below marked *carried forward* still describe the mechanism accurately, but the firewall they describe is gone by design.
**Prior context:** #120 (PR #140, `58d961e`) built the seam this needed: one config resolution, one root-file policy, one result model. #133 (browser playground), #135 (panic-safe entry points), #137 (#128's crediting) and #130 (table-use crediting) shipped before it. The lint-accuracy map from #129/#128/#130 is carried forward below unchanged — it is still the thing to read before triaging a "LINT0006 is wrong" report.

---

## Current state

| Item | Status |
|------|--------|
| #102 cross-file semantic resolution | **Done and now judged.** The seam, the `oxabl_index` crate, the pipeline's run-level index and the LSP's per-file salsa inputs all shipped previously; this session opened the type valves and turned `undefined-symbol` onto absent names. |
| #152 LINT0004 renders `SymbolId(...)` | **Done — closed by the first commit on this branch.** `ResolvedType::display_abl` names ABL types; a test asserts no diagnostic message can contain an internal id. |
| #103 background index | Substantially absorbed — the language server's salsa-backed index *is* the incremental half. Re-scope or close. |
| #57 public lint-rule API | Open — was blocked on #102; now unblocked. |
| #120 CLI reshape onto shared pipelines | **Done — merged as PR #140 (`58d961e`).** `oxabl_pipeline` owns config resolution, both runs, and one result model; CLI, LSP and WASM are renderers. Dogfooded at zero drift. |
| #130 | Done — merged. Table-use forms credit a read on the table they name. |
| #128 / #137 | Done — merged. Standalone unmodelled forms credited (`StatementKind::Skipped` + `TOUCHED_BY_UNMODELLED_STATEMENT`). #136 is the scheduled drain. |
| #133 browser WASM adapter | Done — merged; now a transport adapter over `oxabl_pipeline` rather than over the umbrella directly. |
| #119 panic-safe parse/format | Done — shipped as #135. |
| #134 | Open — the last uncredited-read half: skipped *tails* inside modelled statements. |
| #136 | Open, **partly drained**, and **re-scoped on measurement.** `DELETE OBJECT` is head-parsed and `COMPILE`'s harvest is deleted; together they were 55% of the unjudged-symbol population. The issue now carries a ranked table of the rest (`PUT` ≈ 73%) and the two-shape taxonomy that decides each form's fix. |
| #108 unresolvable-include-as-argument | Open — the fully-wired re-dogfood it was waiting on **has now run**; still unconfirmed. See **Next** item 4. |
| #142 nested unresolvable include is silent | Open. A nested unresolvable include drops its `PREPROC007` in every shared-pipeline client while the `undefined-symbol` findings still fire. Was sequenced after #102, so it is **next in line** now. |
| #144 `oxabl check --watch` | Open. The only live-feedback path for developers who cannot host a language server (Progress editor, plain vim, notepad++). The index constraint it imposed is satisfied: `WorkspaceIndex` is a four-question trait with no editor-specific state, and a second incremental caller implements it the same way the LSP does. |
| #131 / #132 | Open — LINT0006 write-site span breadth; `oxabl_lint` benchmark coverage. |
| #125 | Open and **unblocked** — small, template is fresh. |
| #124 / #126 | Open — the rest of the flow-analysis cluster. |

---

## What this session changed

The plan is `docs/plans/2026-07-31-001-feat-judge-cross-file-population-plan.md` (local; `docs/plans/` is gitignored). Ten units, four phases, each phase gated on an A/B over a large real-world ABL codebase kept outside this repo. Report shapes as proportions — no absolute counts from it belong anywhere in the repo.

**Phase 0 — a readable message.** LINT0004 interpolated a `ResolvedType` with `{:?}`, so a buffer mismatch reached the user as `Buffer(SymbolId(7))` and a narrowing warning as `Primitive(Decimal)`. `ResolvedType::display_abl` pairs a type with the symbol table *and* the schema — a class or buffer carries a `SymbolId` only the former resolves, a table a `TableId` only the latter does — and renders ABL: `CHARACTER`, `DECIMAL EXTENT 3`, `buffer Customer`. It had to land first: a `SymbolId` in a message shifts whenever symbol counts change, and this session changes them throughout, so the LINT0004 half of every later A/B would have been noise. A/B: zero behavior deltas, every LINT0004 message re-rendered at an unchanged location.

**Phase A — the type valves.** An inherited member's declared type is on `Symbol::data_type` (`inherited_member_types` deleted). A declaration whose `AS CLASS` name resolves through the index carries `ResolvedType::Class` (`indexed_receiver_class` deleted; member resolution reads the declaration's own type instead). `check.rs` types an index-synthesized class as its class (the lattice-bottom arm deleted). One thing had to be *added* to make that safe: a synthesized class symbol now records the supertypes the index read from its header, because `ClassLattice` climbs through `SymbolTable::supertypes` and a class with no entry looks like a class that inherits nothing — which reported a subclass assigned into its parent-typed variable, a false positive on the one shape inheritance widening exists to allow. A/B: one group, `LONGCHAR` into `CHARACTER` through an inherited method, all true positives, at `warn`, no file newly carrying an error.

**Phase A2 — reporting an absent name.** Two commits, deliberately separate. First a pure refactor: `NotFoundInWorkspace` split into `AbsentFromWorkspace` (a genuine path-search miss) and `PresentButUnusable` (the name exists and cannot be used from here). It named four situations and only one was "absent" — harmless while every rule skip-listed the reason, false the instant a rule reported it. Telling a broken file from a missing one needed the seam to say so, hence `IndexAnswer::Unusable`, answered from the `parsed` flag both backends already had. R17 rode along: an index with no configured search path answers `NotFound` to everything without having looked anywhere, so a miss there stays `External` — derived beside `index_loaded` from a defaulted `WorkspaceIndex::searches_any_path`, which also keeps the browser's file-less export from diverging from the CLI. The A/B across that commit alone is **empty**, by construction.

Then the rule flip. `undefined-symbol` accepts `AbsentFromWorkspace` and carries a help line naming `[workspace.sources].include_paths`; its reason match is exhaustive and wildcard-free, so the next variant is a compile error in the rule that would have to judge it. **The first cut over-reported by more than twenty times, and every one of those findings was correct ABL** — that is the part worth carrying forward:

- `RUN name IN handle` reaches an entry point in another *running* program. No path could supply it → `Unknowable`.
- An extension-less `RUN name` means an internal procedure first, including any registered `SUPER PROCEDURE`'s. oxabl models neither, so the miss is inconclusive → `External`.
- A bare class name must be checked against the spellings the search actually **tried**, not its own: `NEW JsonObject()` under `USING Progress.Json.ObjectModel.*` is searched for qualified and recorded bare, so a carve-out that only looked at the reference name let the whole AVM class library through as absent.
- A located file that does not visibly declare its class answers `Unusable` — its declaration is usually spliced in from an `{include}`.

Two real defects surfaced on the way and are fixed rather than worked around: a literal `RUN` target accepted a dotted second segment anywhere on the same line, so `RUN write-header. RUN build-list.` produced the target `write-header. RUN` (adjacency to the period now distinguishes a dotted name from a terminator); and the name search applied the *walk's* extension set to a path the author spelled out, so a `.pp` program that plainly exists was reported absent (a `RUN` target now accepts any extension but `.i`, via `search::ExtensionPolicy`). An index's self-exclusion answers `Unusable` too, so a program that `RUN`s itself persistently is not told its own path is missing. A second leg with a real source root removed confirmed the misconfigured shape is diagnosable — findings cluster by the first path segment of the missing name — and exposed that `check --json` dropped `help`, now version 3.

**The `LINT0001` ratio was re-measured, and an earlier figure here was not reproducible.** This section previously recorded ×1.009 with "~97% of the additions a `RUN` of a program that exists nowhere". Both came from a run whose config could not be recovered: the `oxabl.toml` driving an out-of-repo corpus is discovered by walking up from each analyzed file, so it need not appear on the command line, and it is typically untracked — two runs days apart were driven by different include paths and nothing in the output said so. Re-measured against a pinned config: **`LINT0001` ×1.019, every other rule ×1.000**. Do not cite the old number. `scripts/lint-ab-diff.sh` now writes an input manifest per collection and refuses to diff two sides whose inputs disagree, so this cannot recur silently; quote a ratio together with the manifest that produced it.

**Phase B — draining the suppression.** `DELETE OBJECT` is head-parsed into `StatementKind::DeleteObject { target: Expression, no_error }`; the operand may be `ttbl:HANDLE` or `hArray[i]`, which is exactly why it skipped, and `DELETE PROCEDURE`/`WIDGET`/`SERVER` never did. `COMPILE` keeps emitting `Skipped` with an **empty** name list: its operand is a file path, so head-parsing would credit reads that do not exist, while harvesting suppressed real variables named `save` or after a path segment. A/B: **unjudged symbols fell by 55%**, and the finding deltas are confined to LINT0002/LINT0005/LINT0006 — 28 findings, all symbols that were previously unjudgeable — with no LINT0004 movement, which would have meant a type valve moved.

**Where the evidence lives.** Nothing from the corpus is in the repo. `#136` carries the measured ranking and the two-shape taxonomy; the A/B classifications are in the PR description.

---

## What shipped on this branch — #102

The plan is `docs/plans/2026-07-23-007-feat-workspace-resolution-plan.md` (local; `docs/plans/` is gitignored). Read the requirement labels (R6, R7, R11, R14, R17, KTD1/2/4/6/8) as they appear in module docs — they are the shorthand the code comments use.

**Correct the framing before you plan follow-up work.** #102's issue body claimed cross-file names false-positive on every inherited member. They never did: cross-file names were soft-resolved to `UnresolvedReason::External`, and `External` is skip-listed by every rule. The cost was **silence**, not noise — oxabl could not check a whole class of real code and said nothing about it. That is what shipped: the *capability*. The lint value is still to come.

**The mechanism, and why the committed sketch was not it.** `docs/design/semantic-v1-cross-file-sketch.md` is now marked **superseded** in place — read its banner rather than the body. It designed cross-file resolution as a post-hoc `CrossFileResolutions` side table computed after every per-file `Semantic` exists, read back through an `effective_resolution` wrapper. What shipped is an **index consulted during resolve**: the resolve pass asks the index at the moment a name fails locally and writes the hit into the ordinary `references`/`symbols` tables, so a cross-file resolution is shape-identical to a local one. The side table lost on three counts — it needs every file's `Semantic` to exist first (impossible on the per-keystroke path), it gives every consumer a second lookup to forget, and it is the wrong place to compute *why* a miss missed. The sketch's central claim (R10: no per-file public field reshaped) held anyway, which is why it was annotated rather than deleted; its two pinned tests still exist and pass.

**The seam.** `oxabl_semantic::index` defines `WorkspaceIndex` with exactly **four** queries — `class`, `class_members`, `program`, `shared_producer` — answering `Found` / `NotFound` / `Unusable` / `Unknowable`, plus `NullIndex`, the index that knows nothing. (`Unusable` and the defaulted `searches_any_path` were added this session — see **What this session changed**.) There are no client carve-outs: a client that cannot answer a question answers `NotFound`, it does not get a narrower trait. That is what makes "every client resolves identically, differing only in what files exist and how answers are cached" structural rather than documented.

**The new crate.** `oxabl_index` sits *beneath* `oxabl_pipeline`. `index_file` tokenizes, parses and declare-passes a referenced file, then projects it to `FileFacts` and drops the rest; `BatchIndex` is a plain in-run memo over it; `search` is public — deliberately — because the language server's cache must use the *same* name-to-path policy (two candidate spellings tried in order, **exactly one match** or `Unknowable`, `.i` never a root, no escaping the configured paths), and a private module would force that policy to be written twice.

**Decisions / gotchas future sessions should know:**

- **`WorkspaceIndex` carries no `Send + Sync` bound, and that is not an oversight.** The bound reads as harmless prudence and would rule out the only incremental implementation the seam exists to serve: a salsa-backed index answers by calling tracked queries, so it must borrow the database handle, and salsa makes a database `Send` but deliberately **not** `Sync`. Nothing is lost — `oxabl_index::BatchIndex` pins its own `Send + Sync` in a test, and the language server keeps its shared handle behind an `Arc`, building the borrowing `&dyn` view inside the query that uses it.
- **The unresolved-reason model is four-valued now.** It shipped three-valued, and `NotFoundInWorkspace` turned out to name four situations of which only one was "absent" — harmless while every rule skip-listed it, false the moment a rule reported it. See **What this session changed**. `AnalysisContext::index_loaded` still decides whether a miss is a fact about the workspace at all, and it is still **derived from the handle** rather than asserted — only `NullIndex` may report `IndexRevision::ABSENT` — so `with_index(&NullIndex)` cannot be talked into claiming a fact about a workspace nobody looked at.
- **A recovered parse yields no facts at all, not partial ones.** `index_file` returns `FileFacts::unparseable` if the parse recovered any error. Recovery resynchronizes on periods, so a broken statement can leave a class body missing members or a member carrying the wrong type — and a *wrong* fact mis-attributes symbols across the program graph, while a missing one just leaves a name unresolved and silent.
- **Path keys are lexically normalized, and that is load-bearing rather than tidy.** The memo key is a joined path derived from source text and `find_name` tries two spellings, so two lookups can reach one physical file under two strings. Keyed verbatim that mints **two** `IndexedFileId`s for one file — and `shared_producer`, which scans the memo and answers `Unknowable` when two *different* files define one `SHARED` name, would then report "cannot know" for a name with exactly one real producer.
- **Nothing in the index catches panics, deliberately.** Every query is total in its *answers*, but totality is not licence to swallow unwinding: `Cancelled` travels as a panic payload in this workspace, so a guard around a lookup turns a cancelled recompute into `NotFound` and freezes a buffer on stale results. Same reasoning that keeps `LintPipeline::expand`/`collect` unguarded.
- **The index reads nothing until a name is looked up**, which is why `LintPipeline::new` builds one unconditionally: it costs a pair of borrows and no I/O, so it is not another thing a client can forget. It is a **run-level** handle — reused across many edits of one buffer, or across every file of a walk via `with_file` (a per-file sibling borrows it rather than rebuilding) — and `with_index` lets the language server substitute its own.
- **`shared_producer` needed a seed, and the CLI walk is where it comes from.** A `SHARED` name maps onto no path and the filesystem trait exposes no listing, so the query can only answer from files the run has already indexed — and nothing pulls a producer in unless something happens to `RUN` it. `with_known_files` hands the index the list the walk already enumerated, read lazily on the first `SHARED` lookup, so the producer link works on the command line without a directory scan. Clients with no such list (the language server, the browser) simply do not call it.
- **The language server invalidates per file, not globally.** Each indexed file gets its own `IndexedFile` salsa input with a bumpable `disk_revision`; bumping one invalidates exactly that file's dependents. Reusing the single `SchemaHandle` revision every buffer already reads would have invalidated *every* open buffer on any dependency edit. **Salsa's own dependency graph is the reverse-dependency map** — there is no hand-maintained one to keep correct, which is the whole reason to spend the per-file inputs.
- **No `salsa` dependency may ever land in `oxabl_index` or `oxabl_pipeline`.** The pipeline depends on the index crate, the umbrella re-exports the pipeline unconditionally, and the browser bundle is built through the umbrella — so a `salsa` edge in either lands in the WASM payload. The language server's cache implements `WorkspaceIndex` *above* both, and is the only implementation allowed to know salsa exists.
- **No include expansion during indexing.** A declaration that only exists after an `{include}` splice is invisible to the index. Conservative on purpose: a missing fact yields a missing link, which by the firewall below produces no finding.
- **The parity suite was extended rather than relaxed.** A fixture row declares sibling files and the cross-file resolutions they enable, and every row is asserted **twice** — siblings withheld, siblings supplied — so the suite pins the *direction* of each effect. It shipped with three effects and now has six: this session added `Judged` (a finding cross-file resolution **produces**, which is why the withheld answer is no longer a superset of the supplied one) and `ResolvedFromWorkspaceMiss` (a finding that exists only because a path was *searched*, and therefore does not arrive on the browser's export, which configures no path). A finding may appear only where a `Judged` effect declares it, and a declared one that fails to arrive is also a failure. Sibling files are not a browser capability gap — the browser leg supplies a filesystem through an internal seam, so cross-file rows are fully comparable there.

### What is still deliberately closed

The firewall this section used to describe is **gone** — the three valves were deleted deliberately, each with its own A/B. What remains closed is one thing, and it is worth knowing before you read a silence as a bug:

**`check.rs` types every `MethodCall` and `MemberAccess` as `Unknown`, regardless of the symbol it resolved to.** So a cross-file type reaches the type lattice only through an *unqualified* reference: `v-flag = calc-total()` inside a subclass is judged, `v-flag = THIS-OBJECT:calc-total()` is not, and neither is `v-count = v-cache:fetch-label()`. Since `:`-qualified access is the ordinary OO-ABL spelling, this is the **larger half** of the population. It is scoped out on purpose — folding it in would have made this session's evidence unreadable — and it wants its own plan and its own A/B.

Three smaller silences, all pinned by tests that say why:

- **The `AS CLASS pkg.Missing` declaration spelling produces no `undefined-symbol`.** `DataType::Class` is a bare `String` with no `NodeId` and no span, so there is nothing to underline and nothing for the rule to see. Giving it a span follows the `StatementKind::Using` / `RunTarget::Literal` precedent in `ast-invariants.md` §1–§2.
- **A member behind an unexpanded `{include}` is `PresentButUnusable`, not absent.** The index does not expand includes, and a class whose body is spliced in is an ordinary ABL idiom — so a located file that does not visibly declare its class answers `Unusable` rather than `NotFound`. Without that, an include-heavy codebase fills with errors about code that is there.
- **Same-file `CLASS Child INHERITS Base` member resolution still does not work.** Pre-existing, untouched: the index is for *other* files.


## Carried forward: #120, the seam this was built on

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

**Verification at the time #140 merged:** `cargo test --workspace` green — **1731 passed, 0 failed, 2 ignored**. Superseded by the figures above.

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

Still the thing to read before triaging any "LINT0006 is wrong" report. **Two amendments from this session:** the population is smaller by 55% of its unjudged symbols (`DELETE OBJECT` head-parsed, `COMPILE`'s harvest deleted), and #136's prioritization is no longer a suggestion — the ranking is measured and posted on the issue.

**Standalone unmodelled forms are credited (#128/#137).** Around thirty ABL statement forms are recognized by the parser and then discarded — `PUT`, `EXPORT`, `UPDATE`, `SET`, `PROMPT-FOR`, `ENABLE`, `GET-KEY-VALUE`, `IMPORT`, `COPY-LOB`, `HIDE`, embedded SQL and more (the skip list in `oxabl_parser/src/parser/statements.rs` is authoritative). They used to reach `StatementKind::Empty` and credit nothing, so a variable whose only read lived in one of them looked write-only:

```abl
v-total = 42.
PUT v-total.        /* real read the model could not see → false LINT0006 */
```

Those forms emit `StatementKind::Skipped { names }` carrying the identifiers the skip passed over. The resolve pass best-effort-resolves them in `NamespaceId::Values` through `lookup_statement_ident`, a lookup that writes no side table, and records hits as `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT`. All three count-gated rules — LINT0002, LINT0005, LINT0006 — consult that flag through the one shared `is_skipped` predicate. `read_count` and `write_count` stay exact; the flag says the counts are *incomplete*, it does not fabricate an access.

**Know what this cost.** The mark is per-symbol and file-wide, so a genuine dead store on line 40 becomes unjudgeable because of an `ENABLE` mention on line 900. That is real evidence destruction, not just caution, and it is why the coverage count exists rather than letting a partly-checked file look clean. As of #120 that count reaches the audience that runs the gate: `check` carries it too, not just `analyze`. It is also why the retirement path is **scheduled**, not wished for.

**#136 is the actual fix, it is filed, and the counting has been done.** Variable traffic lives in each statement's *head*; the awkwardness lives in its *tail*. One shared parse-head/skip-tail combinator covers the large majority, and head-parsing a form buys exact lint crediting, formatter coverage (`tree.rs` routes these through pass-through arms today) and future LSP fidelity from one piece of work — where the flag buys only the first, once. The prioritization instrument this section proposed has been run: the issue now carries a table of harvested-name share per dispatch keyword, and the taxonomy that decides the *kind* of fix. **Symbol-shaped operands earn a head-parse; path- and literal-shaped ones earn a deleted harvest**, because crediting reads inside a file path invents references. `PUT` is ~73% of the remainder on its own.

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

The capability is now banked: the rules judge what the resolver reaches. The first two items are what is left of that arc.

1. **Type `:`-qualified member and method-call expressions.** The larger half of the cross-file population, and the one valve left closed — see **What is still deliberately closed**. `check.rs` types every `MethodCall` and `MemberAccess` as `Unknown` without consulting the symbol it resolved to, so the fix is small and its blast radius is not; it wants its own plan and its own A/B, and the tests that pin today's silence (`a_colon_qualified_call_on_this_object_stays_unjudged`, `a_mismatch_through_a_colon_qualified_member_stays_silent_at_the_call_site`) are what will say what changed.
2. **A week of in-editor dogfood before the new severities reach users.** `STRATEGY.md` records that in-editor use, not corpus runs, surfaced the first trust-eroding false positives — and a finding can be a true positive and still be one a developer does not want on every keystroke. The corpus A/B cannot substitute. `undefined-symbol` at *error* on an absent path is the one to watch: it is the finding whose cause is most often the reader's configuration rather than their code.
3. **Give `AS CLASS pkg.Missing` a span, then report it.** The one absent-name spelling still silent. Follows the `StatementKind::Using` / `RunTarget::Literal` precedent in `ast-invariants.md` §1–§2: a `NodeId` and a `name_span` on the type name, and the existing rule picks it up for free.
2. **#57 — public lint-rule API.** Was blocked on #102 and is now unblocked. Worth deciding whether a third-party rule sees the index at all, and if so through what.
3. **#142 — nested unresolvable include is silent.** Was explicitly sequenced *after* #102 because #102 rewrites the expansion path. That rewrite has happened, so this is unblocked and small: the CLI half is relaxing `expand_source`'s root-origin filter; the LSP half needs the primary span re-anchored to the include site.
4. **#103 — re-scope or close.** The language server's salsa-backed per-file index is the incremental half #103 described. Whatever remains (a warm cache across sessions, a background pre-index of the workspace) should be re-stated as a new issue rather than inherited.
5. **#134 — skipped tails inside modelled statements.** The one uncredited-read half left.
6. **#136 — head-parse the unmodelled forms.** Partly drained and now measured: `PUT` alone is ~73% of what remains, at a tidy 2 harvested names per statement, and the `OS-*` family plus `UNIX`/`PAUSE` are pure harvest-deletions worth ~8%. Two anomalies in the issue's new table deserve a look first — `CREATE` averaging 46 harvested names per statement, and `DEFINE`/`DEF` reaching the skip path at all.
7. **#108 — confirm or close it.** The fully-wired re-dogfood it was deferred pending has now run, and the collected `PREPROC007` evidence is the input; the check itself is still not done. The "suppress the downstream flood" item that used to sit here is **retracted, not unclaimed** — see the settled decision above.
8. **#125 — OUTPUT dead-store advisory.** Unblocked, small, and LINT0006's two-stage shape is a working template.
9. **#126 — CFG + dataflow scaffolding** absorbs and retires `PASSED_AS_OUTPUT_ARG` and `PARAM_TABLE_LIKE`; #124 waits on it. Check #126 before starting #131 — widening LINT0006's write-site walk form-by-form is exactly the per-shape treadmill def-use records exist to end.
10. **#132 — `oxabl_lint` benchmarks.** Still the only crate with no bench target, so no rule's cost is measured and CodSpeed cannot catch a regression in any of them.
11. **Playground follow-ups worth filing** (none filed yet): TypeScript typings for the wasm package (`--no-typescript` today); a wasm bundle-size budget in CI; and whether the second browser slice adds schema upload / a synthetic include map or stays deliberately single-file.
12. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#102** | **This branch** — cross-file resolution as a `WorkspaceIndex` consulted during resolve; new `oxabl_index` crate, per-file salsa inputs in the LSP, `dependencies` envelope section, three-valued unresolved reasons. **No rule behavior changed, deliberately** |
| **#103** | Substantially absorbed by this branch's salsa-backed index; re-scope or close |
| **#57** | Open — public lint-rule API; **unblocked** now that #102 has shipped |
| **#120 / PR #140** | Merged (`58d961e`) — one shared lint and format run behind every client; `oxabl_pipeline` beneath CLI/LSP/WASM, `check` becomes the gate, cross-client parity suite, dogfooded at zero drift. Built the seam #102 needed |
| **#130** | Merged — table-use forms credit a read on the table they name; `Skipped` gained `may_reference_tables` |
| **#128 / #137** | Merged — standalone unmodelled forms credited; `StatementKind::Skipped` + `TOUCHED_BY_UNMODELLED_STATEMENT` |
| **#134** | Open — the one remaining uncredited-read half; skipped tails inside *modelled* statements |
| **#136** | Open, **scheduled** — head-parse the unmodelled forms; the drain that retires #128's flag and #130's query approximation |
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
| #108 | Open — unresolvable-include-as-argument; the re-dogfood it waited on has run |
| **#142** | Open — a nested unresolvable include drops its `PREPROC007` in every shared-pipeline client. Was sequenced after #102, so **unblocked** |
| **#144** | Open — `oxabl check --watch`. Ships separately; the index constraint it imposed is satisfied — `WorkspaceIndex` holds no editor-specific state |
| #56 | Open — dependency-extraction fidelity vs AVM; converges with #102's index, which is now the thing to measure against |
| `docs/design/semantic-v1-cross-file-sketch.md` | **Superseded** by this branch, annotated in place rather than deleted — its R10 claim held, its post-hoc-side-table mechanism did not ship, and its two pinned tests still pass |
| `docs/design/ast-invariants.md` | Updated — §1 and §2 cover the `name_span` and the two non-wrapper `NodeId`s on `StatementKind::Using` / `RunTarget::Literal` |
| `STRATEGY.md` | The *Public API & client architecture* track carries the third top-level commitment (*one shared pipeline behind every client*) and the settled visible-CLI surface; both are delivered. The ≤50ms warm-cycle latency bet is measured at 2.72ms with the index attached |
