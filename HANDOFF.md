# Handoff: table-parameter FP fixed + dead stores split into LINT0006 (#129); next is cross-file resolution (#102)

**Date:** 2026-07-24
**Branch:** `master` — clean at `d539c31`. PR #129 merged; no open work branch from this session.
**This session:** Shipped **#129** — two false-positive classes in `unused-variable` (LINT0002). A `TABLE FOR`/`DATASET FOR` parameter no longer reports unused when the table it names is used, and the genuine dead store split out into a new rule **`assigned-but-never-read` (LINT0006)** reported at the assignment rather than the `DEFINE`. Dogfooded after merge: **false-positive count went down**, which was the target signal.
**Prior context:** #127 (LINT0002 OUTPUT-argument FP) shipped in the previous session and #129 builds directly on its `PASSED_AS_OUTPUT_ARG` flag. #55 (public API) shipped across PRs #113–#116.

---

## Current state

| Item | Status |
|------|--------|
| #129 table-parameter FP + LINT0006 split | **Done — merged, dogfooded (FP count down).** |
| #128 / #130 | **Open — filed from #129.** Uncredited *reads*; between them they own the one remaining known FP class. |
| #131 / #132 | **Open — filed from #129.** LINT0006 write-site span breadth; `oxabl_lint` benchmark coverage. |
| #125 | **Open and now unblocked** — see below. |
| #124 / #126 | Open — the rest of the flow-analysis cluster. |
| #127 LINT0002 OUTPUT-argument FP | Done — merged, dogfooded clean. |
| #102 / #103 cross-file resolution | Open — still the **top strategic thread**. |
| #57 public lint-rule API | Open — blocked on #102. |
| #108 unresolvable-include-as-argument | Open — deferred pending a fully-wired re-dogfood. |
| Held block-scope false positive | Partly addressed by #122/#123; re-check in a workspace that *has* includes. |

---

## What shipped this session — #129

Two findings, one rule, previously collapsed into each other:

```abl
/* was: "unused parameter ttItem" — every time, no matter how heavily used */
PROCEDURE emit-items:
  DEFINE INPUT PARAMETER TABLE FOR ttItem.
  FOR EACH ttItem: MESSAGE ttItem.ItemCode. END.
END PROCEDURE.

/* was: "unused variable v-total" pointed at the DEFINE, far from the mistake */
v-total = v-qty * v-price.   /* now: LINT0006, reported here */
```

`TABLE FOR tt` declares a `Parameter` in `NamespaceId::Values` while every reference to `tt` resolves through `NamespaceId::Buffers`, so the parameter's own `read_count` was permanently zero by construction. `SymbolFlags::PARAM_TABLE_LIKE` (`1 << 20`) marks the two `FOR` forms at declaration time, and LINT0002 redirects the read-count question to the declaration that actually collects the references. LINT0006 takes the write-only half; LINT0002 narrows to `write_count == 0`, so the two partition one population and a symbol yields exactly one diagnostic.

**Decisions / gotchas future sessions should know:**

- **`FOR EACH tt:` declares a *fresh block-scoped buffer symbol* and credits its reads there, not to the `DEFINE TEMP-TABLE`.** This is the single most important thing on this page. Planning assumed one upward `ScopeTree::resolve` from the parameter's scope would find the backing table; it does not, because those block scopes are *descendants* of the parameter's and invisible to an ancestor walk — and `FOR EACH` is the most idiomatic use of a table parameter, so the one-lookup version would have fixed almost nothing. It was caught empirically from the `oxabl analyze` symbol dump, not by reading code. `backing_read_count` now sums reads across every `Buffers` binding of the name in an ancestor-or-self **or** descendant scope. Any future change here must keep the descendant half.
- **The matching is name-keyed, not identity-keyed, and that is deliberate.** A same-named `DEFINE BUFFER tt FOR <other-table>` would credit the wrong table. The imprecision only ever produces *silence*, never a false claim, which is the correct error direction for a linter. Do not "fix" it into identity-keying without a real bug to point at — and note the shadowing case has no test yet (below).
- **Redirect, don't skip.** Exempting table-shaped parameters is one line and throws away the true positive: a routine that declares a table parameter and never touches the table still warns, and still should. A lookup that finds nothing means the table is not visible (typically an unresolvable include), so the rule stays silent rather than assert what it cannot prove. `DATASET FOR` always takes that silent path, because `DEFINE DATASET` declares into `Values` rather than `Buffers` — pinned by a test so it reads as a choice.
- **`is_table_like_param` is deliberately *outside* the shared `is_skipped` predicate.** `rules/unused_symbol_shared.rs` holds the exemptions both rules share so they cannot drift. The table-like check cannot live in there: LINT0006 skips those symbols outright, while LINT0002 must still report a genuinely-unused one. Folding it in would leave LINT0002 unable to consult the shared list at all. A future third rule in this family must call both.
- **Which write forms reach LINT0006 is an audited list.** Reported: plain assignment (incl. array elements), `ASSIGN` pairs, `MESSAGE ... UPDATE`/`SET`, `RUN ... PERSISTENT`/`ASYNCHRONOUS SET`, `BUFFER-COMPARE ... SAVE RESULT IN`, `CREATE <widget>`. Skipped: write-back `RUN` arguments (leaves #125 whole). Unreachable by construction: `DO` counters and `INPUT-OUTPUT` args (credited `ReadWrite`, so they always carry a read), buffer/table/field targets (not candidates).
- **`NodeId::DUMMY` bit again.** `Expression::new` carries `DUMMY`, which the `references` side table silently drops, so a hand-built assignment target never resolves and the diagnostic falls back to the declaration span. #127's handoff already warned about this and it still cost a debugging cycle — the new rule's tests use an `ident_expr` helper that allocates real ids. **Any** test asserting a write-site span must use it.
- **Two pre-existing assertions were deliberately inverted**, not one as planned: the unit test named for assignment-counting-as-write, and its end-to-end counterpart in `lint0002_output_argument.rs`. Both pinned the same old contract in different layers. Note that #127's handoff explicitly said "`write_count > 0` is not a usable signal and never will be" — #129 is exactly the change that made it one, by giving the write-only half its own code and span. That line is now superseded.

**Verification:** proof-first on the contract pair (LINT0002-silent + LINT0006-fires written and observed failing before either half was implemented). `cargo test --workspace`, `cargo clippy --workspace --all-targets -- -D warnings`, `cargo fmt --check` green; CI green on all seven checks including Schema drift. The `oxabl.toml` JSON schema was regenerated through the `oxabl schema` subcommand, not hand-edited. All fixtures synthetic.

---

## Known false positive that shipped — #128 and #130

Read this before triaging any "LINT0006 is wrong" report.

**A large set of ABL statements is invisible to the resolve pass**, so they credit no reads: the parser skips them to `StatementKind::Empty`. `PUT`, `EXPORT`, `UPDATE`, `SET`, `PROMPT-FOR`, `GET-KEY-VALUE`, `IMPORT`, `COPY-LOB`, `HIDE` and more (the skip list in `oxabl_parser/src/parser/statements.rs` is authoritative). A variable whose only read lives in one of them looks write-only:

```abl
v-total = 42.
PUT v-total.        /* real read the model cannot see → false LINT0006 */
```

This is a property of the semantic model, not of the new rule, and it **predates** #129 — on the prior master the same variable already warned as an unused variable, and a variable read *only* by a `PUT` still does. So a default configuration saw no new noise. The one real regression is narrow: a project that had set `unused-variable = "off"` to mute it gets it back under a code its config does not mention.

**Severity stayed `warn` deliberately.** Demoting LINT0006 to INFO would not fix the class — the identical FP stays loud under LINT0002 either way — and it would quietly demote every genuine dead store, which is the rule's whole population. Documented on all three affected rules (LINT0002, LINT0005, LINT0006) with the reproducing shape and the one-line suppression.

**#128** covers crediting reads in the parser-skipped forms and names two avenues; prefer the cheap one — have `skip_to_statement_end` record the identifier tokens it skips and best-effort-resolve them as `AccessMode::Read`. It over-credits reads, which for these rules yields only false *negatives*, and keeps the parser change small. Do not let that issue default to full per-form statement parsing. **#130** is the sibling: `DEFINE BUFFER b FOR tt`, `EMPTY TEMP-TABLE tt` and `DEFINE QUERY q FOR tt` *are* parsed but the use-walk credits no read to the table, so a table parameter used only that way still false-positives. Between them these two close the last known FP class in this family.

**Process lesson, recorded in #128:** the rule shipped with an audit requirement to enumerate every statement form that bumps `write_count`. That audit was done — but only for the *write* side. The predicate is `read_count == 0 && write_count > 0`, and the unaudited **read** side is where the false positives came from. **For any count-gated rule, audit both sides of the predicate, not just the side the new rule increments.**

---

## #125 is now unblocked

#125 (the "written by a callee via `OUTPUT` and never read" dead-store advisory) can ship as scoped, with no further prerequisites:

- LINT0006's shared skip-list consults `PASSED_AS_OUTPUT_ARG`, so callee-written symbols are explicitly excluded from the warning-severity rule. That territory is still entirely #125's — LINT0006 did not annex it.
- The machinery #125 needs already exists and is proven in production: the flag itself (#127), and LINT0006's two-stage shape (symbol-table candidate collection, then an AST walk to a reporting span) as a working template to copy.
- It does **not** need #126. The original framing had it waiting on CFG def-use records, but the opt-in INFO advisory it was scoped as needs neither a CFG nor definite assignment — just the flag plus the LINT0006 pattern.

Worth doing soon while the LINT0006 code is fresh; it is a small rule with a clear template.

---

## Cheap test additions worth picking up

Small, isolated, no design work — good warm-up tasks. All are gaps the #129 review surfaced and none block anything.

| Test | Why it matters |
|------|----------------|
| A same-named `DEFINE BUFFER tt FOR <other-table>` shadowing the parameter's name | Pins the name-keyed imprecision in `backing_read_count` as a deliberate choice rather than an accident. Currently the behavior is unpinned, so a future refactor could change it silently either way. |
| The upgrade combination `unused-variable = "off"` with `assigned-but-never-read` left at its default | This is the exact config that regresses when LINT0006 lands, and it is the one combination with no test. Existing tests only turn each rule off in isolation. |
| A dead store inside an `ON` trigger or `TRIGGER PROCEDURE` body | LINT0006's walk does not descend those bodies, so the diagnostic falls back to the declaration span. Span quality only — never a wrong claim — but currently unpinned. Related to #131. |

---

## Next

1. **#102 — workspace-wide cross-file semantic resolution** remains the top strategic thread (with #103 background index as the fast-follow). The engine analyses one file at a time, so inherited members from a parent `.cls`, `USING`-imported types, `RUN` targets, and cross-file `SHARED` vars all resolve to `Unknown`/`External` → `undefined-symbol` false positives on real OO ABL. #102 is the ceiling on lint effectiveness and **blocks #57**. Genuine architecture — take it through `/ce-brainstorm` → `/ce-plan` before building.
2. **#128 + #130 — uncredited reads.** Now the highest-value *lint-accuracy* work, and cheap relative to its payoff: between them they close the last known FP class, and #128 alone improves LINT0002, LINT0005 and LINT0006 simultaneously. Do #128 via the lexical fallback first.
3. **#125 — OUTPUT dead-store advisory.** Unblocked (above), small, and the template is fresh.
4. **#126 — CFG + dataflow scaffolding** still absorbs `PASSED_AS_OUTPUT_ARG` and `PARAM_TABLE_LIKE` and retires both, and #124 (path-aware LINT0005) waits on it. Worth planning once #102's shape is understood, since both touch the semantic model. Check #126's status before starting #131 — widening LINT0006's write-site walk form-by-form is exactly the per-shape treadmill def-use records exist to end.
5. **#132 — `oxabl_lint` benchmarks.** The only crate in the workspace with no bench target at all, so no lint rule's cost is measured and CodSpeed cannot catch a regression in any of them.
6. **Re-dogfood in a fully-wired workspace** (include paths + `.df` schema in `oxabl.toml`) to separate real bugs from config noise; then confirm/close **#108** and re-check the held block-scope false positive.
7. **#120** — when ready to reshape the CLI into a lint/format-first tool, do a `/ce-strategy` pass then a plan.
8. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#129** | **Merged** — table-parameter FP + LINT0006 dead-store split (this session) |
| **#128 / #130** | Open — uncredited reads (parser-skipped statements / table-use forms); the last known FP class |
| **#131** | Open — widen LINT0006's write-site walk beyond assignment and `ASSIGN` targets |
| **#132** | Open — `oxabl_lint` has no benchmark coverage at all |
| **#125** | Open — **unblocked**; callee-written dead-store advisory, still owns that flavor |
| #124 / #126 | Open — path-aware LINT0005 and the CFG scaffolding that retires both stopgap flags |
| #127 | Merged — LINT0002 OUTPUT-argument FP; #129 builds on its flag and supersedes its `write_count` note |
| #121 / #122 / #123 | Merged — preprocessor define-time refs, routine-scoped `DEFINE VARIABLE`, LINT0005 |
| #113 / #114 / #115 / #116 | Merged — the four #55 public-API waves |
| #55 | Improve the public API — done across the four waves; can be closed |
| #117 / #118 / #119 / #120 | Filed — deferred #55 follow-ups |
| #112 | Merged — #60 field read/write counts + clippy housekeeping |
| #104 | Merged — VS Code extension + `oxabl schema` + CI (the dogfood loop) |
| **#102 / #103** | Open — cross-file resolution + background index (**the strategic thread, next**) |
| #57 | Open — public lint-rule API; blocked on #102 |
| #108 | Open — unresolvable-include-as-argument → misleading comma error (deferred) |
| #56 | Open — dependency-extraction fidelity vs AVM (converges with #102) |
| `STRATEGY.md` | Public API & client architecture track; the umbrella is the shared client surface |
