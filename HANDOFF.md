# Handoff: analyze-fidelity fix + clippy housekeeping — clearing the pre-102 backlog

**Date:** 2026-07-24
**Branch:** `master` is the base. This session's work is on branch `fix/seeding-example-doc-indent`, open as **PR #112** (not yet merged). The Release Please rollup (#46) remains the other open PR.
**This session:** Confirmed the next strategic thread is **#102 (cross-file resolution)** but agreed to first knock down the older pre-102 backlog and re-dogfood in a fully-wired workspace before committing to that architecture. Started on the backlog: fixed **#60** (synthetic schema field symbols never accumulated read/write counts) and folded in a pre-existing clippy-1.97 housekeeping fix that was blocking the full-workspace lint gate.
**Prior context:** The VS Code extension (#104) is merged and daily-usable in-editor — the dogfood loop that surfaced this session's and last session's fixes. Last session trust-hardened two semantic false positives (#106/PR #110, #107/PR #111) and shipped `oxabl: Restart Server` (#105/PR #109).

---

## Current state

| Item | Status |
|------|--------|
| VS Code extension (#104) | **Merged** — daily-usable in-editor. |
| #60 field read/write counts | **Fixed — PR #112 open** (not yet merged). |
| clippy doc-overindent in `seeding_inventory` example | **Fixed in PR #112** (was failing `--all-targets` on clippy 1.97). |
| #108 unresolvable-include-as-argument | Still open — deferred pending a fully-wired re-dogfood. |
| Held block-scope false positive | Still unfiled — reproduce in a workspace that *has* includes first. |
| #102 / #103 cross-file resolution | Open — the strategic thread, deliberately sequenced *after* the backlog + re-dogfood. |
| #57 public lint-rule API | Open — blocked on #102. |

---

## What was implemented this session (PR #112, open)

### #60 — accumulate read/write counts on schema field symbols
**Root cause:** schema-resolved fields synthesized a `Field` symbol but never bumped its counts. `resolve_field_access` discarded the access mode (`let _ = mode;`) and `field_resolution` never called `bump_count`, so a field referenced or assigned any number of times dumped as `read_count: 0, write_count: 0` in `oxabl analyze` — misleading data and a latent trap for any rule keyed off field usage (e.g. a future "how often is this field updated" rule). **Fix (Option 1 from the issue):** threaded the real `AccessMode` through `field_resolution` and bumped the resolved field symbol via the existing accumulator; the end-of-pass flush already writes counts back for every symbol in the table, synthetics included. Both qualified paths (`buffer.field` and bare `Table.field`) now count; the bare-block-field path from #111 already did. Touched `crates/oxabl_semantic/src/resolve.rs` only. New regression test `field_access_accumulates_read_and_write_counts` (an `ASSIGN` target + a bare read fold onto one synthesized symbol → read=1, write=1). The `synthetic_schema_symbols_not_reported` lint guard still holds — counts change but synthetics stay out of `unused-variable`.

### Housekeeping — clippy doc-list overindent (separate commit)
clippy 1.97's `doc_overindented_list_items` was failing `cargo clippy --all-targets -D warnings` on the `seeding_inventory` example's module doc (pre-existing on clean HEAD, unrelated to #60). Dedented the field-list continuation lines to the 2-space list-item indent so the full-workspace lint gate passes on newer toolchains. Kept as its own `chore(oxabl)` commit so only the `fix:` is release-triggering.

**Verification:** `cargo test --workspace`, `cargo clippy --workspace --all-targets -- -D warnings`, `cargo fmt --all -- --check` all green. All fixtures synthetic — no corpus, no PII.

---

## Next

1. **Merge PR #112**, then continue the pre-102 backlog.
2. **The next step: #55 — improve the public API.** The umbrella `oxabl` crate is incomplete and consumers face a lot of boilerplate. This is polish that pays off before the API surface grows further, and it's independent of the cross-file work. Highest-value items from the issue: (1, *Critical*) the `oxabl` umbrella re-exports ast/common/lexer/parser/workspace but **not** preprocessor/schema/semantic/lint/analyze — a consumer must pull those in as separate deps with separate version lines; (2/5/3, *High*) no `parse(source) -> Result<Program>` convenience (the CLI itself hand-rolls tokenize→`Parser::new` at `src/main.rs:172-183`), no `Diagnostic` renderer (CLI rolls its own inline), and two confusingly-named parser entry points (`parse_statements` bail-on-first vs `parse_program` error-recovery) with `Program` not re-exported at the top level. Medium/low items (streaming lexer API, virtual-span `From` bridge, schema auto-discovery, `Display`/`Serialize` on core types, leaky `pub` recovery helpers, borrowed-token parsing) can be triaged within the same pass. Take it through `/ce-brainstorm` → `/ce-plan` if the scope proves large; a first slice (umbrella re-exports + `parse()` + diagnostic renderer) may be small enough to `/ce-work` directly.
3. **Re-dogfood in a fully-wired workspace** (include paths + `.df` schema in `oxabl.toml`) to separate real bugs from config noise; then confirm/close **#108** and pin down the held "visible-earlier / undefined-later" block-scope false positive. Do this before starting #102 so the brainstorm inherits a clean list of genuinely-cross-file gaps.
4. **Then the strategic thread: #102 — workspace-wide cross-file semantic resolution** (with #103 as the background-index fast-follow). The engine analyses one file at a time today, so every inherited member from a parent `.cls`, every `USING`-imported type, every `RUN` target, and every cross-file `SHARED` var resolves to `Unknown`/`External` → `undefined-symbol` false positives on real OO ABL. #102 is the ceiling on lint effectiveness and **blocks #57** (public rule API). It's a genuine architectural piece (cross-file salsa graph, class/inherited-member index, includes-as-tracked-inputs with an expansion cache, invalidation model, AVM-parity-vs-explicit-"unknown" decision) — take it through `/ce-brainstorm` → `/ce-plan` before building.
5. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish (publisher identity, icon, CI publish).

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#112** | **Open** — #60 field read/write counts + clippy doc-overindent housekeeping (this session) |
| #109 | Merged — `oxabl: Restart Server` command (#105) |
| #110 | Merged — temp-table field scoping fix (#106) |
| #111 | Merged — unqualified `FIRST-OF`/`LAST-OF` field fix (#107) |
| #104 | Merged — VS Code extension + `oxabl schema` + CI |
| **#55** | Open — improve the public API (**the recommended next step**) |
| #108 | Open — unresolvable-include-as-argument → misleading comma error (deferred) |
| #56 | Open — dependency-extraction fidelity vs AVM (converges with #102) |
| **#102 / #103** | Open — cross-file resolution + background index (the strategic thread, sequenced after the backlog + re-dogfood) |
| #57 | Open — public lint-rule API; blocked on #102 |
| `STRATEGY.md` | Dogfood-adoption metric + Linting track: real dogfood has begun and is driving trust-hardening |
