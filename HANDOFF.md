# Handoff: trust-hardening the in-editor linter — first dogfood false positives fixed

**Date:** 2026-07-24
**Branch:** `master` — all of this session's work is **merged**. Tip is #109; the only open PR is #46 (the Release Please rollup).
**This session:** With the VS Code extension (#104) now merged and usable in a real editor, we started the trust-hardening thread the strategy points at: fix the confirmed lint/semantic false positives that dogfood surfaced, and add the client UX papercut fix. Three PRs, all merged — the two semantic false positives (#106, #107) and the `oxabl: Restart Server` command (#105).
**Prior context:** The VS Code client shipped and merged (#104): sideloadable VSIX, `oxabl lsp` for scoped `[abl]` format-on-save + push diagnostics, `oxabl.toml` schema completion, `oxabl schema` subcommand, and CI (e2e stdio handshake, `vscode-client` job, schema-drift guard). The formatter is editor-integrated end to end (#100); diagnostics with live `oxabl.toml`/`.df` reload (#90).

---

## Current state

| Item | Status |
|------|--------|
| VS Code extension (#104) | **Merged** — daily-usable in-editor. |
| #106 temp-table field scoping | **Fixed + merged (PR #110).** |
| #107 unqualified `FIRST-OF`/`LAST-OF` field | **Fixed + merged (PR #111).** |
| #105 `oxabl: Restart Server` command | **Shipped + merged (PR #109).** |
| #108 unresolvable-include-as-argument | Still open — deferred pending a fully-wired re-dogfood. |
| Held block-scope false positive | Still unfiled — reproduce in a workspace that *has* includes first. |

---

## What was implemented this session (all merged to `master`)

### #106 — temp-table fields scoped to their temp-table (PR #110)
**Root cause:** the declare pass in `oxabl_semantic` registered every temp-table `FIELD` symbol in the *enclosing program scope*. `SEM0001` keys duplicates on same-scope + same-namespace, so two temp-tables in one file each carrying a same-named field collided in the program scope's `Values` namespace (the analyze dump showed the field at `scope=0`). **Fix:** `declare_temp_table` now opens a dedicated `ScopeKind::TempTable` scope (child of the enclosing scope, owned by the `DEFINE TEMP-TABLE` statement) and binds that temp-table's fields there — so identical field names across different temp-tables no longer collide, while a field declared twice in the *same* temp-table still errors. Downstream field access was unaffected (it resolves via the buffer `table_id` link, not these program-scope symbols). Touched `scope.rs` (new variant), `resolve.rs` (push scope + declare fields into it), `oxabl_analyze/src/lib.rs` (`temp_table` scope label). Regression + true-positive-guard tests added.

### #107 — unqualified block field resolves in `FIRST-OF`/`LAST-OF` (PR #111)
**Root cause (broader than the issue title):** `resolve_expr_ident` in `oxabl_semantic/src/resolve.rs` resolved bare identifiers only against `Values`/`Buffers` plus a schema *table-name* fallback — there was no path to interpret a bare name as an **unqualified field** of a buffer in scope, so idiomatic bare break-field names (and bare fields in DISPLAY/IF/ASSIGN generally) fell through to `NotInScope`, which `undefined-symbol` (LINT0001) reports. Qualifying (`buffer.field`) took the separate field-access path and resolved fine. **Fix:** a final fallback (`resolve_bare_block_field`) that walks every buffer visible from the current scope and, when a schema is loaded, resolves the bare name to the first buffer whose backing table has that field, synthesizing the typed `Field` symbol via the existing `synth_field_symbol` path. It runs only **after** local values, buffers, built-ins, and schema table names (real declarations always win) and is gated on `Buffers` being a candidate namespace (call names never resolve as fields). No `FIRST-OF` special-casing. Three end-to-end tests in `oxabl_analyze` (repro, qualified control, unknown-bare-name true-positive guard).

### #105 — `oxabl: Restart Server` command (PR #109)
Contributes `oxabl.restartServer` ("oxabl: Restart Server") to the VS Code client: stops the running `LanguageClient` and starts a fresh one, re-running binary discovery (a newly-installed/repathed `oxabl` is picked up without a window reload) and clearing the crash-cap state. Also auto-restarts on `oxabl.enable` / `oxabl.server.path` / `oxabl.trace.server` change via `onDidChangeConfiguration`. No-ops gracefully when disabled or never started. `activate` now takes the `ExtensionContext` and registers command + config listener as disposables; start/stop are factored into shared `startClient`/`stopClient` helpers. The restart-trigger predicate lives in a new vscode-free `config.ts` so it is vitest-testable (matching the `server.ts` pattern); 9 new tests cover the predicate and the command/activation/settings manifest.

**Verification (each PR):** `cargo test --workspace`, `cargo clippy --workspace -- -D warnings`, `cargo fmt --all -- --check` green; client `pnpm run check` + `pnpm test` (20 tests) + esbuild build green. All fixtures synthetic — no corpus, no PII.

---

## Next

1. **Re-dogfood in a fully-wired workspace** (include paths + `.df` schema in `oxabl.toml`) to separate real bugs from config noise; then confirm/close **#108** and pin down the held "visible-earlier / undefined-later" block-scope false positive.
2. **The strategic thread: #102 — workspace-wide cross-file semantic resolution** (with #103 as the background-index fast-follow). The engine analyses one file at a time today, so every inherited member from a parent `.cls`, every `USING`-imported type, every `RUN` target, and every cross-file `SHARED` var resolves to `Unknown`/`External` → `undefined-symbol` false-positives on real OO ABL. #102 is explicitly the ceiling on lint effectiveness and **blocks #57** (public rule API) and any new rules. It's a genuine architectural piece (cross-file salsa graph, class/inherited-member index, includes-as-tracked-inputs with an expansion cache, invalidation model, AVM-parity-vs-explicit-"unknown" decision) — take it through `/ce-brainstorm` → `/ce-plan` before building.
3. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish (publisher identity, icon, CI publish).

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#109** | Merged — `oxabl: Restart Server` command (#105) |
| **#110** | Merged — temp-table field scoping fix (#106) |
| **#111** | Merged — unqualified `FIRST-OF`/`LAST-OF` field fix (#107) |
| #104 | Merged — VS Code extension + `oxabl schema` + CI |
| **#108** | Open — unresolvable-include-as-argument → misleading comma error (deferred) |
| **#102 / #103** | Open — cross-file resolution + background index (the recommended next thread) |
| **#57** | Open — public lint-rule API; blocked on #102 |
| `STRATEGY.md` | Dogfood-adoption metric + Linting track updated: real dogfood has begun and is driving trust-hardening |
