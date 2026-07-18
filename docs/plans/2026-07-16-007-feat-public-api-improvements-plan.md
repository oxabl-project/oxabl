---
title: "feat: Public API improvements (GitHub #55) — phased"
type: feat
status: ready
date: 2026-07-16
origin: GitHub #55
branch: feat/public-api-phase-N  # one branch per phase below
---

# feat: Public API improvements (#55) — phased plan

## Context

#55 is a large umbrella of consumer-integration gaps collected while wiring
oxabl into a real dependency-graph + schema-aware lint tool. Items span
packaging, diagnostics UX, preprocessor/include ergonomics, AST completeness,
and semantic span fidelity. Treating it as one PR is wrong; this plan splits
it into shippable phases ordered by **unblocks-consumers / effort**.

Items already addressed or substantially reduced elsewhere (do not re-plan):

| #55 item | Status |
|----------|--------|
| Built-in / schema resolution (related #58) | Shipped: builtins, abbreviations, schema-backed resolve, include-path config |
| Schema on analyze CLI | Shipped: `oxabl analyze --schema` |
| Include path config | Shipped: `oxabl.toml` + `resolved_include_paths` + PREPROC007 |

## Goals

- Make oxabl usable as a library without forking or wrapping every layer.
- Prefer additive APIs; keep `lint_file` / `analyze_file` / `tokenize` stable.
- Each phase is independently mergeable with tests and, where relevant, benches.

## Non-Goals

- Full LSP / IDE protocol.
- Cross-file SHARED resolution (separate roadmap).
- Replacing the AVM compiler.

---

## Phase A — Packaging & compliance (~0.5d) — *ship first*

| Item | Work |
|------|------|
| **24** LICENSE file | Add root `LICENSE` (MIT) matching crate manifests; GitHub license detection. |
| **23** Publish remaining crates | Publish `oxabl_preprocessor`, `oxabl_schema`, `oxabl_semantic`, `oxabl_workspace` (or document git-pin-only until ready). Prefer publish once APIs stabilize through Phase B/C. |
| **1** (partial) Umbrella re-exports | Expand `oxabl` crate re-exports so consumers depend on one crate for the common pipeline (lexer → preproc → parser → semantic → lint). |

**Exit:** repo shows MIT on GitHub; a consumer can `oxabl = "x.y"` and reach
analyze/lint types without five git deps.

---

## Phase B — Diagnostics & spans (~2–3d) — *highest consumer pain*

| Item | Work |
|------|------|
| **5** Diagnostic renderer | Move CLI rendering into `oxabl_common` (or `oxabl_diagnostics`): `fn render(diag, sources) -> String` / `Display` with `file:line:col` + snippet. CLI becomes a thin caller. |
| **20** Virtual span resolution | Wire `PreprocessedFile::resolve` into `oxabl_semantic::resolve_span` (and lint diagnostic construction). Semantic/lint diags on include-expanded code must point at real files. |
| **6** Span bridging | `From<(FileId, Span)> for FileSpan`; document VirtualSpan → FileSpan path. Stop hardcoding `FileId::new(1)` patterns in examples. |
| **15** Node spans | Prefer a **parser-produced `NodeId → Span` side table** (matches side-table philosophy) over bloating every `Statement`/`Expression` with a span field. Expose `fn span_of(id) -> Option<Span>` from parse result wrapper. |

**Exit:** a multi-include file produces correct diagnostic locations through
`analyze_file` + lint without consumer span math.

**Depends on:** Phase A optional; B is valuable alone.

---

## Phase C — Preprocessor / include surface (~2–3d) — *dependency-graph consumers*

| Item | Work |
|------|------|
| **11** Resolved include paths | Public accessor: `resolved_includes() -> &[(FileId, PathBuf)]` (or expose `sources` with paths, not only `FileId` + text). Today consumers wrap `FileSystem` to spy on resolve. |
| **12** Structured missing includes | Diagnostic field or `missing_includes: Vec<...>` — not message-string scraping. Align with PREPROC007 loudness. |
| **13** Case-insensitive include option | Config flag on resolver / `RealFileSystem` fold-and-scan for case-insensitive FS mismatch (Linux vs Windows ABL practice). |
| **14** Encoding seam | `FileSystem::read` bytes→str hook or `RealFileSystem` encoding option (Windows-1252 common). |
| **25** Non-taken branch includes | `skipped_includes` / `unexpanded_references` on `PreprocessedFile` for dead `&IF` branches — over-approx for build invalidation (#56). |

**Exit:** dependency extractor can build include graph + unresolvable set from
public API alone, with conservative over-approx for dead branches.

**Overlaps:** #56 items 1, 3, 5. Coordinate so C lands the *API* and #56 lands
the *fidelity harness*.

---

## Phase D — AST / lexer hygiene (~2d)

| Item | Work |
|------|------|
| **16** `LockType::Unspecified` | Distinguish absent lock clause from explicit `SHARE-LOCK`. Default runtime behavior can still derive ShareLock; AST preserves written form. |
| **17** `ForEach.no_error` | Align with `Find` / `CanFind`. |
| **18** Traversal helpers | `visit_statements` / `visit_expressions` (or minimal `children`) so consumers stop hand-rolling and missing new variants. |
| **19** Malformed numeric diags | Replace `println!` in lexer with `Kind::Invalid` or collected diagnostics — never write to host stdout from library code. |
| **4** Streaming lexer | `Lexer` iterator / `read_next_token` public for memory-constrained consumers. |
| **9** Recovery methods visibility | `skip_to_period` etc. → `pub(crate)` or `parser::raw`. |

---

## Phase E — Ergonomics polish (~1–2d)

| Item | Work |
|------|------|
| **8 / 21** Display + optional serde | `Display` on key errors; `serde` feature on ast/schema/common. |
| **22** `Schema::get_by_str` | Fold internally; keep atom fast path. |
| **2 / 3** Program type / parse API | First-class `Program` export; document parse entry points. |
| **7** Schema discovery helpers | `SchemaLoader` convenience from workspace config / `.df` dir (CLI already has `--schema`; library path should match). |
| **10** Parse from tokens | Document lifetime contract; optional API if a real consumer needs it. |

---

## Recommended order

1. **A** (LICENSE + re-exports) — half day, unblocks everything social/compliance.
2. **B** (renderer + virtual spans) — unlocks semantic/lint on real multi-include codebases.
3. **C** (include graph API + skipped branches) — unlocks build-tool consumers; feeds #56.
4. **D / E** as demand-driven.

## Testing strategy

- Unit tests per new public surface.
- Integration test: preprocess multi-include file → analyze → lint → render
  diagnostics; assert paths and line numbers land in the *include* file for
  symbols declared there.
- No CodSpeed requirement for pure API phases; add benches only if hot path
  (renderer is cold).

## Risk

Medium overall because surface area is large; **per-phase risk is low** if
APIs stay additive. Highest-risk item is **virtual span wiring** (touches
semantic + lint diagnostic construction + CLI).

## Effort

| Phase | Effort |
|-------|--------|
| A | 0.5d |
| B | 2–3d |
| C | 2–3d |
| D | 2d |
| E | 1–2d |
| **Total** | **~8–11d** if all phases ship |
