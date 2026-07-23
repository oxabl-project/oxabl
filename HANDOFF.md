# Handoff: LSP `textDocument/formatting` wired to `oxabl_formatter` (#100) — the formatter is now editor-integrated end to end

**Date:** 2026-07-23
**Branch:** `master` — everything below is **merged**; no open PRs from this session.
**This session:** Wired `textDocument/formatting` into the `oxabl lsp` server (**#100**), turning the formatter from a CLI-only tool into an editor-integrated one. Also merged the previously-open #98 fix (**#99**). The server now advertises `document_formatting_provider`, resolves a `StyleGuide` from the document's `oxabl.toml [workspace.style]`, parses the open buffer **raw** (preprocessor off), calls `oxabl_formatter::format`, and returns a single whole-document `TextEdit` — or no edits on any bail.
**Prior context:** The formatter engine (#93), CLI + `[workspace.style]` discovery (#94), multi-line-token safety (#96), `IF`/`ELSE` branch spans (#97), and the wrapped-branch indent fix (#98/#99) are all on `master`. Substrate beneath: full-span AST (#91), comment side-table (#92), `oxabl_style` (#87), `oxabl lsp` skeleton (#90).

---

## Current state

The formatter is a runnable, real-world-tested tool available **both** from the CLI and live in an editor. Track A (interactive editor tooling) now has a working `textDocument/formatting` surface on top of the diagnostics skeleton.

| Item | Status |
|------|--------|
| Track A — LSP formatting | **Shipped (#100)** — `document_formatting_provider` advertised; whole-document formatting live; range formatting deliberately unadvertised |
| Track A — LSP diagnostics | Skeleton on `master` (#90) — handshake, incremental sync, push `publishDiagnostics` |
| Track B — formatter engine | On `master`; multi-line-token safe (#96), correct `IF`/`ELSE` branch spans (#97), wrapped-branch indent fixed (#98/#99) |
| Track B — formatter CLI | On `master` (#94) — `oxabl format` (write / `--check` / `--stdout`), `--style <preset\|path>`, `oxabl.toml [workspace.style]` discovery |
| Editor extension (VS Code) | **Not started** — no client yet surfaces the new capability to a real user |

---

## What was implemented this session

### #100 — LSP `textDocument/formatting` wiring (merged)

Plan: `docs/plans/2026-07-23-006-feat-lsp-formatting-wiring-plan.md` (gitignored, point-in-time). Four units, all landed in `crates/oxabl_lsp`:

- **Capability (R1):** `document_formatting_provider: Some(OneOf::Left(true))` in `capabilities.rs`. Range formatting stays unset — the engine has no region concept and bails whole-file.
- **Style resolution (R3):** new `formatting.rs::style_for_uri` reuses `oxabl_workspace::resolved_style` against the document's filesystem path, so the editor applies the *same* `oxabl.toml [workspace.style]` the CLI would. Non-`file` or unresolvable URIs → `StyleGuide::default_base()`. No new LSP config surface. `uri_to_path` in `lib.rs` was promoted to `pub(crate)` and reused rather than hand-rolling a second URI decoder.
- **Handler (R2/R4/R5/R6/R7):** `formatting.rs::compute_formatting_edits` runs `tokenize → parse_program → format` inside `catch_unwind`, mirroring the CLI's `format_one`. It parses the rope directly and **never** touches the salsa `expanded_text`/`collect_from_expanded` diagnostics query (that path is preprocessor-*on* and would reformat macro output, not the buffer). Wired into the `Message::Request` arm (shutdown → formatting → `MethodNotFound` fallthrough); runs inline on the main loop, no snapshot, no debounce.
- **Never mangle (KTD3):** every non-success — `FormatBail`, unchanged output, parse-dirty input, a caught panic, or an unopened URI — returns an empty edit list. The editor leaves the buffer untouched.
- **Tests:** unit (`capabilities`, `style_for_uri`, `compute_formatting_edits`) + `tests/formatting_e2e.rs` through the real message loop (advertisement, reformat round-trip + idempotence, unopened-URI tolerance, bail-survives-thread, `[workspace.style]` discovery).

### #99 — wrapped multi-line branch mis-indent (merged; was open last handoff)

The #98 fix: the printer's `block_ends` closer-snap was narrowed to non-wrapper constructs via an `is_prefix_wrapper` predicate, so a wrapped multi-line `IF`/`ELSE` branch keeps its continuation indent instead of snapping to the enclosing depth. Layout-only, guard-invisible.

**Verification (all merged work):** `cargo test --workspace`; `cargo clippy --workspace -- -D warnings`; `cargo fmt --all -- --check`. All fixtures synthetic ABL (CC-1) — no corpus, no PII.

---

## Metrics movement

Wiring formatting into the LSP moves two of the four key metrics from **unmeasurable** to **measurable** (per `STRATEGY.md`): *interactive latency* (there is now a real editor round-trip to time) and *daily dogfood adoption* (once a client surfaces it). *Formatter safety* is now enforced end-to-end through the server — the idempotence and no-mangle properties are exercised by `formatting_e2e.rs`, not just the CLI.

---

## Next

1. **VS Code extension (Track A) — the natural next step.** Nothing surfaces the new `document_formatting_provider` to a real user yet. A thin client that launches `oxabl lsp` and offers "Format Document" (and format-on-save, which is client-side) is what turns the shipped capability into daily dogfood. `README.md` still says "LSP integration pending" for the formatter — flip it once a client ships.
2. **Deferred config extension:** a `preset = "..."` key inside `[workspace.style]` (a named-preset base the table overlays). Needs a small wrapper struct around `StyleGuide` because `deny_unknown_fields` rejects the extra key. Improves both CLI and LSP at once.
3. **Deferred engine rules:** the token-*movement* placement variants (`do_placement`/`dot_colon_same_line`/`period_placement` NewLine forms) and `blank_lines_between_sections`, once their semantics are pinned. v1 reads-and-preserves them.
4. **v2 territory:** reflow / width-driven wrapping (the doc-IR it needs), reordering (`using_sort`, structure-order), comment-content rewrite (`comment_style`), and minimal-diff LSP edits (whole-document replace is correct today given the guard) — all read-but-not-enforced.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#100** | Merged — LSP `textDocument/formatting` wired to `oxabl_formatter` (this session) |
| **#99** | Merged — the #98 fix (`is_prefix_wrapper` gate on the `block_ends` push) |
| **#98** | Fixed by #99 — wrapped multi-line branch statement mis-indented its last line |
| #97 | Merged — full-extent spans on `IF`/`ELSE` block branches |
| #96 | Merged — multi-line-token reindent verbatim + shared-tokenization perf fix |
| #94 | Merged — `oxabl format` CLI + `[workspace.style]` discovery (Slice 4) |
| #90 | Merged — `oxabl lsp` diagnostics skeleton (the substrate this session built on) |
| #78 | Formatter tracking issue — substrate + engine + CLI + LSP formatting wiring now all done |
| `docs/solutions/logic-errors/formatter-multiline-token-reindent-bail.md` | Learning captured from #96 |
| `STRATEGY.md` | Formatter track updated: the LSP `textDocument/formatting` surface has shipped |
