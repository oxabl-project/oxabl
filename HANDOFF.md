# Handoff: Track B slice 4 (`oxabl format` CLI + `[workspace.style]` discovery) implemented — LSP formatting wiring is next

**Date:** 2026-07-23
**Branch:** `feat/oxabl-formatter-cli` (off `master`) — open as **PR #94**, in final review.
**This session:** Track B **Slice 4** — the delivery surface for the formatter — implemented against `docs/plans/2026-07-23-003-feat-oxabl-format-cli-plan.md`. Adds the `oxabl format` subcommand plus `oxabl.toml [workspace.style]` auto-discovery, giving the Slice 3 engine its first caller. A Fable review pass plus manual testing against real ABL then turned up four pre-existing formatter-engine indentation/blank-line bugs, all fixed and folded into the same PR (see below).
**Prior context:** Slice 3 (the `oxabl_formatter` layout engine) merged to `master` as **#93**, bundling the full-span (**#91**) and comment side-table (**#92**) substrate beneath it. `oxabl_style` (#87) and the `oxabl lsp` skeleton (#90) are on `master`.

---

## Current state

The formatter is now a runnable tool, not just a library. The remaining formatter work is the editor (LSP) surface, not engine internals or CLI plumbing.

| Item | Status |
|------|--------|
| Track A — interactive editor tooling | `oxabl lsp` skeleton on `master` (#90); `textDocument/formatting` still not wired to `oxabl_formatter` |
| Track B — formatter engine | Complete on `master` (`oxabl_formatter`, #93) |
| Track B — formatter CLI | **Shipped this session** — `oxabl format` (write / `--check` / `--stdout`), `--style <preset\|path>`, `oxabl.toml [workspace.style]` discovery |
| Track B — formatter engine fixes | Four indentation/blank-line correctness bugs fixed this session, folded into #94 (see below) |
| `oxabl_style` | Configurable style guide (#87); now exposes a shared `from_preset_name` resolver |
| Working tree | On `feat/oxabl-formatter-cli`; open as PR #94, awaiting final review + merge |

---

## What was implemented this session (Slice 4)

- **`oxabl_style::from_preset_name(&str) -> Option<StyleGuide>`** — one source of truth for "preset name → guide", shared by the `oxabl-style` binary (its inline match now delegates) and the new CLI `--style` flag. `oestandards`/`consultingwerk` only; `default_base`/`strict_base` are deliberately not nameable.
- **`oxabl_workspace::resolved_style(target, cli_style) -> (StyleGuide, Option<String>)`** — nearest-ancestor `oxabl.toml [workspace.style]` discovery mirroring `resolved_lint_config`. `WorkspaceSection` gains `pub style: StyleGuide` (`#[serde(default)]`, no wrapper): a partial table fills from `default_base()`, an unknown key is a hard error that degrades to the default and surfaces the message. Precedence: CLI `--style` wins wholesale > discovered table > `default_base()`.
- **`oxabl format` subcommand** (`crates/oxabl/src/main.rs`) — write-in-place (default), `--check` (CI, exit 1 on any diff), `--stdout` (single file), `--style` (preset-name-first, else `.toml` path; unresolvable → exit 2). `--check`/`--stdout` are mutually exclusive (clap `conflicts_with`). Each file is parsed **raw** (preprocessing off, per R8/KTD4) so spans are real byte offsets, then handed to `oxabl_formatter::format`. Any `FormatBail` (or a lexer panic, caught per-file) leaves the file byte-for-byte unchanged and is reported; the batch never aborts mid-walk. A `format_one(source, &style) -> FormatOutcome` helper keeps the pure decision separate from the I/O/exit shell.
- **`--help` reflow no-op (R4.4)** — the `Format` subcommand's long help states the v1 layout-only, no-movement contract: `wrap_long_lines`/`max_line_length` are read but not enforced; a 200-column line stays 200 columns.

**Exit codes (KTD5):** write mode → 0 (incl. bails), 1 on I/O write/read failure, 2 on path/discovery error; `--check` → 0 iff nothing would change and all readable, 1 on any diff or read failure, 2 on path/discovery error; `--stdout` → 0 on success, directory is a usage error (exit 2).

**Verification (all green):** `cargo test --workspace`; `cargo clippy --workspace -- -D warnings`; `cargo fmt --all -- --check`. New tests: `oxabl_style` preset-resolver units, `oxabl_workspace` `resolved_style` discovery/precedence units, and `crates/oxabl/tests/format_cli.rs` (16 end-to-end tests over `tempfile` dirs covering every mode, exit code, `--style` preset/path/bogus, discovery + CLI override, and batch resilience). All fixtures are synthetic ABL (CC-1) — no corpus, no PII.

**Note on the lexer-panic guard test:** the per-file `catch_unwind` around raw `tokenize`/`parse_program` is in place (KTD4), but no synthetic ABL input was found that deterministically panics the lexer, so batch resilience is verified via the parse-error bail path — which shares the exact continue-past-a-bad-file logic.

---

## Formatter-engine fixes folded into #94 (this session)

Review (a Fable pass on the diff) + manual testing against real ABL surfaced four **pre-existing** correctness bugs in the `oxabl_formatter` printer/blanks passes (all present in #93, none introduced by the CLI slice). Each has a synthetic regression test in `crates/oxabl_formatter/tests/formatting.rs`:

- **Prefix-wrapper double-indent.** `IF … THEN DO:`, `ELSE DO:`, and labeled blocks counted the wrapper *and* the `DO` as separate levels, indenting bodies 8 per level instead of 4 (16 nested). Fixed in `tree.rs`/`printer.rs`: a prefix wrapper (`IF`/`ELSE`/label/`ON`) contributes no level for a branch that is a **self-delimiting block** (or an else-position `IF`, so else-if chains stay flush) — while a leaf branch or a THEN-nested bare `IF` still gets its `+1` (`children_with_deltas`).
- **Panic guard scope (CLI).** The `format()` call was moved inside `format_one`'s `catch_unwind` (not just `tokenize`/`parse`), so an engine panic can't abort a directory walk mid-way.
- **Trailing comment reindented its own code line.** `IF … THEN DO: /* x */` could drag the opener to the body's depth (attachment hands the trailing comment back as *leading* of the body's first statement). Fixed in `printer.rs`: the own-line-comment indent override now skips any line a statement actually starts on.
- **Trailing comment suppressed the after-opener blank drop.** `is_block_opener` used a naive `ends_with(':')`, so `DO: /* x */` (ending in `*/`) wasn't seen as an opener and a spurious blank survived. Fixed in `blanks.rs`: detect the opener by its last *code* token being `:` (tokenized, ignoring trailing comments and `:` inside strings).

**Known gap left in place (out of scope, pre-existing):** a leaf `ELSE` whose statement is on the *same* line (`ELSE MESSAGE "x".` after a leaf `THEN`) still indents one level too deep — it needs the same "anchor the ELSE line" work and has no regression yet.

---

## Next

0. **Multi-line-token bail — [#95](https://github.com/oxabl-project/oxabl/issues/95) (highest-value formatter follow-up).** The engine whole-file-bails (guard trip, file left unchanged — no corruption) on any file containing a multi-line **string literal** or multi-line **`{include}` reference**, because the line-based reindent would shift bytes inside the token. Common in real ABL (include `&args` across lines). Fix: leave physical lines that *begin inside* a multi-line token verbatim. Its own PR; synthetic repros in the issue.

1. **LSP `textDocument/formatting` wiring (Track A).** Declare `document_formatting_provider` in `crates/oxabl_lsp/src/capabilities.rs` and add a handler that parses the document rope **raw** (preproc off — the existing `collect_from_expanded` path parses *expanded* text and is not reusable, per KTD4) and calls `oxabl_formatter::format`. Small, self-contained; unblocked by the re-entrant library API today. Write the plan first (`/ce-plan`).
2. **Deferred config extension:** a `preset = "..."` key inside `[workspace.style]` (a named-preset base the table overlays). Needs a small wrapper struct around `StyleGuide` because `deny_unknown_fields` rejects the extra key — a targeted revision, not a rework.
3. **Deferred engine rules:** the token-*movement* placement variants (`do_placement`/`dot_colon_same_line`/`period_placement` NewLine forms) and `blank_lines_between_sections`, once their semantics are pinned. v1 reads-and-preserves them.
4. **v2 territory:** reflow / width-driven wrapping (the doc-IR it needs), reordering (`using_sort`, structure-order), and comment-content rewrite (`comment_style`) — all read-but-not-enforced today.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#94** | This session's PR — `oxabl format` CLI + `[workspace.style]` discovery + the four folded formatter-engine fixes. In final review. |
| **#95** | Multi-line string / `{include}` reindent bail — the highest-value formatter follow-up; own PR next. |
| #78 | Formatter tracking issue — substrate (#91/#92) + engine (#93) done; CLI shipped this session; LSP formatting wiring remains |
| `docs/plans/2026-07-23-003-feat-oxabl-format-cli-plan.md` | Plan implemented this session (Slice 4) |
| `docs/plans/2026-07-23-002-feat-oxabl-formatter-engine-plan.md` | Slice 3 plan (engine, #93) — the library this slice calls |
| `STRATEGY.md` | Track definitions and the formatter-safety metric; Formatter paragraph updated to "CLI shipped; LSP wiring remaining" |
