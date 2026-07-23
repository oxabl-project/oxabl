# Handoff: Track B slice 2 (comment side-table) implemented — formatter crate is next

**Date:** 2026-07-23
**Branch:** `master`
**Last merge (#91):** `2e774b1` — full-fidelity spans on AST wrapper nodes (Track B blocker)
**This session:** Track B **Slice 2** (comment side-table + blank-line primitive + StyleGuide field) implemented against `docs/plans/2026-07-23-001-feat-comment-side-table-plan.md`. **Uncommitted** in the working tree — not yet committed or PR'd.
**Also landed since last handoff:** #90 (`d025e3d`, `oxabl lsp` skeleton, Track A), #89 (`9800767`, STRATEGY.md + example-identifier cleanup), #88 (`7172c66`, span-seeding placement fix), #87 (`64570d4`, `oxabl_style` crate)

---

## Current state

Track B's fidelity layer is now complete: the AST carries full spans (#91) **and** a comment side-table (this session). The formatter itself is the only remaining piece before Track B ships a tool.

| Item | Status |
|------|--------|
| Track A — interactive editor tooling | `oxabl lsp` skeleton shipped (#90) |
| Track B — formatter | Fidelity substrate complete: full spans (#91) + comment side-table (this session, **uncommitted**). No `oxabl_formatter` crate yet. |
| `oxabl_style` | Configurable ABL style guide crate (#87); gained `max_consecutive_blank_lines` this session |
| Working tree | **Dirty** on `master` — Slice 2 changes staged for a commit/PR (see below) |

---

## What was implemented this session (Slice 2, uncommitted)

Per `docs/plans/2026-07-23-001-feat-comment-side-table-plan.md`. Additive, behavior-preserving — no `oxabl_formatter` crate, no comment attachment, no printer, no enforcement of the new field (all deferred to Slice 3).

- **U1 — `Comment`/`CommentKind` in `oxabl_ast`** (`crates/oxabl_ast/src/comment.rs`): `Comment { span, kind }` (`Copy`), `CommentKind { Line, Block }`; text is *not* stored (derived by span later). `docs/design/ast-invariants.md` gained §13 documenting the table's invariants and the **pinned span-end convention** (`//` span includes its trailing `\n`; `&`-directive span excludes it; `/* */` covers through `*/`).
- **U2 — collection into `Program.comments`** (`crates/oxabl_parser/src/parser/mod.rs`): a single linear `collect_comments()` pass over the full token slice at the end of `parse_program`, decoupled from the cursor/skip path (so no skip site can drop a comment) and **not** gated on `has_comments` (so leading-only comments survive). `classify_comment()` derives kind from leading source bytes and `debug_assert!`s on any unexpected shape; the `{`-led arm is a silent defensive exclusion (include/preproc refs never lex to `Kind::Comment`). Fast-path `advance()` untouched.
- **U3 — `blank_lines_between`** (`crates/oxabl_common/src/blank_lines.rs`): pure `(source, start, end) -> usize`, newline-count-minus-one clamped at zero, whitespace-only lines count as blank, `\r\n` handled by keying on `\n`.
- **U4 — `max_consecutive_blank_lines`** (`crates/oxabl_style/src/style_guide.rs`): `usize` field, default `1`, `Scope::Formatting`, TOML round-tripping. Plumbing only — no consumer reads it yet.

**Verification (all green):** whole-workspace `cargo test`; `cargo clippy --workspace -- -D warnings` (the CI command) clean; `cargo fmt --check` clean; `parser_bench` shows no regression from the one-shot collection pass. New tests: `oxabl_ast` +2, `oxabl_parser` +11, `oxabl_common` +8, `oxabl_style` +4.

**Pre-existing lint (not from this work, not in CI):** `cargo clippy --all-targets` trips `doc-overindented-list-items` in `crates/oxabl/examples/seeding_inventory.rs`. CI runs `--workspace` (no `--all-targets`), so it's green there — worth a separate housekeeping fix.

---

## Next

1. **Commit + PR this slice.** Conventional-commit `feat(ast): comment side-table on Program + blank-line detection`; open the PR, let CI/CodSpeed confirm no parser regression.
2. **New `oxabl_formatter` crate (Slice 3).** The layout-only formatting engine on the now-fidelity-ready AST. This is where the deferred pieces land: **comment attachment** (binary-search the sorted `Program.comments` against node spans — leading/trailing/dangling classification), the **printer**, and **enforcement** of `max_consecutive_blank_lines` (collapse runs + edge-trim) via the new `blank_lines_between` primitive. Targets: idempotency (`format(format(x)) == format(x)`) and zero semantic-preservation-guard trips per `STRATEGY.md`. Write the Slice 3 plan first (`/ce-plan`).
3. Track A follow-ups deferred out of #90 remain open and don't block the formatter: fine-grained salsa, inline disable pragmas, custom-rule registry.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| #78 | Formatter tracking issue — item 1 (spans) closed by #91; comment side-table done this session; `oxabl_formatter` crate remains |
| PR #91 | Merged — full-fidelity AST wrapper spans |
| PR #90 | Merged — `oxabl lsp` diagnostics skeleton |
| PR #87 | Merged — `oxabl_style` crate |
| `docs/plans/2026-07-23-001-feat-comment-side-table-plan.md` | Plan implemented this session (Slice 2) |
| `docs/plans/2026-07-22-001-feat-ast-wrapper-spans-plan.md` | Slice 1 plan (spans, #91) — the substrate this built on |
| `STRATEGY.md` | Track definitions and key metrics referenced above |
