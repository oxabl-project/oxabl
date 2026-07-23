---
artifact_contract: ce-unified-plan/v1
artifact_readiness: requirements-only
product_contract_source: ce-brainstorm
title: Formatter (Track B) - Plan
date: 2026-07-22
status: requirements-only
---

# Formatter (Track B) — Plan

## Goal Capsule

- **Objective.** Ship `oxabl_formatter`: a layout-only ("vertical") formatter for Progress ABL that normalizes whitespace, indentation, blank lines, and block/`END` placement while preserving the author's code as written — keywords, casing, abbreviations, and *existing intra-statement line breaks*. It enforces `Scope::Formatting` rules from a resolved `StyleGuide`; `Scope::Diagnostic` rules remain `oxabl_lint`'s job.
- **Product authority.** The project owner (ABL domain expert). All scope was grilled through and settled with the project owner — see [Resolved Decisions](#resolved-decisions); no open questions remain.
- **Open blockers.** One structurally-invasive prerequisite: full-fidelity `Span` on the `Statement`/`Expression` wrappers (issue #78 item 1). Everything else in Track B is additive.

---

## Product Contract

### Problem & Value

ABL shops enforce house style by hand or with slow, heavyweight legacy tooling. oxabl already has a fast lexer/parser/semantic stack; a formatter is the highest-visibility developer payoff and the natural consumer of the `oxabl_style` crate. The value is a `cargo fmt`-class experience for ABL: deterministic, fast, safe-by-default reformatting that a team can drop into CI and format-on-save without fear it will rewrite meaning or mangle intent.

The hidden cost is that oxabl's AST is **not lossless today** — comments and blank lines are discarded, and spans are incomplete. A formatter cannot exist until the parser output is fidelity-ready. This plan scopes that fidelity work and the v1 formatter that sits on top of it.

### Primary Actor & Outcome

- **Actor.** An ABL developer (or CI job) who runs `oxabl format` over `.p`/`.w`/`.i`/`.cls` source, or an editor that calls the formatter via the LSP (Track A wires that later).
- **Outcome.** Source is returned reformatted to the resolved style, byte-for-byte identical on already-conforming input, semantically unchanged always, and comments/blank lines preserved and sensibly placed.

### Settled Direction (do not re-litigate)

1. **oxc model, not a CST.** Full spans on every node + one sorted comment side-table + comment attachment done *at format time*. No rowan/Roslyn lossless CST, no whitespace tokenization. A CST would discard the `NodeId` side-table design that `oxabl_semantic`/`oxabl_lint`/`oxabl_analyze` depend on. (Confirmed by the project owner.)
2. **v1 is layout-only, defined by a hard boundary principle.** The v1 formatter **never moves code between lines** (beyond indentation), **never reorders tokens or statements**, and **never rewrites comment content**. Everything in scope is a consequence of that boundary; everything deferred to v2 is a violation of it. (Confirmed by the project owner.)
3. **Simple indent-and-emit IR for v1.** Do *not* build the Wadler/Prettier `Doc` IR now. Reflow and the doc-IR that enables it are a deliberate **v2** pass. v1's IR must not structurally preclude a later v2 doc-IR, but v1 builds none of it.
4. **Safe-default is non-mangling.** With no `[style]` config, the formatter behaves as `StyleGuide::default_base()`: preserve keywords, casing, and abbreviations; fix pure layout only. A first pass over an existing codebase must never rewrite keywords or inject constructs.

### In Scope (v1) — everything that respects the no-movement boundary

- Spans-everywhere fidelity work (the #78.1 blocker) and the `ast-invariants.md` update it forces.
- Comment side-table on `Program`; blank-line detection by scanning source between spans; blank-line normalization (R2.4).
- Comment attachment at format time (leading / trailing / dangling classification) with the ABL watch-items handled — comments **re-emitted verbatim**, never rewritten.
- Reindentation; `period_placement`, `dot_colon_same_line`, `do_placement`.
- **In-place keyword recasing / abbreviation** (`keyword_case`, `keyword_abbreviation`) when the StyleGuide opts in — a single-token, no-movement change.
- Consuming a resolved `StyleGuide` (`Scope::Formatting` rules only).
- Layout-only pretty-printer over the AST + attached comments.
- Idempotency guarantee and a semantic-preservation guard.
- CLI surface (`oxabl format`) and a library API shaped for the LSP to call later.

### Out of Scope (v1 — planned v2+), each a consequence of the no-movement boundary

- **Reflow / width-driven line wrapping** (`wrap_long_lines`, `max_line_length`, `first_param_same_line`, `multi_param_threshold`, `where_placement`, `and_or_placement`) and the **doc-IR** that enables it — moves code between lines. The explicit v2 follow-up.
- **Reordering** (`using_sort`, `class_structure_order`, `procedure_structure_order`) — reorders statements.
- **Comment content rewrite** (`comment_style` `//` ↔ `/* */`) — rewrites author content.
- All `Scope::Diagnostic` rules (naming, required constructs, symbolic operators, etc.) — those stay in `oxabl_lint`.
- Formatting files that only parse *after* preprocessor expansion (~0.4% of the corpus; see [Preprocessor / round-trip](#requirement-area-8--preprocessor--round-trip-position)).
- `textDocument/formatting` and format-on-save wiring — Track A owns LSP integration; v1 only exposes the interface it needs.

---

## Requirement Areas

### Requirement Area 1 — Spans-everywhere blocker (#78.1) [BLOCKER]

- **R1.1** Add `span: Span` to the `Statement` and `Expression` wrapper structs (today `{ id, kind }`), covering each node's **full byte extent including its trailing period** where applicable. This is the one structurally-invasive change; all other Track B work is additive.
- **R1.2** Add spans to any remaining `StatementKind`/`ExpressionKind`/`DataType` variants that lack one and that the formatter must position. Today only `Identifier`, `IncludeReference`, `IncludeArgReference`, `DefineFrame.raw_span`, and `DefineEvent.value_span` carry explicit spans (ast-invariants §1).
- **R1.3** Spans must be **non-overlapping and in source order** among siblings (§1 currently states this is aspirational and unasserted). v1 fidelity work should make it a real invariant with a `debug_assert!`, because comment attachment binary-searches against it.
- **R1.4** Synthetic/recovery nodes (e.g. `Statement::Empty` at a period) may keep zero-width spans (`start == end`), per §1 — the formatter must tolerate them.
- **R1.5** **ast-invariants.md must be updated in the same PR** as any `oxabl_ast` public-type change (project rule). Specifically §1 moves "uniform-span coverage" from *aspirational* to *guaranteed*.
- **R1.6 (cleanup).** Remove the vestigial `Kind::LineComment` / `Kind::BlockCommentStart` / `Kind::BlockCommentEnd` variants (codegen-only, never emitted) as part of tidying the comment path.

**Migration risk.** Adding a field to the two central enums touches every constructor and every exhaustive `match`/pattern across `oxabl_parser`, `oxabl_semantic`, `oxabl_lint`, `oxabl_analyze`, and the ~1000-test suite. The `StatementKind` size assertion (`<= 656` bytes on 64-bit) and the `Expression`/`Statement` wrapper size are the guardrails to watch. Mitigation: land R1.1 as a mechanical, behavior-preserving PR of its own (spans wired, no formatter yet), green the whole workspace, *then* build the formatter on top. Treat it as the tracer-bullet first slice.

### Requirement Area 2 — Comment side-table + blank-line detection

- **R2.1** Accumulate comments into a single **sorted `Vec<Comment { span, style }>`** during lexing/parsing — recorded on the existing `skip_comments()` path instead of being discarded — and hang it off the `Program` root. `style` distinguishes line (`//`) from block (`/* */`, nested-aware).
- **R2.2** Preserve the existing `has_comments` fast-path: files with no comments pay no attachment cost.
- **R2.3** Keep the **source text + `SourceMap` reachable at format time** so the printer can (a) extract comment text by span and (b) detect blank lines by scanning the original bytes *between* node spans. Blank lines are **not** tokenized (non-goal); they are inferred from source gaps.
- **R2.4** Blank-line policy: collapse runs of consecutive blank lines to a cap of **1**, via a **new StyleGuide field `max_consecutive_blank_lines: usize` (`Scope::Formatting`, default `1`)**. Plus standard edge trims: remove blank lines immediately **after a block opener** and immediately **before its `END`**, trim **leading file blanks**, and normalize to **exactly one trailing newline**. Blank lines are inferred from source gaps, never tokenized (non-goal).

### Requirement Area 3 — Comment attachment at format time

- **R3.1** Attachment lives **in `oxabl_formatter`, not the AST**. Binary-search the sorted comment table against node spans to classify each comment as **leading** (before a node, same or prior line), **trailing** (after a node on the same line, before the next), or **dangling** (inside an otherwise-empty construct, e.g. an empty block body).
- **R3.2** Attachment is **advisory metadata for the printer**, computed per-format-run; it never mutates the shared AST (keeps the NodeId side-table model intact).
- **R3.3 — ABL watch-items (must be handled):**
  - **`{...}` include / preprocessor references are NOT comments.** They are semantically meaningful (`IncludeReference`, `IncludeArgReference`, `{&macro}`) and must stay as AST nodes — never enter the comment table. **Correction (verified during Slice 2 planning):** the lexer has no route that reclassifies a `{...}` to `Kind::Comment` — a malformed include ref lexes to `Kind::Invalid` and a bare brace to `Kind::LeftBrace`, neither of which is a comment token. So there is no "Invalid recovery route" to exclude; the concern is resolved at the source. The comment collector's leading-byte gate keeps a `{`-case as a purely defensive guard against future lexer changes, not a live exclusion path.
  - **Line continuations** (`~` / `\` before a newline) collapse in `skip_whitespace`; spans must still cover the **original** bytes so attachment and re-emit are faithful.
  - **Trailing period ownership.** Because a node's span now includes its trailing `.`, a trailing comment after the period (`END. /* done */`) must attach to the correct node and not leak onto the next statement.
- **R3.4** When a comment cannot be confidently classified, prefer the **least-destructive** placement (emit it on its own line at the nearest node boundary) rather than dropping or relocating it. No comment may ever be lost.

### Requirement Area 4 — Consuming the resolved StyleGuide (+ #86 relationship)

- **R4.1** The formatter takes an already-resolved `StyleGuide` as input and reads **only `Scope::Formatting` rules**. It does not do config discovery itself — that is the workspace/CLI layer's job.
- **R4.2 — v1 rule set (all no-movement).** v1 honors: `indent_size`, `indent_style`, `do_placement`, `dot_colon_same_line`, `period_placement`, `end_with_type`, `blank_lines_between_sections`, `max_consecutive_blank_lines` (new, R2.4), and — because they are **single-token, in-place** changes that move nothing — `keyword_case` and `keyword_abbreviation` **when the StyleGuide opts in**.
- **R4.3 — v2-deferred rules (all violate the no-movement boundary).** Every reflow field: `wrap_long_lines`, `max_line_length`, `first_param_same_line`, `multi_param_threshold`, `where_placement`, `and_or_placement`; every reorder rule: `using_sort`, `class_structure_order`, `procedure_structure_order`; and every content rewrite: `comment_style` (`//` ↔ `/* */`). v1 **reads these fields but does not enforce them**.
- **R4.4 — Default is not a broken promise.** `default_base()` ships `wrap_long_lines: true` / `max_line_length: 120`, but v1 **silently does not enforce reflow-scoped fields**. This must be documented (CLI `--help`, crate docs, release notes) so users understand v1 leaves long lines as written by design — it is not a bug.
- **R4.5 — #86 relationship: parallel, not a hard prerequisite.** The `oxabl_style` crate already exists on `master` with the full typed API and TOML round-trip. The formatter *engine* needs only a `StyleGuide` value, which it can receive directly today. Issue #86 (wiring `[style]` in `oxabl.toml` into `oxabl_workspace` with nearest-ancestor discovery and precedence `CLI > oxabl.toml [style] > preset > default_base()`) is required for the **CLI's config-discovery UX**, not for the core engine. Recommendation: build the formatter engine against a passed-in `StyleGuide` in parallel with #86; the CLI's `--style`/auto-discovery flags depend on #86 landing and can be the last wiring step. If #86 is not ready, the CLI ships with `default_base()` + an explicit `--style <preset|path>` flag.

### Requirement Area 5 — Pretty-printer / IR (layout-only)

- **R5.1** v1 uses a **simple indent-and-emit traversal** over the AST + attached comments: walk in source order, emit tokens/reconstructed text with computed indentation, insert newlines only where the author had them or where a layout rule mandates one. No width measurement, no group/break algebra.
- **R5.2** Keyword/casing/abbreviation output derives from the node + StyleGuide, **not** from re-slicing raw source, so `keyword_case` can be applied when opted in. Identifiers, literals, and comment bodies are emitted **verbatim from source by span** (never reconstructed) to guarantee no accidental mangling.
- **R5.3** Indentation is structural (block depth), configurable via `indent_size`/`indent_style`. `END`/`END <type>` and `DO:`/colon placement follow their Formatting rules.
- **R5.4 (v2 guard-rail, build nothing now).** The v1 IR must not make a future doc-IR impossible, but v1 implements none of it. Record reflow + doc-IR as the planned v2 follow-up. Acceptable v1 shape: emit into a line-buffer abstraction that a v2 doc-IR could later replace, without committing to Wadler primitives now.

### Requirement Area 6 — Idempotency & safe-default guarantees

- **R6.1 — Idempotency.** `format(format(x)) == format(x)` byte-for-byte, for all inputs. This is a hard, test-enforced guarantee (property test over the corpus).
- **R6.2 — Stability on conforming input.** Already-conforming source is returned unchanged (no-op diff), so CI `--check` is trustworthy and format-on-save is quiet.
- **R6.3 — Semantic-preservation guard.** After formatting, **re-lex the output and compare the non-trivia (comment/whitespace-excluded) token stream against the input's**. On mismatch, the formatter **refuses to write** (CLI error / LSP no-op) rather than emit semantically-altered code. This is the safety net that makes layout-only trustworthy on preprocessor-heavy real-world ABL.
- **R6.4 — Safe default.** With no config, output equals `default_base()` behavior: keywords/casing/abbreviations preserved, only layout fixed. `keyword_case`, `keyword_abbreviation`, and `comment_style` transformations fire **only** when the resolved StyleGuide explicitly opts in.
- **R6.5 — Bail granularity is whole-file.** If a file cannot be formatted safely — parse errors, unresolvable preprocessor dependency, or the R6.3 guard trips **anywhere** — the formatter emits the **original file bytes unchanged** and reports why. No region-level partial formatting in v1; no best-effort rewrite that could corrupt.

### Requirement Area 7 — Delivery surface

- **R7.1 — CLI.** `oxabl format <path>` with at least: default = write in place, `--check` (exit non-zero on diff, print nothing/write nothing — CI mode), and `--stdout` (print formatted result). Style resolution via #86 discovery + `--style <preset|path>` override.
- **R7.2 — Library API for LSP (Track A).** Expose a pure function roughly `format(source, program, comments, style) -> Result<String, FormatBail>` (exact signature is ce-plan's job) that Track A can call for `textDocument/formatting`. It must be re-entrant, allocation-conscious, and return a whole-document string (range-formatting is a later concern). **This plan only defines the interface expectation; the LSP wiring itself is Track A's.**

### Requirement Area 8 — Preprocessor / round-trip position

- **R8.1 — Format raw, unexpanded source (CONFIRMED with data).** ast-invariants §1 states spans are offsets into **post-preprocessor expanded text**, but a formatter must round-trip the **original** file with `{include}` refs and `&` directives intact — it must never expand them into output. The design position: the formatter parses the file with preprocessing **off** (the CLI already supports this mode, where expanded-text == raw source, so virtual offset == real offset). `{include}` refs (`IncludeReference`), positional args (`IncludeArgReference`), and `&IF/&SCOPED-DEFINE` (`PreprocIf<T>`, `PreprocEnd`) are **already first-class AST nodes**, so they format as ordinary constructs. **Empirically validated:** `oxabl check` (no `--preprocess`) over a large real-world ABL codebase (kept outside the repo) = **>99% raw-parse success** — only a handful of failures, some of them preprocessor-branch-straddling files that only parse post-expansion; the large high-churn target file parses raw with 0 failures. v1 raw-source bail rate is **~0.4%**.
- **R8.2 — Bail on expansion-dependent files.** A file that only parses *after* expansion (e.g. an include supplying the tail of a statement) cannot be formatted in v1; it hits the whole-file R6.5 bail. This is an accepted v1 scope boundary, and the measured ~0.4% rate makes it a non-issue in practice.
- **R8.3** The spans-everywhere work (R1) must therefore guarantee correct spans **in no-preprocess mode**. This is strictly simpler than the expanded case and should be the mode the formatter's fidelity tests target.

---

## Success Criteria / Acceptance Signals

- **S1** `Statement`/`Expression` carry full spans; ast-invariants §1 updated; whole workspace (~1000 tests) green after the R1 slice.
- **S2** Round-trip fidelity: formatting a corpus of real ABL preserves every comment, every `{include}`/`&` directive, and all semantics (R6.3 guard never trips on valid preserving formats).
- **S3** Idempotency property test passes across the corpus (R6.1).
- **S4** Safe-default no-op: a curated already-clean file formats to itself byte-for-byte under `default_base()` (R6.2).
- **S5** `oxabl format --check` returns correct exit codes in CI; `--stdout`/write modes work.
- **S6** A benchmark exists (CodSpeed) covering the format hot path, per the project's "new hot path → new bench" convention.

## Risks & Migration

- **Central-enum churn (highest).** R1.1/R1.2 ripple through every crate and the test suite. Mitigate by landing spans as a standalone behavior-preserving PR before any formatter code.
- **Comment attachment edge cases.** ABL's `{...}` ambiguity, line continuations, and trailing-period ownership (R3.3) are the bug-dense area. Mitigate with a dedicated fixture suite and the R6.3 guard as backstop.
- **Preprocessor round-trip surprises.** Real ABL leans hard on includes/macros; the no-preprocess parse path (R8) must be robust enough to parse everyday files. Risk that more files than expected bail (R8.2). Early corpus measurement recommended in planning.
- **Safe-default drift.** Any transformation firing without opt-in violates the non-mangling promise (R6.4). Guarded by S4 and by scoping casing/abbreviation/comment-style behind explicit config.

## Resolved Decisions

All Track B open questions were grilled through with the project owner and are settled:

1. **Raw-source design position — confirmed with data.** Format raw, unexpanded source. Measured >99% raw-parse success across a large real-world ABL codebase (~0.4% bail). Folded into R8.1/R8.2. (Was open question B5.)
2. **v1 boundary principle — adopted.** Never move code between lines (beyond indentation), never reorder tokens/statements, never rewrite comment content. This is the defining v1 constraint (Settled Direction §2; drives In/Out scope).
3. **Keyword recasing/abbreviation is in v1** — single-token, no-movement (R4.2).
4. **Comment style `//` ↔ `/* */` is deferred to v2** — a content rewrite (R4.3). (Was B1.)
5. **`using_sort` and structure-order rules deferred to v2** — reordering (R4.3). (Was B3.)
6. **Reflow fields deferred to v2, and the default is documented, not broken** — `default_base()` ships `wrap_long_lines:true`/`max_line_length:120`; v1 reads but silently does not enforce reflow-scoped fields, documented so it reads as intent not bug (R4.3/R4.4).
7. **Bail granularity = whole-file** — guard trips anywhere → emit original bytes unchanged (R6.5). (Was B4.)
8. **Blank lines** — collapse consecutive blanks to a cap of 1 via a new `max_consecutive_blank_lines: usize` StyleGuide field (`Scope::Formatting`, default 1), plus edge trims (after opener, before `END`, leading file blanks, single trailing newline) (R2.4). (Was B2.)

## Project Constraints

- **No proprietary or customer data / PII / private corpus in the oxabl repo.** All in-repo benchmarks and fixtures use **synthetic ABL**. The real ERP corpus stays out-of-repo and is referenced by **absolute path for local runs only** — never committed, never used as a checked-in fixture. This applies to every fixture or benchmark this track adds (S2/S3/S6 corpus work runs against the out-of-repo path locally; committed tests use synthetic samples).

## Sequencing / Dependencies

1. **Slice 1 (blocker):** R1 spans-everywhere + ast-invariants update + `Kind::*Comment` cleanup. Standalone PR, workspace green.
2. **Slice 2:** R2 comment side-table on `Program` + source/SourceMap reachability + blank-line detection.
3. **Slice 3:** `oxabl_formatter` crate — R3 attachment + R5 layout printer + R6 guarantees, against a passed-in `StyleGuide`.
4. **Slice 4:** R7 CLI surface + library API; wire #86 style discovery when available (parallel track).
5. **v2 (deferred):** doc-IR + reflow/wrapping (R5.4), the reflow/reorder/content-rewrite rules (R4.3), and LSP `textDocument/formatting` (Track A).
