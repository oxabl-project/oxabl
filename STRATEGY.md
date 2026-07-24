---
name: Oxabl
last_updated: 2026-07-24
---

# Oxabl Strategy

## Target problem

Developers working in large, long-lived Progress ABL codebases have almost no fast, modern tooling — no editor-integrated formatter, linter, or language server that responds while they type. What exists is slow or absent, so they edit without the instant feedback every other mainstream language takes for granted. The gap persists because ABL's preprocessor/include model and idiosyncratic syntax make *correct, fast* tooling genuinely hard to build.

## Our approach

Build a correctness-first, blazingly-fast Rust foundation (lexer, parser, semantic analysis) and layer developer tooling on top, shipped as a single binary and delivered editor-first (LSP → VS Code). We win on two commitments competitors can't easily match: fast enough for per-keystroke interactivity, and safe enough to never mangle code — the two properties that decide whether developers actually leave the tooling on.

## Who it's for

**Primary:** Developers maintaining large, legacy ABL codebases from their editor — dogfooded daily by the team building oxabl. They're hiring oxabl for instant, trustworthy format-and-lint feedback in-editor, without breaking flow or risking their source.

## Key metrics

- **Interactive latency** — warm language-server cycle ≤50ms p95 on a representative large, high-churn file; single-file format/lint in sub-millisecond time. The "fast enough to stay on" bet; regresses if the pipeline slows.
- **Real-world conformance** — >99% of a large, real-world ABL codebase (kept outside this repo) lexes, parses, and formats cleanly. Correctness against actual code, not toy samples.
- **Formatter safety** — `format(format(x)) == format(x)` (idempotent) with zero semantic-preservation-guard trips. If it mangles code, the churn cost outweighs the tool.
- **Daily dogfood adoption** — developers running oxabl in-editor every day. The lagging signal that the approach landed; the first editor client (a sideloadable VS Code extension) has landed and real in-editor dogfood has begun — already surfacing (and driving fixes for) the first trust-eroding lint false positives, which is exactly the feedback loop this metric exists to create.

## Tracks

### Foundation & conformance
The fast, correct core — lexer, parser, semantic analysis — held to >99% coverage of real-world ABL.
_Why it serves the approach:_ Every tool above is only as trustworthy and fast as this base; correctness-first starts here.

### Interactive editor tooling
The resident, incrementally-recomputing engine (coarse salsa substrate + language server) and the editor clients that surface diagnostics and formatting live. The first client has landed: a thin, sideloadable VS Code extension that launches `oxabl lsp` for format-on-save and push diagnostics, with granular per-rule control driven from `oxabl.toml` — validated in-editor by a JSON Schema generated from the config structs, and live-reloaded when the config changes.
_Why it serves the approach:_ The editor-first delivery path and the home of the per-keystroke interactivity commitment.

### Formatter
A fidelity-ready AST (full spans + comment side-table) and a layout-only formatting engine that is idempotent and non-mangling by construction. The engine (`oxabl_formatter`) is a pure library — comment attachment, structural reindent, blank-line normalization, opt-in keyword recasing, and a re-lex semantic-preservation guard that bails whole-file rather than emit altered code. It has two users: the `oxabl format` CLI (write-in-place / `--check` / `--stdout`, with `--style <preset|path>` and `oxabl.toml` `[workspace.style]` nearest-ancestor discovery, precedence CLI > `oxabl.toml` > safe default), and — now shipped — the language server's `textDocument/formatting` request, which parses the open buffer raw and returns whole-document edits, falling back to no edits on any bail so the editor never receives a mangling rewrite. The formatter is now a fully editor-integrated tool end to end.
_Why it serves the approach:_ Directly delivers the "never mangle code" safety commitment as a shippable tool.

### Linting & rules
The lint engine, its per-project configuration surface, and eventually a public API for registering and selecting rules. A rule is only as trustworthy as the resolver beneath it: early dogfood confirmed that false positives — not missed diagnostics — are what make a developer turn the linter off, so trust-hardening the resolver (single-file correctness now; cross-file/workspace resolution next) is the gate on rule value.
_Why it serves the approach:_ Turns the semantic foundation into actionable, tunable feedback — the payoff developers see.

### Public API & client architecture
The `oxabl` umbrella crate is now a curated, single-dependency public API: named modules per layer plus a few top-level conveniences (`parse`, `analyze`, `format_source`, `render_diagnostics`, `Diagnostic`), with internal helpers unreachable by construction and serde-serializable diagnostics behind an opt-in feature. The CLI, LSP, VS Code extension, and browser WASM demo consume this one surface instead of reaching into sub-crates or hand-rolling the tokenize→parse→analyze/format pipeline. The WASM crate is deliberately only a transport adapter: it maps shared diagnostics and formatter results to a small JSON wire shape, while unsupported project capabilities (includes and schema-backed checks in the first slice) stay explicitly unavailable rather than being reimplemented differently. Near-term direction: today's `check`/`analyze` subcommands (and their `--json`) are transitional scaffolding; the target is shared lint and format *pipelines* in the library that every client drives identically, so a diagnostic reads the same in the CLI, the editor, and any future consumer.
_Why it serves the approach:_ "Single binary, editor-first, consistent everywhere" only holds if there is one shared surface the clients agree on — this is that surface.

### Browser try-it-out
The first browser slice now compiles the shared single-file analysis and safe formatter pipelines to WebAssembly and embeds them in the Oxabl website. It runs entirely client-side, loads lazily, and is built as a versioned release artifact by the Oxabl repository; the Railway-hosted static website only serves that artifact and the UI around it. Includes, workspace resolution, `oxabl.toml`, and schema upload are intentionally not simulated in the MVP.
_Why it serves the approach:_ It shortens first contact to seconds while ensuring the demo earns trust with exactly the behavior users get after installing the CLI or extension.

## Not working on

- Reflow / line-wrapping in the formatter — v1 is layout-only; width-driven wrapping is deferred.
- Fine-grained incremental recomputation — coarse per-file recompute already meets the interactivity bar.

## Marketing

**One-liner:** Blazingly fast, correctness-first tooling for Progress ABL.
