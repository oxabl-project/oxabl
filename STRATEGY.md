---
name: Oxabl
last_updated: 2026-07-23
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
- **Daily dogfood adoption** — developers running oxabl in-editor every day once the extension ships. The lagging signal that the approach landed.

## Tracks

### Foundation & conformance
The fast, correct core — lexer, parser, semantic analysis — held to >99% coverage of real-world ABL.
_Why it serves the approach:_ Every tool above is only as trustworthy and fast as this base; correctness-first starts here.

### Interactive editor tooling
The resident, incrementally-recomputing engine (coarse salsa substrate + language server) and the editor clients that surface diagnostics and formatting live.
_Why it serves the approach:_ The editor-first delivery path and the home of the per-keystroke interactivity commitment.

### Formatter
A fidelity-ready AST (full spans + comment side-table) and a layout-only formatting engine that is idempotent and non-mangling by construction. The engine (`oxabl_formatter`) is a pure library — comment attachment, structural reindent, blank-line normalization, opt-in keyword recasing, and a re-lex semantic-preservation guard that bails whole-file rather than emit altered code. It has two users: the `oxabl format` CLI (write-in-place / `--check` / `--stdout`, with `--style <preset|path>` and `oxabl.toml` `[workspace.style]` nearest-ancestor discovery, precedence CLI > `oxabl.toml` > safe default), and — now shipped — the language server's `textDocument/formatting` request, which parses the open buffer raw and returns whole-document edits, falling back to no edits on any bail so the editor never receives a mangling rewrite. The formatter is now a fully editor-integrated tool end to end.
_Why it serves the approach:_ Directly delivers the "never mangle code" safety commitment as a shippable tool.

### Linting & rules
The lint engine, its per-project configuration surface, and eventually a public API for registering and selecting rules.
_Why it serves the approach:_ Turns the semantic foundation into actionable, tunable feedback — the payoff developers see.

## Not working on

- Reflow / line-wrapping in the formatter — v1 is layout-only; width-driven wrapping is deferred.
- Fine-grained incremental recomputation — coarse per-file recompute already meets the interactivity bar.

## Marketing

**One-liner:** Blazingly fast, correctness-first tooling for Progress ABL.
