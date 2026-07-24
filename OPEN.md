# Open questions — #120 shared lint & format pipelines

Raised while planning [#120](https://github.com/oxabl-project/oxabl/issues/120). The plan is implementation-ready and every question below has a decisive answer recorded as a Key Technical Decision, so nothing here blocks starting work. They are collected because three of them change user-visible behavior and were settled by delegated judgement rather than by a maintainer.

Plan: `docs/plans/2026-07-24-001-refactor-shared-lint-format-pipelines-plan.md` (local only — `docs/plans/` is gitignored by repo policy).

## Decided by delegation, want confirmation

Q1. **What does `oxabl check` report?** Decided: lint diagnostics plus a separate file-level format-drift summary, two channels rather than one merged stream, with `--no-lint` and `--no-format` escapes. Exit 0 with neither, 1 with either, 2 on usage or config error. Rationale: a lint finding is span-anchored while format drift is a per-file boolean, so merging them would mean synthesizing fake spans. `format --check` survives as the granular dry-run over the same pipeline. The alternative is ruff's actual split — `check` is lint-only — which would drop the `--no-format` flag and simplify the exit contract. (KTD7)

Q2. **Where does the parse-conformance walk live?** Decided: a `conformance` subcommand marked hidden in clap, with its report, `error_patterns` aggregation, `--json` shape, and 0/1/2 exit codes moved unchanged, plus `--debug`. Rationale: it measures the parser against a corpus, so it is project instrumentation rather than a developer tool, and its exit-1 meaning is unrelated to `check`'s. Hiding it keeps the public CLI surface small but makes the project's own instrument undiscoverable to new contributors — that is the tradeoff to confirm. (KTD8)

Q3. **Does `analyze` survive as its own subcommand?** Decided: yes, unfolded, still single-file, still exit 0 with diagnostics present, rewired onto the shared pipeline. Rationale: a semantic-model dump and a diagnostic report answer different questions, and folding it into `check --json` would bloat the diagnostic wire shape and discard the per-section versioned envelope that has fixture-test investment. (KTD9)

## Surfaced by review, decided the cheap way

Q4. **Does the browser's `wasm_bindgen` surface gain a config parameter?** Decided: no, not in this change. Its entry points take source only, so per-rule lint severity joins includes and schema as a deliberate browser capability gap, and the cross-client parity test asserts the gap rather than asserting equal severities. Adding an optional config argument is a JS-facing API change worth doing on its own terms.

Q5. **Should `find_workspace_root` and `WorkspaceConfig::from_path` gain `FileSystem`-taking variants?** Decided: out of scope here — config resolution stays real-filesystem-bound, and the "reads `oxabl.toml` once" property is proven structurally (an inner function takes an already-parsed config) rather than by an instrumented filesystem. Worth revisiting, because this is what would let an in-memory client such as the browser ever discover config at all.

Q6. **Is promoting `preproc_diagnostics` into a versioned envelope section in scope?** Decided: yes, bundled into the `analyze` rewire. It is a data-model cleanup with no dependency on pipeline unification, riding along because the JSON contract is already being broken and doing it later costs a second round of fixture churn. It touches three test files and will look like unrelated churn in review if not called out in the PR description.
