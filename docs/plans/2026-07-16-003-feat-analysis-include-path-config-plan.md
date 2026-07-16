---
title: "feat: Analysis include-path config loading + loud unresolvable-include diagnostics"
type: feat
status: draft
date: 2026-07-16
origin: [GitHub #58, GitHub #56]
branch: feat/analysis-include-path-config
---

# feat: Analysis include-path config loading + loud unresolvable-include diagnostics

## Problem Statement

Include expansion already works. The preprocessor fully expands `{file.i}` inline
into its span tree whenever the file is found on the configured search path
(`ProcessContext::expand_include`, `crates/oxabl_preprocessor/src/preprocessor.rs:688-763`),
so any variable/buffer/temp-table declared in a resolved include becomes ordinary
AST and is visible to the semantic and lint passes "for free." The CLI already
threads `--include-path`/`-I` flags into `Preprocessor::new(&fs, include_paths)`
(`crates/oxabl/src/main.rs:38-40, 63-65, 153, 404`), and the `FileSystem` trait
already does first-match-wins PROPATH search
(`crates/oxabl_workspace/src/file_system.rs:18-26`).

The feature is therefore **not** new resolution machinery. Two real gaps remain,
plus one correctness item to lock down:

1. **Config is never loaded.** `WorkspaceConfig` (`crates/oxabl_workspace/src/config.rs`)
   already models `[workspace.sources].include_paths` and parses `oxabl.toml`, and
   `WorkspaceConfig::from_path` exists — but nothing in the CLI calls it. Every
   invocation must retype `-I` for every include directory. This is the primary
   ergonomic gap (GitHub #58).

2. **Unresolved includes fail silently.** When `resolve_include` returns `None`,
   `expand_include` emits a `PREPROC004` **Warning** and returns `Vec::new()` —
   the include is *elided* (preprocessor.rs:700-715). In batch `check` output,
   warnings are explicitly filtered out (`main.rs:433-448` skips any
   non-`Error` severity). So the failure is invisible, and every symbol that the
   missing include would have declared silently vanishes.

   **Concrete example.** `order.p` contains `{shared/globals.i}` which defines
   `DEFINE SHARED VARIABLE gcCompany AS CHARACTER.` plus 30 other globals. If
   `shared/globals.i` is not on the search path, the include elides to nothing,
   `gcCompany` is never declared, and every one of the ~40 references to those
   globals fires `undefined-symbol` (LINT0001). The user sees 40 red squiggles
   and zero explanation. Per GitHub #56, this is exactly the *silent wrongness*
   we must replace with a *loud unresolvable*: one diagnostic that says "the
   include could not be resolved, so symbols from it cannot be checked" is worth
   more than 40 misleading downstream errors — and it points at the fix (widen
   PROPATH). The over-report-vs-under-report asymmetry from #56 applies: a single
   honest "I don't know" beats a flood of confidently-wrong findings.

Distinguish clearly: **resolution already works when the file is found.** This
plan does not touch the found-file happy path except to test it. It closes the
config-loading gap, makes the not-found case loud, and pins down PROPATH ordering
fidelity with tests (and a fix only if a divergence is found).

---

## Goals / Non-Goals

### Goals

- **G1.** Auto-discover and load `oxabl.toml`, extracting
  `workspace.sources.include_paths`, and merge it with CLI `-I` flags so include
  directories need not be retyped per invocation. Applies to both `check` and
  `analyze`.
- **G2.** Make an unresolved include *loud*: emit a diagnostic that survives batch
  output (not a silently-filtered warning) and states that symbols from the
  include cannot be checked, so consumers can widen invalidation and users can
  fix PROPATH.
- **G3.** Document and test PROPATH resolution-order fidelity (first-match-wins
  across the ordered search list, relative-vs-absolute, working-directory
  semantics). Fix `resolve_include` only if it diverges from AVM behavior.
- **G4.** Add a regression-guard benchmark for multi-directory include resolution.

### Non-Goals

- The full XREF-diff audit harness from #56 (comparing our unresolved set against
  the AVM's XREF output) — that is a separate, larger project.
- Table/buffer schema-dependency extraction from includes.
- Any cross-file symbol resolution (`RUN "other.p"`, `USING` across files).
- LSP wiring of `oxabl.toml` (separate task; builds on G1).
- Environment-variable expansion in paths (`$DLC`, `$ABLINCLUDE`).
- Automatic file-extension resolution (`{shared}` → `{shared.i}`).
- Salsa/stat caching of resolution results (noted under Benchmark/Risks only).

---

## Design

### (a) Config-file schema, discovery, and precedence vs CLI flags

**Schema (already exists — no change required).**
`crates/oxabl_workspace/src/config.rs` already defines:

```toml
[workspace]
name = "sports2000"

[workspace.sources]
directories  = ["src/", "procedures/"]
include_paths = ["src/include/", "/shared/abl/"]   # PROPATH equivalent, in order

[workspace.schema]
files = ["schema/sports2000.df"]
```

`WorkspaceConfig::from_toml` and `WorkspaceConfig::from_path(root)` are
implemented and tested (config.rs:46-62). We reuse them verbatim. (Confirmed:
the config type already lives in `oxabl_workspace`; this feature adds *loading*,
not the type.)

**Discovery.** Walk upward from the target file's directory (single-file mode) or
from the target directory (`check <dir>` mode) to the filesystem root, using the
first ancestor that contains `oxabl.toml` as the workspace root. This mirrors how
`cargo`, `rustfmt`, and `tsc` locate their config. If none is found, config
contributes nothing (flags-only behavior, exactly as today). Discovery failures
(unreadable/malformed `oxabl.toml`) are surfaced as a diagnostic on stderr, never
a panic — `from_path` already returns `Result<_, String>`.

**Relative-path anchoring.** Relative `include_paths` from the config are resolved
**relative to the workspace root** (the directory containing `oxabl.toml`), not
the cwd and not the analyzed file's directory. This is the least-surprising rule
and matches how PROPATH is typically configured relative to a project root. CLI
`-I` paths are resolved relative to the cwd (standard for command-line args).
Both are normalized to absolute `PathBuf`s (via a non-failing join-against-root +
`components()` cleanup, **not** `std::fs::canonicalize`, which errors on
not-yet-existing dirs — see Risks) before being handed to the preprocessor, so
`resolve_include`'s `dir.join(name)` behaves identically for each source.

**Precedence / merge.** CLI flags take precedence over the config file, but the
two are *merged*, not mutually exclusive — the union is what the AVM effectively
sees, and dropping either would reintroduce false positives. Order matters for
first-match-wins, so the merged list is:

```
merged = [<all -I flags, in CLI order>, <all config include_paths, in file order>]
```

CLI-first means an explicit `-I` can shadow a config entry for the same relative
name (an intentional override), while config entries still fill in everything the
user didn't type. This is a deliberate choice over "CLI replaces config entirely"
(see Alternatives).

**Wiring point (and testability).** The discovery + merge logic lives as two
functions — `find_workspace_root(start: &Path) -> Option<PathBuf>` (ancestor walk
for `oxabl.toml`) and `resolved_include_paths(target: &Path, cli_flags:
&[PathBuf]) -> Vec<PathBuf>` (load config, anchor + canonicalize, merge CLI-first).
These must **not** live in `main.rs`: a `crates/oxabl/tests/*.rs` integration
test cannot import symbols from the binary target, and `oxabl`'s `lib.rs` is
currently only re-exports. Place them in `oxabl_workspace` (next to
`WorkspaceConfig`) so they are directly unit-testable and reusable by the future
LSP. `run_check` (main.rs:219-305) and `run_analyze` (main.rs:135-217) call
`resolved_include_paths(path, include_paths)` once and pass `&paths` where they
currently pass `include_paths` straight through. No change to `Preprocessor` or
`FileSystem` signatures — they already accept `&[PathBuf]`.

Because `WorkspaceConfig::from_path` and the ancestor walk hit disk via
`std::fs` directly (config.rs:56-61), they are **not** parameterized over the
`FileSystem` trait; injecting an `InMemoryFileSystem` will not work. Unit tests
for these helpers therefore use a real `tempfile` scratch directory (see
Testing). Making config loading `FileSystem`-generic is possible but out of scope
here — noted as a follow-up.

### (b) Loud unresolved-include diagnostic

**Root cause of silence.** `PREPROC004` is emitted at `Severity::Warning`
(preprocessor.rs:709-713), and the surfacing loop in `parse_file_with_preprocess`
filters out everything that isn't `Severity::Error` before printing
(main.rs:433-448, specifically the `continue` at 436-438). So today an unresolved
include produces exactly zero user-visible output in the batch path, while
silently deleting all its symbols. The original rationale
(preprocessor.rs:703-708) was that system includes like `src/web2/wrap-cgi.i`
can't be found on dev machines and would "spam every web file." That rationale is
sound but the current fix (suppress entirely) overshoots into silent wrongness.

**Decision: keep it a preprocessor diagnostic, not a new LINT code, but make it
reach the user.** Rationale:

- The preprocessor is the only layer that *knows* an include failed to resolve.
  The lint layer sees only the downstream symptom (a `NotInScope` reference) and
  cannot attribute it to a specific missing include without new plumbing that is
  explicitly out of scope here.
- A lint rule would fire *N* times (once per orphaned reference) — re-creating
  the flood. One preprocessor diagnostic per unresolved include is the correct
  cardinality: loud once, at the true cause.

**Design.**

1. **Promote visibility, not necessarily severity.** Introduce a distinct code
   `PREPROC007` ("unresolvable include") kept separate from `PREPROC004` so the
   message can carry the symbol-loss consequence and so downstream consumers can
   filter on it precisely. Keep severity at `Warning` (ABL system includes are a
   real false-alarm source), but change the *surfacing* rule in the CLI: unlike
   generic warnings, unresolvable-include diagnostics are always printed. In the
   `check` surfacing loop (`parse_file_with_preprocess`, main.rs:433-448) the
   `continue` guard changes from `if !matches!(d.severity, Error)` to
   `if !matches!(d.severity, Error) && d.code != DiagnosticCode("PREPROC007")`.
   This gives "loud, not silent" without reclassifying every benign warning as an
   error (which would break exit codes and CI for corpora full of unlocatable
   system includes).

2. **Message.** `format!("unresolvable include '{include_name}' — symbols it \
   declares cannot be checked; add its directory to include_paths (oxabl.toml \
   [workspace.sources]) or pass -I")`. Attach `Diagnostic::with_help(...)` with
   the same remediation so LSP/renderers can show it as a help line.

3. **Threading from span tree to diagnostics stream.** No new mechanism needed.
   `expand_include` already pushes into `ctx.diagnostics`
   (preprocessor.rs:709), which `process` returns inside
   `PreprocessedFile.diagnostics` (span_tree.rs:50-56, preprocessor.rs:64-70).
   The diagnostic's `span` is already a real `FileSpan` — a concrete `FileId`
   plus real byte offsets into *that* file — not a virtual offset. This matters
   for rendering (see point 4): `PreprocessedFile::resolve` maps the *other*
   direction (virtual → real) and is therefore irrelevant here; there is no
   virtual-to-real step to perform, the coordinates are already real. We simply
   change the code/message at the existing push site and adjust the CLI filter.
   `analyze` currently drops preprocessor diagnostics entirely on the success
   path (main.rs:155-156 `Ok(pf) => pf.to_text()`); it must be taught to iterate
   `pf.diagnostics` and surface `PREPROC007` (+ errors) the same way `check` does.

4. **Rendering across FileIds (correctness fix for nested includes).** The
   existing `check` render loop looks up `d.span.span.start` against a
   `SourceMap` built from the *root* `source` (main.rs:434, 439), *regardless of*
   `d.span.file`. That is correct only when the diagnostic belongs to the root
   file. A `{missing.i}` referenced *inside a resolved include* carries a
   `d.span.file` that is **not** the root FileId, so resolving its offset against
   the root SourceMap prints a garbage line/col. Worse, the included file's text
   lives in `PreprocessedFile.sources`, which is **private** (span_tree.rs:59),
   so the CLI cannot build a SourceMap for it today. Fix, in order of preference:

   - **(a) Guard + degrade (minimum viable, no API change):** compute line/col
     only when `d.span.file == root_file_id` (the `FileId` the CLI passed to
     `process`, `FileId::new(1)`). Otherwise print
     `"in included file: <message>"` with no line/col (or the include *reference*
     site if we choose to relocate the span — see below). This is loud and
     correct-by-omission: never a wrong line number.
   - **(b) Add `PreprocessedFile::source_text(&self, file: FileId) ->
     Option<&str>` accessor** (thin wrapper over the private `get_source`,
     span_tree.rs:129-134) so the CLI can build a SourceMap for the owning file
     and print an accurate line/col even for nested-include diagnostics.
     Preferred if we want precise locations; small, additive public API.

   Either way, drop any reliance on `resolve_position_through_include` /
   `PreprocessedFile::resolve` for this path — those solve the unrelated
   virtual-offset problem.

5. **JSON output — scoped decision.** The surfacing described above is
   `eprintln!`-to-stderr only. The machine-readable paths stay silent:
   `JsonReport` / `JsonFailure` (main.rs:86-112, populated 614-680) have no
   diagnostics field, and `analyze --format json` (the default) never emits
   preprocessor diagnostics. Per #56's machine-consumer concern, **v1 adds a
   diagnostics channel to JSON, not just stderr** (chosen over deferring):
   - `JsonReport` gains `preproc_diagnostics: Vec<JsonDiagnostic>` where
     `JsonDiagnostic { file: String, code: String, severity: String, line:
     usize, col: usize, message: String }`. `parse_file_with_preprocess` returns
     the surfaced `PREPROC007`/error diagnostics (alongside `FileResult`) so
     `run_check`'s JSON assembly can include them; the line/col follows the same
     FileId guard as point 4 (0/0 or the include path string when not the root
     file).
   - `run_analyze`'s JSON dump gains a top-level `"preproc_diagnostics"` array
     with the same shape, populated on the `Ok(pf)` arm before `to_text()`.
   If review prefers a smaller v1, the documented fallback is **stderr-only
   surfacing**, explicitly recording that JSON consumers get no include signal
   until a follow-up — but the plan's default is to wire JSON now, because
   "loud, not silent" is hollow if the machine-readable output stays silent.

### (c) PROPATH-order correctness

**Rules to hold (and document in the resolve_include doc-comment):**

1. **First-match-wins, in order.** `resolve_include` iterates `include_paths`
   in slice order and returns the first `dir.join(name)` that exists
   (file_system.rs:18-26). This matches PROPATH: earlier entries shadow later
   ones. Already correct; already tested
   (`resolve_include_prefers_earlier_path`, file_system.rs:120-129).

2. **Absolute vs relative.** `dir.join(name)`: if `name` is absolute it replaces
   `dir` (Rust `Path::join` semantics) — acceptable, since `{ /abs/x.i }` is
   rare and behaves as an absolute reference. If `dir` is absolute and `name`
   relative (the common case) the result is `dir/name`. Our config/CLI helper
   normalizes every search `dir` to absolute before the preprocessor runs
   (see (a)), so relative-vs-absolute ambiguity is resolved at the *config*
   boundary, keeping `resolve_include` itself semantics-free.

3. **Working-directory semantics.** The AVM implicitly searches the current
   working directory. We do **not** add an implicit `.` entry (matches today's
   behavior; documented as an explicit choice — users add `"."` to
   `include_paths` if they want it). This is called out in Open Questions.

**Verdict: no fix to `resolve_include` is expected.** The function already
implements first-match-wins. The work in (c) is (i) a documentation doc-comment
stating the three rules, and (ii) the new tests below. Should the relative-path
normalization in (a) reveal a divergence (e.g. a `dir` that isn't
normalized slipping through), the fix is confined to the config helper, not
`resolve_include`.

### Alternatives considered

- **New LINT code for unresolved includes** instead of a preprocessor diagnostic.
  Rejected: fires once per orphaned reference (flood), and the lint layer can't
  cheaply attribute the symptom to the cause. The preprocessor already has the
  precise information at the precise cardinality.
- **Promote PREPROC004 to `Severity::Error`.** Rejected: unlocatable system
  includes are legitimately common in real corpora; making them errors breaks
  exit codes and CI. We keep `Warning` severity but force *surfacing* for the
  new `PREPROC007` code.
- **CLI flags fully replace config include_paths.** Rejected: users would lose
  every config path the moment they add one ad-hoc `-I`, reintroducing FPs.
  Union-with-CLI-precedence matches AVM PROPATH intuition.
- **Resolve config-relative paths against cwd or the analyzed file.** Rejected:
  workspace-root anchoring is stable regardless of where `oxabl` is invoked from.
- **New crate for config loading.** Rejected: `WorkspaceConfig::from_path`
  already exists; the only missing piece is a ~20-line discovery/merge helper,
  placed in `oxabl_workspace` (not the binary) so it is unit-testable.
- **Helper functions in `main.rs`.** Rejected: the binary target's symbols are
  not importable from `crates/oxabl/tests/*.rs`, and `lib.rs` is only re-exports,
  so helpers there could not be unit-tested. Put them in `oxabl_workspace`.

---

## Implementation Steps

1. **Doc-comment PROPATH rules** on `FileSystem::resolve_include`
   (`crates/oxabl_workspace/src/file_system.rs:16-18`): state first-match-wins,
   absolute-vs-relative `join` behavior, and the no-implicit-cwd choice.

2. **Add `PREPROC007` at the elision site**
   (`crates/oxabl_preprocessor/src/preprocessor.rs:709-714`): replace the
   `PREPROC004` push in the `None` arm with a `PREPROC007` warning carrying the
   symbol-loss message + `with_help`. (Keep `PREPROC004` reserved / or retire it
   — decide during impl; simplest is to rename in place since it has a single
   emit site.) Severity stays `Warning`.

3. **Config discovery + merge helper in `oxabl_workspace`** (new module, e.g.
   `crates/oxabl_workspace/src/include_paths.rs`, not the binary): add
   `find_workspace_root(start: &Path) -> Option<PathBuf>` (ancestor walk for
   `oxabl.toml`) and `resolved_include_paths(target: &Path, cli: &[PathBuf]) ->
   Vec<PathBuf>` (load config via `WorkspaceConfig::from_path`, anchor config
   paths to the workspace root, normalize to absolute, merge CLI-first). Return
   config parse errors so the caller can surface them on stderr. Placed here so
   they are unit-testable and reusable by the LSP.

4. **Optional accessor `PreprocessedFile::source_text`** (`span_tree.rs`, thin
   pub wrapper over the private `get_source`, 129-134) — needed only if we pick
   rendering option (b) in Design (b.4). Skip if we ship the guard-and-degrade
   option (a).

5. **Add `PREPROC007` at the elision site**
   (`crates/oxabl_preprocessor/src/preprocessor.rs:709-714`): replace the
   `PREPROC004` push in the `None` arm with a `PREPROC007` warning carrying the
   symbol-loss message + `with_help`. (Retire `PREPROC004` in place — it has this
   single emit site.) Severity stays `Warning`.

6. **Wire `run_check`** (`main.rs:219-305`): compute `let paths =
   resolved_include_paths(path, include_paths);` once and pass `&paths` into
   `parse_file_with_preprocess` (main.rs:281) and `run_debug_parse`
   (main.rs:254).

7. **Wire `run_analyze`** (`main.rs:135-217`): same helper; pass merged paths to
   `Preprocessor::new` (main.rs:153).

8. **Make the diagnostic loud in `check`** — the surfacing loop lives in
   `parse_file_with_preprocess` (main.rs:433-448), not `run_check`. Change the
   `continue` guard (main.rs:436-438) to also keep `PREPROC007`, and apply the
   FileId guard from Design (b.4): render line/col only when
   `d.span.file == FileId::new(1)` (the root file the CLI passed to `process`),
   otherwise print `"in included file: <message>"` (option a) or resolve via the
   new `source_text` accessor (option b).

9. **Make the diagnostic loud in `analyze`** (`main.rs:151-170`): on the
   `Ok(pf)` arm, before `to_text()`, iterate `pf.diagnostics` and surface errors +
   `PREPROC007` (mirror the `check` loop, same FileId guard). Do **not** use
   `pf.resolve` — diagnostic spans are already real `FileSpan`s.

10. **JSON channels** (Design b.5): add `preproc_diagnostics: Vec<JsonDiagnostic>`
    to `JsonReport` (main.rs:86-98) with a new `JsonDiagnostic` struct; populate
    it in `run_check`'s JSON assembly (main.rs:614-680) from the surfaced
    diagnostics. Add a top-level `"preproc_diagnostics"` array to `run_analyze`'s
    JSON dump. Both follow the FileId guard for line/col.

11. **Update `CLAUDE.md`** Current Status + the `oxabl_preprocessor` bullet to note
    config auto-loading and the `PREPROC007` loud-unresolved-include contract.

---

## Testing

### Existing tests to preserve (must stay green)

- Preprocessor include tests (preprocessor.rs ~1120-1519+): `include_expansion`,
  `nested_include`, `include_not_found`, `include_with_define_propagation`,
  `resolve_position_through_include`, `dynamic_include_name_from_preproc_var`,
  the `include_*_comment_not_expanded` set, `expand_include_with_name_on_own_line`,
  `cyclic_include_detected`, `dependencies_tracks_all_includes`.
  Note: `include_not_found` (preprocessor.rs:1256-1265) today asserts **only**
  elision (`to_text() == "BEFORE  AFTER"`) — it does *not* currently assert any
  diagnostic. Extend it (or clone into the new `unresolved_include_emits_loud_diagnostic_not_silent`
  below) to add a *new* assertion that a `PREPROC007` diagnostic is present; keep
  the existing elision assertion.
- FileSystem tests (file_system.rs:110-136): `resolve_include_finds_first_match`,
  `resolve_include_prefers_earlier_path`, `resolve_include_returns_none_when_not_found`.
- Config tests (config.rs:64-130): all `parse_*` cases.

### New tests

All hermetic via `InMemoryFileSystem`; preprocessor tests drive
`Preprocessor::new(&fs, &paths).process(...)` and assert on
`PreprocessedFile.{tree,to_text(),diagnostics}`.

**Preprocessor — resolution & loud diagnostic**
(`crates/oxabl_preprocessor/src/preprocessor.rs` test module):

- `include_found_in_configured_second_path_expands_symbols` — file only in
  `/inc2`, paths `[/inc1, /inc2]`; assert the include's declaration text appears
  in `to_text()` (symbols present → no downstream FP) and `diagnostics` is empty
  of `PREPROC007`.
- `unresolved_include_emits_loud_diagnostic_not_silent` — file absent from all
  paths; assert `diagnostics` contains exactly one `PREPROC007` with the
  symbol-loss message and a `help`, and that it is **not** silently absent.
- `propath_first_match_wins_uses_earliest_dir` — same-named include in both
  `/inc1` and `/inc2`; assert `to_text()` contains the `/inc1` variant's marker,
  confirming order fidelity at the expansion level (complements the
  `resolve_include` unit test).
- `include_relative_vs_absolute_dir_resolution` — one relative and one absolute
  search dir (both pre-normalized as the helper would), file in the
  absolute one; assert resolution succeeds.
- `nested_unresolved_include_carries_inner_file_id` — root file resolves
  `{outer.i}` (present); `outer.i` references `{missing.i}` (absent). Assert one
  `PREPROC007` whose `span.file` is the inner include's `FileId` (i.e. **not**
  `FileId::new(1)`), and that `outer.i`'s own text still expands. This is the
  case that exposes the root-SourceMap rendering bug (Design b.4) — the test pins
  the FileId so the CLI-render test can assert the guard.

**Config helper unit tests** (`oxabl_workspace`, alongside the new
`include_paths` module, using a `tempfile::TempDir` scratch directory on the real
FS — required because `WorkspaceConfig::from_path` and the ancestor walk use
`std::fs` directly and are not `FileSystem`-parameterized):

- `config_include_paths_loaded_when_no_flags` — write `oxabl.toml` with an
  `include_paths` entry; assert `resolved_include_paths` returns it (absolute) with
  no CLI flags.
- `cli_flag_and_config_paths_merge_cli_first` — assert the merged list is
  `[<cli flags…>, <config paths…>]` in that order (CLI precedence, union).
- `config_relative_paths_anchored_to_workspace_root` — call with a `target` in a
  nested subdir; assert a config-relative entry resolves against the root
  (dir containing `oxabl.toml`), not the subdir or cwd.
- `find_workspace_root_walks_ancestors` — `oxabl.toml` two dirs up; assert it is
  found.
- `missing_oxabl_toml_returns_flags_only` — no config present; assert output ==
  the CLI flags unchanged.
- `malformed_oxabl_toml_surfaces_error_not_panic` — assert an `Err`/diagnostic is
  returned and behavior degrades to flags-only.

**CLI end-to-end** (`crates/oxabl/tests/include_path_config.rs`) — the binary's
`main.rs` symbols are not importable from a test crate, so drive the **built
binary** via `env!("CARGO_BIN_EXE_oxabl")` with a `tempfile` workspace (add
`tempfile`, and optionally `assert_cmd`, as `[dev-dependencies]` of
`crates/oxabl`):

- `check_reports_preproc007_when_include_dir_absent` — run
  `oxabl check --preprocess <dir>` with no include dir configured; assert stderr
  contains a `PREPROC007` line naming the missing include.
- `check_clean_when_oxabl_toml_provides_include_path` — same workspace plus an
  `oxabl.toml` pointing at the include dir; assert no `PREPROC007` and success.
- `check_json_lists_preproc_diagnostics` — run with `--json`; parse stdout and
  assert the `preproc_diagnostics` array contains the `PREPROC007` entry (Design
  b.5) — guards against the machine-consumer-silent regression.
- `nested_unresolved_include_renders_without_garbage_linecol` — the two-level
  fixture from the preprocessor test; assert the CLI prints
  `"in included file"` (or an accurate line/col via `source_text`) and never a
  line/col computed against the root SourceMap.

**FileSystem unit** (file_system.rs): keep the three existing tests; they already
cover first-match-wins and none-found and need no change.

---

## Benchmark

**New:** `bench_multi_path_resolution` added to
`crates/oxabl_preprocessor/benches/preprocessor_bench.rs` (extends the existing
`bench_isolation` pattern, in-memory FS, no disk I/O).

- Configure a **deep search path** — e.g. 8 directories `/inc0../inc7`, with the
  target includes present only in `/inc5` — so each `resolve_include` call
  iterates ~6 `exists()` misses before the hit. This is the regression guard for
  this feature: it measures the cost of ordered path iteration and repeated
  `exists()` stats, which is precisely what grows when users add many PROPATH
  entries.
- Drive a fixture with several distinct includes (reuse/extend
  `bench_preprocessor_isolation.abl`) so per-include iteration dominates.
- Register it in `preprocessor_benchmarks` (preprocessor_bench.rs:80-102)
  alongside `bench_isolation`.

**Stat-caching considerations (documented, not implemented here):** each
`resolve_include` re-issues `exists()` for every candidate on every reference;
with `N` includes × `M` paths that is `O(N·M)` stat syscalls per file. The
in-memory bench is `HashMap` lookups (cheap) — the *real* cost is `RealFileSystem`
disk stats, which this bench cannot capture. Note in the bench doc-comment that a
future `resolve_include` result cache (keyed by `name`) would cut repeat stats,
and that the bench's job is to catch algorithmic regressions (e.g. accidental
O(N²) path handling), not model disk latency. No cache is added now
(single-pass preprocessor; out of scope).

---

## Risks & Edge Cases

- **Nested / recursive includes.** Symbols from a resolved include that itself
  `{...}`s another are already expanded transitively (recursion at
  preprocessor.rs:753). An unresolved *inner* include emits its own `PREPROC007`
  whose `span.file` is the inner include's `FileId`, not the root's. The CLI must
  not resolve that offset against the root file's SourceMap (Design b.4) — this
  is the concrete rendering bug the guard fixes, and
  `nested_unresolved_include_carries_inner_file_id` +
  `nested_unresolved_include_renders_without_garbage_linecol` pin it. Note this is
  the *diagnostic span* direction (real FileSpan), unrelated to the virtual-offset
  `resolve` machinery.
- **Include cycles.** Already handled: `PREPROC006` error + halt
  (preprocessor.rs:731-739). Merging more search paths can surface a cycle that a
  narrower path hid; the existing detection covers it. No change.
- **Case-sensitivity across OSes.** `resolve_include` delegates existence to the
  OS via `RealFileSystem::exists` (file_system.rs:39-41). On case-insensitive
  volumes (macOS default, Windows) `{Shared.i}` may match `shared.i`; on Linux it
  won't. We do not normalize case — document this as OS-defined behavior
  (matches AVM, which is also host-FS-dependent). Tests use `InMemoryFileSystem`
  with exact keys, so they are deterministic regardless of host.
- **Repeated stats / performance.** See Benchmark. Deep PROPATHs × many includes
  multiply `exists()` calls; the bench guards against algorithmic regressions.
- **Config discovery false root.** An `oxabl.toml` in an unexpected ancestor could
  be picked up. Mitigation: use the *nearest* ancestor and print which config was
  loaded under `--debug`.
- **Malformed `oxabl.toml`.** `from_path` returns `Err(String)`; surface on
  stderr and fall back to flags-only rather than aborting the run.
- **Canonicalization of non-existent dirs.** `canonicalize` fails on missing
  paths; use a non-failing normalization (join against root + `.components()`
  cleanup) so a configured-but-absent dir simply never matches rather than
  erroring.

---

## Rollout

- Branch: `feat/analysis-include-path-config` off `master`.
- Single PR (config loading + loud diagnostic + PROPATH doc/tests + bench are
  small and cohesive). Conventional-commit `feat:` so Release Please bumps minor.
- No public API breakage: `WorkspaceConfig`, `FileSystem`, and `Preprocessor`
  signatures are unchanged; only the binary crate gains a helper and the
  diagnostic code/surfacing changes.
- CI (`cargo check`/`test`/`fmt`/`clippy -D warnings`) + CodSpeed pick up the new
  bench automatically.

---

## Open Questions

1. **Config filename/format.** Stick with `oxabl.toml` (already modeled/tested) —
   confirm we don't also want `.oxabl.toml` or a `[tool.oxabl]` table in a shared
   file. Recommendation: `oxabl.toml` only for v1.
2. **Diagnostic layer.** This plan makes it a preprocessor diagnostic
   (`PREPROC007`) surfaced loudly, not a LINT code. Confirm that's the desired
   home, or whether a later phase should *also* add a lint hint that correlates
   an orphaned `undefined-symbol` back to a specific `PREPROC007` (explicitly
   deferred here).
3. **Implicit cwd in PROPATH.** Do we ever want to auto-append `.` to the search
   list to match AVM's implicit-cwd behavior, or keep it explicit? Current plan:
   explicit only.
4. **Severity policy.** Is `Warning`-but-always-surfaced the right contract, or
   should `PREPROC007` be `Error` under a strict/`--deny-unresolved` mode? Left
   as a future flag.
5. **`--no-config` escape hatch.** Should there be a flag to ignore a discovered
   `oxabl.toml` for reproducible one-off runs? Likely yes; trivial to add.
6. **JSON diagnostic shape.** The plan wires `preproc_diagnostics` into both
   `JsonReport` and the `analyze` JSON dump (Design b.5). Confirm the flat
   `{file, code, severity, line, col, message}` shape is acceptable, and whether
   nested-include diagnostics should report the include *reference* site in the
   root file (stable line/col) instead of `0/0` when `span.file` isn't the root.
