# Inactive preprocessor regions

An `&IF` / `&ELSEIF` / `&ELSE` branch that the preprocessor does not take is
not compiled. `oxabl lsp` tells the editor where these branches are, so the
editor can dim them. It only marks a branch that it knows is not compiled. A
branch decided on a guess stays as it is.

## The notification

The server sends `textDocument/inactiveRegions`, in the shape clangd uses:

```json
{
  "textDocument": { "uri": "file:///abs/report.p", "version": 7 },
  "regions": [{ "start": { "line": 4, "character": 0 }, "end": { "line": 5, "character": 0 } }]
}
```

- The client asks for it at `initialize` with
  `capabilities.textDocument.inactiveRegionsCapabilities.inactiveRegions: true`.
  A client that does not ask gets nothing. The server answers with
  `capabilities.inactiveRegionsProvider: true`.
- A region is the text of one branch: from the end of its directive to the
  start of the next directive of its chain (`&ELSEIF`, `&ELSE` or `&ENDIF`).
  The directive lines stay undimmed. Regions are sorted, and nested or touching
  ones are merged.
- Positions use the negotiated position encoding.
- The server sends the regions just before each `textDocument/publishDiagnostics`
  of the same analysis. So they are for the same buffer version, the same
  `oxabl.toml` and the same include context as the diagnostics. `version` is
  that buffer version. A client drops regions for a version that it no longer
  shows.
- An empty list clears the regions.

### Why this mechanism

- **Not semantic tokens.** In VS Code a semantic token replaces the TextMate
  colour of its text when the theme styles the token, and a theme can give a
  colour or a font style, not an opacity. A token over a whole branch would
  paint the branch in one colour. A modifier alone does not dim anything.
- **A decoration with an opacity.** The client puts one decoration with
  `opacity: 0.55` over each region. The TextMate colours stay, only fainter, as
  the C/C++ extension does for its inactive regions.
- **clangd's shape, not an `oxabl/` method.** A client that already shows
  clangd's inactive regions can show oxabl's with no change.
- **Pushed, not asked for.** A request would run a second analysis, which can
  see another version or another configuration than the diagnostics. The push
  rides on the analysis that gives the diagnostics.

### Who pays for it

The branch records cost a lookup per name that a condition or a define reads.
Only the language server reads them, so they are opt-in:
`Preprocessor::with_branch_tracking`, `PreprocessMode::TrackBranches` in
`oxabl_analyze::expand_source_tracking`, and
`LintPipeline::with_branch_tracking`. The daemon turns them on for the
expansion that its diagnostics and include contexts read
(`AnalysisConfig::lint_pipeline`, `analyse_root`). `oxabl check`, `oxabl build`
and the index do not, and their preprocessing costs what it did before. Without
tracking, `PreprocessedFile::branches` is empty, which says nothing about the
branches. The bench `preprocessor/tracked/if_eval` measures the cost with
tracking on.

## What is decided

The preprocessor records each branch as taken, not taken, or undecided
(`oxabl_preprocessor::BranchState`, in `PreprocessedFile::branches`). Only a
branch that is not taken is a region.

A condition is undecided when:

- it uses a function or a shape that the evaluator does not model, or a syntax
  error, or a mixed comparison (`PREPROC010`, for example `SEARCH`);
- it reads a predefined name that has no value (`PREPROC009`, for example
  `PROVERSION` with no `proversion` under `[workspace.preprocessor]`);
- it reads a name (`{&name}` or `DEFINED(name)`) whose value is not known;
- it reads a compile-environment name that `oxabl.toml` does not set (see
  below).

A reference inside a reference (`{&OPEN-QUERY-{&FRAME-NAME}}`) is expanded
from the inside out, as the AVM does: the inner value becomes part of the outer
name. Both names must be known. An inner include argument (`{&a-{1}}`) or
computed name (`{&a-{&LINE-NUMBER}}`) is checked after it is substituted: the
condition is checked once as written and once after every substitution. A value is never scanned again, so a value
that holds a reference to its own name stays text and cannot make the
expansion loop.

All branches of an undecided `&IF` are undecided. For an undecided `&ELSEIF`,
the branches before it keep their state and the rest of the chain is
undecided. Every chain inside an undecided branch is undecided too. An `&IF`
that is never closed is undecided, and so is every chain that opens after it,
closed or not.

A name is not known:

- after a define or an undefine of it in an undecided branch, as it may or may
  not have happened;
- when its value reads a name that is not known;
- after an include whose text did not arrive (not found, not readable, a
  cycle, the depth limit, the expansion limit, an unterminated reference), or
  an include in an undecided branch that was not expanded. Such text can
  define any name, so after it every condition that reads a name is
  undecided. The `OPSYS`, `PROVERSION` and database functions read
  `[workspace.preprocessor]` and no define changes them, so they stay decided;
- when it is `{&SEQUENCE}` and an undecided branch may count one more
  reference.

An include argument (`{N}`, `{&name}`) whose site reads a name that is not
known is not known either. A scoped name that an include defines ends with the
include, and what was known of the name before comes back. An `&UNDEFINE`
shows the definition it hid again, so after it the name is not known when any
earlier definition of it was not known.

The compile environment is that of the diagnostics: `[workspace.preprocessor]`
in `oxabl.toml` gives `OPSYS`, `WINDOW-SYSTEM`, `PROVERSION` and the other
predefined names.

### Defaults of the compile environment

`opsys`, `window-system`, `batch-mode` and `process-architecture` have
defaults (a 64-bit Unix batch compile with a TTY UI). The diagnostics decide
the branches on these defaults with no warning: a parse must take one branch.
The dimming does not. A default describes a common compile, not the compile
of this project, and a dimmed branch tells the reader "this code is not
compiled". So a key that `oxabl.toml` does not set is not known: `{&OPSYS}`,
`{&WINDOW-SYSTEM}`, `{&BATCH-MODE}`, `{&PROCESS-ARCHITECTURE}`, and the `OPSYS`
function when `opsys` is not set, leave their branches undimmed. Without this,
the GUI branch of every `&IF "{&WINDOW-SYSTEM}" = "TTY"` would be dimmed in a
project that never said it compiles for TTY. Set the keys to dim these
branches. `PreprocessorConfig::stated` records which keys the configuration
sets (`oxabl.toml` and, for `oxabl build`, the overlay).

`DBTYPE` of a connected database that `database-types` does not name answers
`PROGRESS` for the diagnostics. That is a default too, so for the dimming the
condition is not decided, also when every other name is known. Name the
database under `database-types` to dim on its type.

## Includes

- **A root context.** In an include with a selected root
  ([include context](include-context.md)), the regions are those of the root's
  expansion, in the include's own bytes
  (`oxabl_daemon::db::ContextDiagnostics::inactive_regions`). When the root
  expands the include more than once, a branch is a region only when no
  expansion compiles it and every expansion decides it. When the run did not
  expand an include that the compiler may expand, that expansion could be one
  more of the include, so the include gets no regions at all
  (`PreprocessedFile::missed_expansion`). That is so for:
  - an include, or a reference that can hold one, in a branch chosen on a
    guess;
  - an include whose name or arguments read a name that is not known, and a
    `{&name}` with a value that is not known, since another value names
    another include. An unset compile-environment name (`{&WINDOW-SYSTEM}` and
    the others of the defaults above) is the exception: its value is one of a
    few fixed words, never an include reference, unless the source defines
    the name;
  - an include that oxabl does not find: the compiler's PROPATH may find it;
  - an include dropped because it could not be read, closed a cycle, or passed
    the depth or expansion limit.
- **Fragment mode.** An include opened alone is preprocessed with
  `Preprocessor::with_includer_unknown`. The includer can set any name, the
  predefined ones too, and pass any argument. So a condition that reads a name
  the include did not set itself, or an argument, is undecided. A condition on
  the `OPSYS` function stays decided; one on `{&OPSYS}` does not.
  A named argument (`&x=value`) is known only in the text of the include it is
  passed to. An include that file references does not see it, as in the
  compiler, so a condition there that reads the name is undecided too.

## Known limits

- The regions come with the diagnostics. With `diagnostics = false` under
  `[workspace.features]`, or with no `oxabl.toml`, the server sends no regions
  (an empty list when the diagnostics are off). When the diagnostics publish
  and the regions of the same analysis are lost (a cancelled read), the server
  sends an empty list, so old regions do not stay on new text. It does the same
  for a version whose analysis panics (also in an include context), as no
  publish follows for that version.
- A chain on one line (`&IF a &THEN x &ELSE y &ENDIF`) dims only the text
  between its directives.
- A file whose preprocessing fails has no regions.
- An include whose root missed an expansion gets no regions, even when the
  missed text cannot reach it. The run cannot tell. A project whose
  `include_paths` do not find every include sees no regions in its includes.

## VS Code

`clients/vscode/src/inactiveRegions.ts` asks for the notification and keeps the
last regions of each document with the document version they are for
(`regionStore.ts`). It paints them in each visible editor of the document, and
again when an editor shows the document, but only while the document has that
version. An edit makes a new version, and the regions of the old one are kept
for it only when the edit cannot change a decision: one insertion of text with
no `& { } / * " ' ~`, on lines that hold none of them after the edit. The
line must not continue a directive from an earlier line: an `&IF` or `&ELSEIF`
with no `&THEN` yet, or a definition after a `~` at the end of a line. Then the
regions move with the text, and a region that the insertion touches goes. Any
other edit (a removal, a replacement, a directive line, several changes at
once) removes the regions until the next publish. An edit away from a region
can change it (a define above an `&IF`), so only such inert text keeps them. The setting
`oxabl.inactiveRegions.enable` (default `true`) turns the dimming off and on
with no restart. The e2e scenario `inactive-regions` tests it with the real
server.

## Tests

`crates/oxabl/tests/inactive_regions_lsp.rs` drives the real `oxabl lsp` over
stdio: nested chains under two compile environments, an edit, undecided
conditions, compile-environment names the project does and does not set, a
reference inside a reference, a chain inside an unclosed `&IF`, a name that an
unread include can define, an undefine and the end of an include's scope, an
include under three includer contexts and in fragment mode, an include whose
root misses an expansion of it (a guessed branch, a guessed include name, an
include that is not found), an unset environment name in a root's text, an
include argument and a computed name inside a reference, a `DBTYPE` default
(with one and with all four environment keys set), a value that names itself (also through `oxabl check`, with a time
limit), and a client that does not ask (an open and an edit).
`crates/oxabl_lsp/tests/inactive_regions_panic.rs` injects a panic in the
analysis of an edit, of a reopen, and of an include's root, and wants the
regions cleared. `clients/vscode/src/regionStore.test.ts` tests that old regions
are not painted on new text, that they move with an inert insertion, and that a
removal or an edit of a directive line drops them.
