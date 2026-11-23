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

## What is decided

The preprocessor records each branch as taken, not taken, or undecided
(`oxabl_preprocessor::BranchState`, in `PreprocessedFile::branches`). Only a
branch that is not taken is a region.

A condition is undecided when:

- it uses a function or a shape that the evaluator does not model, or a syntax
  error, or a mixed comparison (`PREPROC010`, for example `SEARCH`);
- it reads a predefined name that has no value (`PREPROC009`, for example
  `PROVERSION` with no `proversion` under `[workspace.preprocessor]`);
- it reads a name (`{&name}` or `DEFINED(name)`) whose value is not known.

All branches of an undecided `&IF` are undecided. For an undecided `&ELSEIF`,
the branches before it keep their state and the rest of the chain is
undecided. Every chain inside an undecided branch is undecided too.

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

## Includes

- **A root context.** In an include with a selected root
  ([include context](include-context.md)), the regions are those of the root's
  expansion, in the include's own bytes
  (`oxabl_daemon::db::ContextDiagnostics::inactive_regions`). When the root
  expands the include more than once, a branch is a region only when no
  expansion compiles it and every expansion decides it.
- **Fragment mode.** An include opened alone is preprocessed with
  `Preprocessor::with_includer_unknown`. The includer can set any name, the
  predefined ones too, and pass any argument. So a condition that reads a name
  the include did not set itself, or an argument, is undecided. A condition on
  the `OPSYS` function stays decided; one on `{&OPSYS}` does not.

## Known limits

- The regions come with the diagnostics. With `diagnostics = false` under
  `[workspace.features]`, or with no `oxabl.toml`, the server sends no regions
  (an empty list when the diagnostics are off).
- A chain on one line (`&IF a &THEN x &ELSE y &ENDIF`) dims only the text
  between its directives.
- A file whose preprocessing fails has no regions.

## VS Code

`clients/vscode/src/inactiveRegions.ts` asks for the notification and keeps the
last regions of each document. It paints them in each visible editor of the
document, and again when an editor shows the document. The setting
`oxabl.inactiveRegions.enable` (default `true`) turns the dimming off and on
with no restart. The e2e scenario `inactive-regions` tests it with the real
server.

## Tests

`crates/oxabl/tests/inactive_regions_lsp.rs` drives the real `oxabl lsp` over
stdio: nested chains under two compile environments, an edit, undecided
conditions, a name that an unread include can define, an undefine and the end
of an include's scope, an include under three includer contexts and in
fragment mode, and a client that does not ask.
