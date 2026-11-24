# Include context

An include file has no meaning alone. A variable it declares is used or unused,
and a name it reads is defined or not, only in a root file that expands it. So
`oxabl lsp` analyses an open `.i` in one of two modes:

- **Fragment mode**, the default. The include is analysed alone, as
  `oxabl_lint::fragment` describes, and findings that depend on the includer are
  left out or lowered.
- **A root context**, chosen by the client. The diagnostics of the include are
  the findings of that root's expanded compilation unit that sit in the include
  (`CollectedDiagnostics::include_origin`), with spans in the include's own
  bytes.

The server never chooses a root by itself. A wrong root would give answers that
look complete.

## Requests

`oxabl/includerContexts` takes `{ textDocument: { uri } }` for an include and
answers:

```json
{
  "selected": "/abs/reads.p",
  "contexts": [{ "root": "/abs/reads.p", "uri": "file:///abs/reads.p", "direct": true }],
  "complete": true,
  "reason": null
}
```

- `contexts` are the roots in the workspace index (`ReverseGraph`) with a
  direct or transitive include edge to the file, sorted by path, each once.
- `complete` is false when the list can miss a root: an unresolved include
  reference that may name the file, a file that could not be analysed, or one
  with an incomplete dependency set. `reason` then says which. With no include
  path configured, the list is empty, `complete` is false and `reason` names
  the remedy, the same refusal as `oxabl/impact`. With no root at all, `reason`
  says so.

`oxabl/selectIncluderContext` takes `{ textDocument: { uri }, root }`. `root`
is a path from the list, or `null` for fragment mode. The server checks the
root against a fresh list before it applies it, and refuses a root that does not
include the file with `InvalidParams`. When a later selection for the same file
arrives before an earlier one is checked, the earlier one fails with
`ContentModified` and is not applied.

Both run on worker threads: they wait for a current workspace pass.

## The analysis

`oxabl_daemon::db::compute_context_diagnostics` analyses the root on a worker
snapshot. It reads every file through the open buffers (`Session::buffer_overlay`)
over the configured file system, so unsaved text of the include and of the root
counts, and nothing is written to disk. It uses the snapshot's cross-file index.
It is not memoized: it runs again when the include, the root, an include of the
root, the configuration or the schema changes.

The related locations of a finding (`relatedInformation`) point at the root
or at the physical include file that holds each one, at a position in the
text that the analysis read.

A root that gives no answer for the include gives one warning at the start of
the include that says why: the root cannot be read, its preprocessing failed,
or it no longer includes the file.

Results go through the same `dispose` rules as every diagnostics result
(version, configuration generation, cancellation, panic). A result for a root
that is no longer selected is dropped, and while a root is selected a fragment
result for the file is dropped.

## Known limits

- Only semantic and lint findings in the include are shown in a root context.
  A parse or preprocessor error inside the include is not shown there; fragment
  mode shows it.
- The callee effect summaries that some strict database rules read are not
  built for a root context.
- The selection is kept per open document. It ends when the document closes or
  the server restarts.
- This is one compilation unit's answer. It never says that a declaration is
  unused in the whole workspace.

## The expanded compilation unit

`oxabl/expandedUnit` takes `{ textDocument: { uri } }` and answers with the
source oxabl parsed for that file, after preprocessing: for a root file, the
root; for an include, its selected root, or the include alone in fragment mode.
It reads through the open buffers, like the root context, and runs
`oxabl_daemon::db::compute_expanded_unit` on a worker snapshot.

- `text`: a header comment (the root, `complete`, `reduced` or `failed`, and
  each problem), then the expanded text. A marker line `/* ▼ name path (from
  root:line → include:line) */` comes before each include expansion and
  `/* ▲ name */` after it, nested as the includes nest. An include that did not
  arrive gets a `/* ✖ ... */` line where it would be.
- `regions`: the marker lines of each expansion, its path, its depth and the
  chain of `{...}` sites from the root down to it, each a physical location.
- `lines`, `files`: the physical file and line of each output line.
- `diagnostics`: the findings of the same analysis (root findings, findings
  in includes, preprocessor errors in includes), placed in `text`, each with
  its physical `source` location. A finding whose text the expansion did not
  keep sits on the first line.
- `problems`: what makes the expansion reduced: an include that was not found
  or could not be read, a lost stretch of text (the depth limit, an unclosed
  `&IF`, a condition oxabl cannot decide), and preprocessor errors in includes
  such as a cyclic include. `status` is `reduced` when there is any, and
  `failed` when the root cannot be read or its preprocessing failed.
- `dependencies`: the root and every include read, for the client's refresh.

The markers are lines of their own, so an include that starts mid-line breaks
that line in the view. The view is for inspection, not for editing.

`oxabl_analyze::ExpandedFile` gives what the rendering needs:
`include_expansions` (every expansion with its parent, site and expanded
span), `expanded_offset` (a physical byte to its place in the expansion),
`file_path` and `nested_preprocessor_diagnostics`.

## VS Code

The status bar shows `oxabl: fragment` or `oxabl: in <root>` for the active
include, and the command `oxabl.selectIncluderContext` (**oxabl: Select Include
Context**) shows the list as a quick pick. The e2e scenario `include-context`
tests it with the real server.

The command `oxabl.showExpandedUnit` (**oxabl: Show Expanded Compilation
Unit**) shows the answer of `oxabl/expandedUnit` as a read-only document with
the scheme `oxabl-expanded`, named `<root> (expanded).p`. Its folding ranges
are the regions, and every region starts folded. Document links on each `▼`
line open the include and each `{...}` site of its chain. Go to Definition on
any line opens the physical line. Its diagnostics carry the physical location
as related information. An edit or save of any file in `dependencies`, a
change of `oxabl.toml` and a new include context ask again. Each view counts
its requests and drops an answer that a newer request replaced. The e2e
scenario `expanded-unit` tests it with the real server.
