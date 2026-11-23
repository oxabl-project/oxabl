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

## VS Code

The status bar shows `oxabl: fragment` or `oxabl: in <root>` for the active
include, and the command `oxabl.selectIncluderContext` (**oxabl: Select Include
Context**) shows the list as a quick pick. The e2e scenario `include-context`
tests it with the real server.
