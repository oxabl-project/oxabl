# Database inlay hints

ABL code does not show some database facts: the lock that a read takes when
it names none, the index that a search uses, and the block where a buffer
scope or a transaction starts and ends. The compiler decides them. oxabl
already computes them for the strict database rules. The inlay hints show them
in the editor, at the code they are about.

- `oxabl_lint::hints` selects the facts.
- `oxabl_pipeline` (`LintPipeline::database_hints`) places them in the file and
  writes their labels.
- `oxabl lsp` sends them as the answer to `textDocument/inlayHint`.
- The VS Code extension sends the settings that turn each kind off.

## Kinds

| Kind | Label and place | Shown for | Selected by |
| --- | --- | --- | --- |
| `locks` | `SHARE-LOCK`, after the buffer name | A `FIND`, a `FOR` or `PRESELECT` phrase, a `CAN-FIND` or an `OPEN QUERY` phrase that reads a database table and names no lock | `default-lock` (LK001) |
| `index` | `index <name>`, after the buffer name; `index <name>, WHOLE-INDEX` when no condition brackets the index | A search of a database table or a temp-table for which the effects analysis names an index | The effects analysis, the facts `find-in-loop` (DB001) reads |
| `scope` | `<buffer> scope starts` before the block, `<buffer> scope ends` after it | A buffer scope that a reference outside a `FOR` block raises to the procedure | `scope-widened` (TX005) |
| `transactions` | `transaction starts` before the block, `transaction ends` after it | A block that is a transaction block only because it updates the database, and a main block that is the transaction | `implicit-transaction` (TX001), `procedure-wide-transaction` (TX002) |

Each hint has a tooltip that says why, and names the rule.

A lock that the code writes, a `DO TRANSACTION` and a `DO FOR` scope are in the
code, so they get no hint. A `GET` gets no lock hint: it takes the lock of its
query, as LK001 says.

## The hints are the rules' findings

The hints must never say something that the strict rules do not. So the hints
do not compute the facts again, and they do not select them again:

- Each rule that a kind follows has a selection function: `reported_reads`
  (LK001), `reported_scopes` (TX005), `reported_blocks` (TX001) and
  `reported_main` (TX002). The rule makes its findings from it, and the hints
  are made from it.
- The facts come from one `RunShared` (scopes, effects, flow), as in a lint run.
- `oxabl_analyze::database_hints_from_expanded` builds the analysis context
  with the same function as the lint run (`lint_context`).

`crates/oxabl/tests/inlay_hints_lsp.rs` compares them black-box. Over a
synthetic program it asks `oxabl lsp` for the hints, and compares them with the
findings of `oxabl check --json`, the findings the server publishes, and the
index choices of `oxabl explain --json`.

## Unknown facts give no hint

- A file that the scope analysis cannot judge gives no hint of any kind: a
  recovered parse, an include that did not arrive where it can change the
  answer, or a file past the work budget. The rules make no finding there.
- A read whose lock is Unknown (text that the preprocessor did not expand stands
  after the record phrase) gives no lock hint.
- A search for which the effects analysis names no index (no schema, a table
  the schema does not have, several indexes, text that did not expand) gives
  no index hint.
- A temp-table and a table the schema does not have give no lock, scope or
  transaction hint.
- A hint whose place is in an include gives no hint in the file that includes
  it. A block hint is shown only when the start and the end of the block are
  both in the file.

## When the hints are on

A file shows hints when its configuration enables at least one rule of
`strict.locks`, `strict.transactions` or `strict.access` (also through
`[[workspace.lint.overrides]]`), and `diagnostics` is on in
`[workspace.features]`. These are the groups that read the scope and effects
analysis. The hints do not need the exact rule of their kind:

- A project that opts in to database analysis gets all facts, also before it
  turns on each rule. A fact is useful also where its rule is off.
- A project that did not opt in pays nothing: the analysis does not run, and
  legacy code does not fill with `SHARE-LOCK` hints.

The effect code lenses also need a strict rule: DB001 or TX004.

The server advertises `inlayHintProvider` to every client that supports inlay
hints. A file that does not show hints gets an empty list, so a change of
`oxabl.toml` applies with no restart.

## Settings

The client turns each kind off on its own. The server reads an `inlayHints`
object with the boolean keys `locks`, `index`, `scope` and `transactions`. A
key that is absent keeps its value; at the start every kind is on.

- At the start: `initializationOptions.inlayHints`.
- After a change: `workspace/didChangeConfiguration` with
  `settings.oxabl.inlayHints` (or `settings.inlayHints`). The server then sends
  `workspace/inlayHint/refresh` when the client supports it.

The VS Code settings are `oxabl.inlayHints.locks`, `.index`, `.scope` and
`.transactions`. VS Code's own `editor.inlayHints.enabled` turns all hints off.

## Cost

The scope analysis needs the whole file, so the hints of a file are computed
for the whole file at once. A request answers only the hints in its range.

- The work runs on a worker thread, never on the server's main loop.
- The worker reads the expansion on a snapshot of the session database (the
  memoized expansion that the diagnostics read) and drops the snapshot before
  the analysis. A write to the database waits for every snapshot to drop, so
  an analysis that held one would delay the next keystroke.
- The server keeps the hints of each open file until the expanded text, the
  configuration, the schema or the kinds change. A request for another range of
  the same text, for example after a scroll, costs a filter.
- A request that an edit overtakes answers `ContentModified`, never an empty
  list. The client asks again.
- A kind that is off is not computed. Only the index needs the effects
  analysis.

`crates/oxabl_pipeline/benches/hints_bench.rs` measures the analysis of a
procedure file of about 5,400 lines. On a loaded development machine it took
about 31 ms for all kinds and about 15 ms without the index. That is in the
50 ms interactive budget, and it does not run while the user types.

## Limits

- The lint run of the diagnostics and the hints analyse the scopes each. They
  share no result yet.
- A class file gets no scope hint, as TX005 reports none in a class.
