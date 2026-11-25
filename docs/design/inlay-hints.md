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

The parameter-name hints (the name of the parameter before each argument of a
call) are in the same answer and share the range and cost rules below. They
are not database facts, and they need no strict rule. See
`docs/design/signature-help.md`.

## Kinds

| Kind | Label and place | Shown for | Selected by |
| --- | --- | --- | --- |
| `locks` | `SHARE-LOCK`, after the buffer name | A `FIND`, a `FOR` or `PRESELECT` phrase, a `CAN-FIND` or an `OPEN QUERY` phrase that reads a database table and names no lock | `default-lock` (LK001) |
| `index` | `index <name>`, after the buffer name; `index <name>, WHOLE-INDEX` when no condition brackets the index | A search of a database table or a temp-table for which the effects analysis names an index | The effects analysis, the facts `find-in-loop` (DB001) reads. No rule turns it on |
| `scope` | `<buffer> scope starts` before the block, `<buffer> scope ends` after it | A buffer scope that a reference outside a `FOR` block raises to the procedure | `scope-widened` (TX005) |
| `transactions` | `transaction starts` before the block, `transaction ends` after it | A block that is a transaction block only because it updates the database, and a main block that is the transaction | `implicit-transaction` (TX001), `procedure-wide-transaction` (TX002) |

Each hint has a tooltip that says why, and names the rule. The lock tooltip
says that oxabl does not read the `-NL` startup option. The transaction tooltip
says that a transaction starts at the block only when none is active yet: a
block that runs inside the caller's transaction is a subtransaction.

When two block pairs start and end at the same place (a raised scope and a
procedure-wide transaction both cover the main block), the end hints come in
the reverse order of the start hints, so the pairs nest.

A lock that the code writes, a `DO TRANSACTION` and a `DO FOR` scope are in the
code, so they get no hint. A `GET` gets no lock hint: it takes the lock of its
query, as LK001 says.

## The hints are the rules' findings

The lock, scope and transaction hints must never say something that the strict
rules do not. So the hints do not compute the facts again, and they do not
select them again. The index hint has no rule to agree with: no rule reports
index choices alone, so it comes from the effects analysis, and needs no rule.
Its tooltip says that no lint rule reports it. The tests compare it with
`oxabl explain --json`, which names the same index.

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
- A read that follows an include that did not arrive gives no index hint. The
  missing include can define a temp-table or a buffer of the same name, so the
  table of the read is not known. A read that comes before the include keeps
  its hint, and so does a read in a file where every include arrived.
- A temp-table and a table the schema does not have give no lock, scope or
  transaction hint.
- A hint whose place is in an include gives no hint in the file that includes
  it. A block hint is shown only when the start and the end of the block are
  both in the file.
- A kind whose rule the file turns off gives no hint (see below). The index
  kind has no rule: a file with no strict rule still gets it.

## Include files

An include has no meaning alone (`docs/design/include-context.md`).

- **With a selected include context** (`oxabl/selectIncluderContext`), the
  hints of the include come from the analysis of the selected root, as its
  diagnostics do. The root is read with the open buffers over the disk. A
  hint is shown when its place is in the include's own text. The tooltip names
  the root. An edit, an open or a close of the root, or of another file that
  the root's analysis read (a sibling or nested include), makes the server
  ask the client again (`workspace/inlayHint/refresh`) once the edits settle,
  after the same debounce as the include's diagnostics, not on each
  keystroke. The include's context diagnostics are analysed again then too.
  The server knows which files the root read from the last hints it
  computed, so before the first hint request of the include only an edit of
  the root itself does this. The root's configuration decides whether hints show. When the root
  expands the include more than once, or no longer includes it, the include
  gets no hint: one place cannot hold two answers.
- **In fragment mode** (no context), the include is analysed alone. A hint
  whose statement holds a `{...}` reference or sits under an `&IF` gives no
  hint: the rules lower the confidence of such findings, because each includer
  can read them differently (`oxabl_lint::fragment::IncludeDependence`). A
  period or a colon ends a statement there only before whitespace, so a
  reference after `customer.region` is in the statement. A hint whose finding
  is not in the include's own text (a reference in a nested include) cannot be
  checked, so it gives no hint either. Every
  other hint says in its tooltip that a file that includes it can change it,
  for example with a temp-table of the same name.

## When the hints are on

A file shows hints when `diagnostics` is on in `[workspace.features]`. The
`index` kind needs no rule: a project with no strict rule gets it. Each other
kind follows its own rule, for the file's path (also through
`[[workspace.lint.overrides]]`):

| Kind | Shown when this rule is on |
| --- | --- |
| `locks` | LK001 |
| `scope` | TX005 |
| `transactions` | TX001 for a nested block, TX002 for the main block |
| `index` | no rule |

A project that turns a rule off does not hold its fact. For example, a project
that runs with `-NL` turns LK001 off, and then a read with no lock phrase does
not take `SHARE-LOCK`.

The effect code lenses also need a strict rule: DB001 or TX004.

The server advertises `inlayHintProvider` to every client that supports inlay
hints. A file that does not show hints gets an empty list, so a change of
`oxabl.toml` applies with no restart.

## Settings

The client turns each kind off on its own. The server reads an `inlayHints`
object with the boolean keys `locks`, `index`, `scope`, `transactions` and
`parameters`. A key that is absent keeps its value; at the start every kind is
on, except `parameters`, which is off.

- At the start: `initializationOptions.inlayHints`.
- After a change: `workspace/didChangeConfiguration` with
  `settings.oxabl.inlayHints` (or `settings.inlayHints`). The server then sends
  `workspace/inlayHint/refresh` when the client supports it.

The VS Code settings are `oxabl.inlayHints.locks`, `.index`, `.scope`,
`.transactions` and `.parameters`. VS Code's own `editor.inlayHints.enabled`
turns all hints off.

## Cost

Issue #411 asks that hints are computed only for the requested range. The
scope analysis cannot do that: a buffer's scope and a transaction depend on
references anywhere in the routine, and a raised scope on references anywhere
in the file. So the hints are **computed once per text and filtered per
range**: a file is analysed whole once for each version of its text, and each
request returns only the hints in its range.

- One worker thread answers all inlay hint requests, in order. At most one
  analysis runs at a time.
- A request whose file changed before the worker reached it answers
  `ContentModified` with no analysis. VS Code asks again for the new text,
  because the document changed. For an include in a context, an edit of the
  open root does the same, and the debounced refresh makes the client ask
  again. An edit, an open or a close of any other file does not.
- The analysis of the database hints holds no snapshot of the session
  database. A write to the database waits for every snapshot to drop, so an
  analysis that held one would delay the next keystroke, and an edit of any
  other buffer would cancel it.
- The parameter-name hints read other files, so their analysis does hold a
  snapshot, and the two things above are true of it. A write waits until the
  snapshot drops; it is a few ms to tens of ms for a file of 7,000 lines. A
  write cancels the analysis. The worker then runs it again on a new snapshot
  while the text of the file is the same, up to four times. When the text
  changed, the request answers `ContentModified`, and the client asks again.
- The server keeps the hints of each open file until its text, its include
  context, the configuration, the schema, a file on disk (a watched-file
  event) or the kinds change. In a context it also keeps the files the root's
  analysis read, and the hints stay until the open-buffer text of one of them
  changes; an edit of a file that the root does not read keeps them. A request
  for another range of the same text, for example after a scroll, costs a
  filter. The key holds the file's own text, not only the expanded text: an
  edit of a `&SCOPED-DEFINE` line can keep the expanded text and move every
  offset.
- A kind that is off, or whose rule is off, is not computed. When no kind
  is wanted (the index setting is off and no strict rule is on), the file is
  not analysed. The parameter-name hints are computed apart from the database
  hints, once for each version of the text, and they are kept only while no
  open buffer or file on disk changes, because they read other files.
- Only the index needs the effects analysis. It needs the scope analysis, the
  flow analysis and the call facts too, so a project with no strict rule pays
  for these when the editor asks for hints. The analysis is for the requested
  file only. The server keeps the result until the file's text, configuration
  or schema changes, so a scroll costs a filter.

The tests of this behaviour do not depend on the speed of the machine. In a
test build (the `test-panics` feature of `oxabl_common`), the comment
`OXABL-TEST-HOLD:lsp.inlay-hints` in the analysed text makes the analysis wait
2 s, so edits sent meanwhile are queued behind it. The comment
`OXABL-TEST-HOLD:lsp.parameter-hints` makes the analysis of the parameter-name
hints wait the same time while it holds its snapshot (`@600` after the name
sets the time in ms).

`crates/oxabl_pipeline/benches/hints_bench.rs` measures the analysis of a
procedure file of about 5,400 lines. On a loaded development machine it took
about 16 ms for all kinds, 13 ms without the index (`hints/scopes`) and 15 ms
for the index alone (`hints/index`, what a project with no strict rule gets).
That is in the 50 ms interactive budget, and it does not run on the server's
main loop. The cost grows in step with the size of the file:
`hints/many_procedures` (2,400 procedures, about 53,000 lines, the index
alone) takes about 0.4 s in a release build. It took 4.2 s
there before the scope analysis stopped to compare each reference with every block of its
buffer. A file of that size is still analysed whole once for each version of
its text, so its hints come late; the worker never blocks the main loop.

Over `oxabl lsp` (release build, a file of 5,400 lines), the first
request for a text took about 35 ms in a project with no strict rule, against
about 15 ms before, when it answered an empty list with no analysis. Each
later range answered in under 1 ms.

## Limits

- The lint run of the diagnostics and the hints analyse the scopes each. They
  share no result yet.
- A class file gets no scope hint, as TX005 reports none in a class.
