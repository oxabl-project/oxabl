# Quick fixes

A lint finding can carry fixes. A fix is a set of text edits that removes the
finding. `oxabl lsp` offers each fix as a code action of kind `quickfix`
(`textDocument/codeAction`). `oxabl check --fix` applies the preferred fix of
each finding and writes the file. `oxabl check --json` shows the fixes of each
finding. The steps that build it are in `quick-fixes-steps.md`.

The first fixes:

| Rule | Fix |
| --- | --- |
| `missing-no-undo` (TX006) | Add `NO-UNDO` to the definition |
| `unused-variable` (LINT0002) | Remove the `DEFINE VARIABLE` statement |
| `undefined-symbol` (LINT0001), a class name | Add `USING pkg.Name.` for the one class of that name |
| `default-lock` (LK001) | Add `NO-LOCK`, or add `EXCLUSIVE-LOCK` (two fixes) |

## The rule that decides every case

**Fail closed.** A fix is offered only when oxabl knows that the edit changes
the text that it reads, and only that text. When oxabl does not know where the
text came from, what an `&IF` gives, or which class a name means, it offers no
fix. The finding stays, with its help line. A fix that oxabl is not sure of is
worse than no fix, because a person or a CI job applies it without reading it.

## Terms

| Term | Meaning |
| --- | --- |
| Fix | A title, one or more edits, a preferred flag, and the finding it belongs to. |
| Edit | A byte range of one file and the new text for it. An empty range is an insertion. |
| Anchor | For an insertion, the token that the insertion follows (or, at the start of a line, the token it comes before). |
| Own text | The bytes of the analysed file that the file itself writes. Text that an include writes, and text that the preprocessor makes (`{&name}` values, `{1}` arguments), is not own text. |
| Preferred fix | The fix that `--fix` applies. A finding has at most one. |
| Token guard | The check that the token stream after the edits is the token stream before them, with the intended change and nothing else. |

## The fix model

`oxabl_common` holds the model. `Diagnostic` gets one field.

```rust
pub struct Diagnostic {
    // ... the fields of today ...
    /// The fixes of this finding, preferred first. Empty for most findings.
    pub fixes: Vec<Fix>,
}

pub struct Fix {
    /// The text a client shows, for example "Add NO-UNDO".
    pub title: String,
    /// The edits, sorted by start, not overlapping.
    pub edits: Vec<Edit>,
    /// Whether `--fix` applies this fix and the editor marks it preferred.
    pub preferred: bool,
    /// What the edits must keep true before a client can see the fix.
    pub conditions: Vec<FixCondition>,
    /// A fix whose edits the pipeline makes, because the rule cannot see what
    /// they need. `edits` is empty until then. The pipeline fills the edits or
    /// drops the fix; no client sees a deferred fix.
    pub deferred: Option<Deferred>,
}

pub enum Deferred {
    /// Add `USING <the one class of this simple name>.` (see the `USING` fix).
    AddUsing { name: String },
}

pub struct Edit {
    /// While the rule runs: a span of the expanded text. After the collector:
    /// a span of the own text of one file (see "From expanded text to the file").
    pub span: FileSpan,
    /// The new text. Keywords are written in upper case; see "Keyword case".
    pub new_text: String,
    /// The statement or phrase that the edit changes, in the same coordinates.
    /// The collector checks it for elided references.
    pub statement: FileSpan,
}

pub enum FixCondition {
    /// No edit lies in an `&IF` branch, taken or not.
    OutsideConditionalBranch,
    /// No text in a branch that was not taken holds this word (case-insensitive,
    /// whole word), in the analysed file or in an include it reads.
    NotInSkippedText(String),
    /// The expansion lost no text (`missed_expansion` and `text_losses` are empty).
    WholeExpansion,
}
```

`Diagnostic::with_fix(fix)` adds a fix. A rule makes its fixes when it makes
the finding. Its edits are in the expanded text, as the span of the finding
is. A rule that cannot place an edit makes no fix. It never makes a fix with an
edit that it is not sure of.

A fix never changes the finding. The message, the span, the help line and the
severity stay as they are today. Only the new field is new.

The `title` is plain English with keywords in upper case. It does not change
with the keyword case of the project, so a test and a person can rely on it.

### Keyword case

A rule writes each keyword of `new_text` in upper case. The pipeline writes
them in the keyword case of the project (`[workspace.style] keyword_case`)
when it places the fix:

- `uppercase`: upper case.
- `lowercase`: lower case.
- `preserve` (the default): the case of the first keyword of the statement
  that the edit changes (`Edit::statement`). When that keyword has no letter
  case (all lower case gives lower case, all upper case gives upper case, mixed
  case gives upper case), the nearest keyword before it. Else upper case.

This is the rule that completion uses (`completion/case.rs`). The fix code
reuses that function, so the two never disagree.

Names (a class name, a table name) keep their spelling.

### Line breaks

A rule writes a line break as `\n`. The pipeline writes the line break of the
file in its place: `\r\n` when the first line break of the file is `\r\n`,
else `\n`. A delete of whole lines takes the line break of the file with it,
whichever it is.

## From expanded text to the file

The collector in `oxabl_analyze` (`collect.rs`) maps the span of each finding
from the expanded text to the root file. It maps the edits of each fix in the
same pass, with stricter rules (1 to 6 below). Then `LintPipeline::collect` in
`oxabl_pipeline`, which every client calls, writes the keywords in the
project's case, fills a deferred fix, and runs rule 7. A fix with an edit that
does not map is dropped. The finding stays.

An edit maps when all of these are true:

1. The analysed file is a root, not a fragment. An include file that a client
   checks alone (`oxabl check` on a `.i`, an open `.i` in fragment mode) gets no
   fixes. An open include with a selected includer context
   (`docs/design/include-context.md`) gets no fixes.
2. Each byte that the edit replaces is own text of the root, in one chunk of
   the expansion (`ChunkOrigin::Root`). For an insertion, the anchor is own text
   of the root, in the same chunk as the insertion point.
3. The parse did not recover (`CollectedDiagnostics::parse_recovered` is
   false). A file with a `PARSE001` anywhere in its expansion gets no fixes.
4. The edit does not lie in an `&IF` branch whose state is `Undecided`.
5. No elided reference (`ExpandedFile::elided_offsets`) lies in
   `Edit::statement`. Such a reference can put text there, for example a
   `{&NOUNDO}` that a caller defines.
6. Each `FixCondition` of the fix holds.
7. The fix alone passes the token guard (below) on the root's own text.

Then the edit span is a span of the root file's own bytes, with the root's
`FileId`, and the keywords are in the project's case.

A finding in an include (`include_origin`, `include_findings`) keeps its fixes
out: the collector drops them. A fix of v1 never edits an include file.

### A root file that another unit includes

The compiler lets a unit include a `.p` or a `.w` file. Then a fix changes
that unit too, and a variable that this file does not read can be read there.
oxabl checks this where it knows the includers:

- `oxabl check --fix` and `oxabl check --json` drop the fixes of a file that
  another root of the same run includes (the run reads the include list of
  every root it checks).
- `oxabl lsp` drops them when the newest workspace pass of the daemon
  (`ReverseGraph::dependents`) shows an include edge to the file. When there is
  no workspace pass yet, the server does not wait for one.

A unit outside the run, or an include edge that is newer than the workspace
pass, is not seen. This is a known gap. It is in the report of the plan as a
question for the owner. An `.i` file, the usual include, never gets fixes
(rule 1).

## The token guard

The formatter's guard (`oxabl_formatter::guard`) compares the non-trivia token
stream of the input and the output. The fix guard uses the same comparison
(`tokens_equal`) with one change: it expects the tokens of the edits.

1. Lex the text before the edits. Each edit must start and end on a token
   boundary or in trivia (white space, a comment). An edit that cuts a token
   or a comment fails the guard.
2. Make the expected stream: the tokens before the edits, with the tokens in
   each replaced range taken out, and the tokens of each `new_text` put in at
   that place.
3. Lex the text after the edits. Its stream must equal the expected stream,
   token by token, with the formatter's rules (keywords by kind, literals by
   value, identifiers by text). The `END` and `END <type>` allowance of the
   formatter does not apply.
4. The literal TAB columns must not change (`literal_tab_columns`).

The guard runs for each fix alone, when the collector places it (rule 7). It
runs again in `--fix` for all the fixes of a file together.

## `oxabl check --fix`

`--fix` applies the preferred fix of each reported finding, file by file.

### Which fixes

- Only findings that the run reports. A finding that `--baseline` hides is not
  fixed. A finding in an include file has no fix.
- Only preferred fixes. A finding with no preferred fix (the `default-lock`
  finding whose help names no lock) is not fixed.
- `--fix-only <RULE>` (by code or by kebab name, repeatable, comma-separated
  values too) limits the fixes to those rules. An unknown rule name is a usage
  error (exit 2). `--fix-only` without `--fix` is a usage error.
- `--fix` with `--no-lint`, `--update-baseline` or `--watch` is a usage error
  (exit 2).

### Several fixes in one file

1. Collect the preferred fixes of the file. Two fixes with the same edits
   (same spans, same text) count once. Two `USING` fixes for the same class
   are the same fix.
2. Drop each fix whose every edit lies in a range that another fix deletes
   (new text empty). Example: the `NO-UNDO` fix of a variable that the
   `unused-variable` fix removes.
3. Sort the edits by start. Insertions at the same offset keep the order of
   their findings in the file (by finding start, then by title).
4. If two edits overlap (one replaced byte in both, or an insertion strictly
   inside a replaced range), the file is not fixed: `two fixes change the same
   text`.
5. Apply the edits from the end of the file to the start, so no offset moves.
6. Run the token guard on the whole file with all edits. A failure: `the fixed
   text changes more than the fixes`.
7. Analyse the fixed text again, with the same configuration. The file is not
   fixed when its parse recovered (`the fixed text does not parse`) or when a
   finding that a fix removed is still there: same code, at the span the
   finding maps to after the edits (`a fixed finding is still reported`).
   Other findings may come and go: a `USING` can let a type check run that
   could not run before.
8. Read the file from disk again. If its bytes are not the bytes that the run
   read, the file is not fixed: `the file changed on disk during the run`.
9. Encode the text in the file's encoding (`[workspace.sources].encoding`,
   with the byte order mark of the original) and write it. An encoding error or
   a write error is a failure of the run (`failures` in JSON, exit 1).

**All apply or none.** A file is written only when every one of its fixes
passes steps 4 to 9. Otherwise the file stays as it is, byte for byte, and the
run says why. Other files are not affected.

`--fix` is one pass. A fix that makes a new fixable finding is not applied
again in the same run.

### What the run reports

The findings are those of the text on disk at the end of the run: the fixed
text for a written file, the old text for a file that was not fixed. The exit
code comes from those findings, as without `--fix`. The format channel checks
the fixed text.

Text output: one line on stderr for each file with fixes, before the findings.

```text
fixed src/order.p: 3 fixes
not fixed src/legacy.p: two fixes change the same text
```

`1 fix` in the singular.

JSON: the report has a `fix` object, only with `--fix`:

```json
"fix": {
  "fixed": [{ "path": "src/order.p", "fixes": 3 }],
  "not_fixed": [{ "path": "src/legacy.p", "reason": "two fixes change the same text" }]
}
```

### Test faults

As the other `OXABL-TEST-*` markers (`oxabl_common::panic_guard`), a source
with these markers changes the fix engine in builds with the test-only
`test-panics` feature, and does nothing in any other build:

- `OXABL-TEST-FAULT:fix.conflict`: the engine adds to the first fix of the
  file an edit that overlaps the edit of the second fix. Step 4 refuses the
  file.
- `OXABL-TEST-FAULT:fix.guard`: the engine adds one `+` token at the end of the
  fixed text. Step 6 refuses the file.
- `OXABL-TEST-PANIC:fix.apply`: the engine panics while it applies the fixes of
  the file. The panic is contained; the file is not written; the run reports
  an internal failure for it (`failures`, exit 1).
- `OXABL-TEST-HOLD:fix.write`: the engine waits (`HOLD_TIME`, or the
  milliseconds after `@`) before step 8 reads the file again, so a test can
  change the file in that time.

The markers act in the `--fix` engine only, not in the code action request.
The black-box tests use them to show the "all or none" rule, because no pair
of first fixes can overlap.

## JSON output of `oxabl check`

`version` goes from 5 to 6. A diagnostic row gets `fixes`, omitted when empty,
as `related` is:

```json
{
  "path": "src/order.p",
  "code": "TX006",
  "severity": "warning",
  "source": "lint",
  "message": "variable `iCount` is defined without NO-UNDO",
  "help": "Add `NO-UNDO` to the definition.",
  "span": { "start": 16, "end": 22 },
  "start": { "byte": 16, "line": 1, "column": 17 },
  "end": { "byte": 22, "line": 1, "column": 23 },
  "fixes": [
    {
      "title": "Add NO-UNDO",
      "preferred": true,
      "edits": [
        {
          "path": "src/order.p",
          "span": { "start": 33, "end": 33 },
          "start": { "byte": 33, "line": 1, "column": 34 },
          "end": { "byte": 33, "line": 1, "column": 34 },
          "new_text": " NO-UNDO"
        }
      ]
    }
  ]
}
```

- `span` and the positions use the coordinates of the diagnostic: bytes of the
  decoded text, 1-based lines, 1-based byte columns.
- `path` is spelled as the diagnostic's `path`. Each edit has its own `path`,
  so a later fix can edit a second file without a new schema.
- `preproc` rows and `include_diagnostics` rows have no `fixes`.
- The `fix` object is there only with `--fix`.

Compatibility: the change only adds keys. A reader that ignores unknown keys
reads version 6 as version 5. The baseline key does not read `fixes`, so a
baseline file stays valid.

## The code action request

`oxabl lsp` advertises

```json
"codeActionProvider": { "codeActionKinds": ["quickfix"], "resolveProvider": false }
```

and answers `textDocument/codeAction`:

1. When `context.only` is given and holds neither `quickfix` nor a prefix of
   it, the answer is `[]`.
2. The server takes the diagnostics of the document's current text: the same
   memoized query that publishes them (`compute_diagnostics`). A request on a
   text that the server has not analysed yet runs the analysis, as a published
   diagnostic would.
3. For each finding whose range touches the request range (an empty request
   range at the start or the end of a finding touches it), each fix becomes a
   code action:

   ```json
   {
     "title": "Add NO-UNDO",
     "kind": "quickfix",
     "diagnostics": [ <the finding as the server publishes it> ],
     "isPreferred": true,
     "edit": { ... }
   }
   ```

   `isPreferred` is there only for the preferred fix.
4. The edit is a `WorkspaceEdit`. When the client advertises
   `workspace.workspaceEdit.documentChanges`, it is `documentChanges` with one
   `TextDocumentEdit` whose `textDocument.version` is the version of the
   analysed text. Else it is `changes` with the document URI. Ranges are in the
   negotiated position encoding.
5. The actions are sorted by finding start, then in the order of the
   finding's fixes. A rule puts its preferred fix first; with no preferred
   fix, the rule's own order stays (`default-lock`: `NO-LOCK`, then
   `EXCLUSIVE-LOCK`).

A document that is not open answers `null`. An include file (rule 1) answers
`[]`. A request that the client cancels, or whose document changes before the
answer, answers as the other requests of the server do (`RequestCanceled`,
`ContentModified`).

### No `codeAction/resolve`

The edits come with the action. The first fixes are cheap: each is a few
tokens around the finding, found when the rule runs. The `USING` fix needs the
class listing, which the daemon keeps (see the `USING` fix); after the first
listing a lookup is a map read. So the code action request costs the memoized
diagnostics and a conversion of positions. A resolve step would save nothing
and would add a second request in which the text can change. A later fix that
is costly to compute (#443) can add `resolveProvider: true` and put its edits
in `codeAction/resolve` with the document version in `data`; the model does
not change for that.

The client checks the version of `documentChanges`, so an action that the user
picks after an edit is refused by the editor and not applied to the wrong text.

## The first fixes

### Add `NO-UNDO` (TX006)

Title `Add NO-UNDO`, preferred. The edit inserts ` NO-UNDO` (a space, then the
keyword).

| Definition | Insertion point |
| --- | --- |
| `DEFINE VARIABLE x AS <type>`, `DEFINE VARIABLE x LIKE <field>` | After the last token of the type phrase: the data type keyword, or the field name of `LIKE` |
| `DEFINE INPUT PARAMETER p AS <type>` (each direction), `LIKE` too | The same |
| `DEFINE TEMP-TABLE tt` | After the table name |
| `DEFINE PROPERTY P AS <type>` | After the data type keyword |

Examples:

```abl
DEFINE VARIABLE iCount AS INTEGER INITIAL 5.        /* before */
DEFINE VARIABLE iCount AS INTEGER NO-UNDO INITIAL 5. /* after  */

DEFINE TEMP-TABLE ttItem FIELD ItemNum AS INTEGER.
DEFINE TEMP-TABLE ttItem NO-UNDO FIELD ItemNum AS INTEGER.
```

No fix when:

- the definition is `SHARED`, `NEW SHARED` or `NEW GLOBAL SHARED`: the
  producer and the consumers must change together (the rule's help says so);
- a temp-table header writes `UNDO`: the author asked for undo;
- the type phrase is not plain tokens: an `{&name}` or an `{include}` in it or
  after it in the statement (rules 2 and 5), or an `&IF` in the type (the rule
  does not report it);
- the fix cannot find the type phrase in the tokens of the statement (`AS` or
  `LIKE` after the name, then the type). Then it makes no fix.

### Add a lock (LK001)

Two fixes: `Add NO-LOCK` and `Add EXCLUSIVE-LOCK`. The edit inserts
` NO-LOCK` or ` EXCLUSIVE-LOCK` at the end of the record phrase: after its
last token (the table name, a field list, `OF <table>`, the `WHERE`
expression, `USE-INDEX <index>`), and before the parts of the statement that
are not in the record phrase (`NO-ERROR`, `BY`, `BREAK`, `WHILE`,
`TRANSACTION`, `ON`, `WITH`, `:`, `,`, `.` and the `)` of a `CAN-FIND`).

```abl
FIND FIRST Customer WHERE Customer.CustNum = 1 NO-ERROR.
FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR.

FOR EACH Customer WHERE Customer.Balance > 0 BY Customer.Name:
FOR EACH Customer WHERE Customer.Balance > 0 NO-LOCK BY Customer.Name:

FOR EACH Customer NO-LOCK, EACH Order OF Customer:
FOR EACH Customer NO-LOCK, EACH Order OF Customer NO-LOCK:
```

When the record phrase writes `NO-WAIT`, the lock goes before it, because
`NO-WAIT` belongs to the lock.

The preferred fix follows the help line of the finding:

| The help says | Preferred |
| --- | --- |
| Nothing changes the record later: add `NO-LOCK` | `Add NO-LOCK` |
| The record is changed later: add `EXCLUSIVE-LOCK` | `Add EXCLUSIVE-LOCK` |
| Write the lock phrase the read needs | none: the editor shows both, `--fix` applies neither |

A `CAN-FIND` gets only `Add NO-LOCK`, preferred: ABL does not allow
`EXCLUSIVE-LOCK` in a `CAN-FIND`.

The scope model does not keep the end of a record phrase today (`RecordRead`
has `at`, the table name). The step adds the end of the phrase to
`RecordRead`, from the parser. No fix when that end is not known.

### Remove an unused variable (LINT0002)

Title ``Remove unused variable `name` ``, preferred. The edit deletes the
lines of the statement, with the line break at its end.

The fix is offered only when all of these are true:

- the symbol is a variable, not a parameter;
- its declaration is a `DEFINE VARIABLE` statement, or a `VAR` statement with
  one name;
- the statement starts a line (only white space before it) and ends one (only
  white space after its period). A comment on the same line stops the fix;
- in a class, the variable is `PRIVATE`, written or by default. A `PUBLIC`,
  `PROTECTED`, `PACKAGE-PRIVATE` or `PACKAGE-PROTECTED` variable can be read
  by another class;
- conditions `OutsideConditionalBranch`, `NotInSkippedText(name)` and
  `WholeExpansion`: another set of defines, or text that oxabl did not see,
  can read the variable;
- the file is not an include target of another unit (see above).

The deleted range runs from the start of the first line of the statement to
the start of the line after its last line. At the end of a file with no line
break after the statement, the range ends at the end of the file.

```abl
DEFINE VARIABLE iUsed AS INTEGER NO-UNDO.
DEFINE VARIABLE iUnused AS INTEGER NO-UNDO.
DISPLAY iUsed.
```

becomes

```abl
DEFINE VARIABLE iUsed AS INTEGER NO-UNDO.
DISPLAY iUsed.
```

A comment line above the statement stays.

### Add a `USING` (LINT0001)

Title ``Add `USING pkg.Name.` `` (the qualified name as the class header
writes it), preferred.

The finding: `undefined-symbol` on a class name that the resolver searched as
a class (`NEW Name()`, the type operand of `CAST`, `TYPE-OF`, `GET-CLASS`),
with the reason `AbsentFromWorkspace` and a name with no `.`. The rule adds a
deferred fix (`Deferred::AddUsing`) to such a finding. The pipeline makes its
edit or drops it, because the rule cannot list the classes of the workspace.

**The class listing.** The fix reads `ClassListing`, the list of `.cls` files
under each include path entry that `completion.md` ("The class listing")
describes. This plan moves the listing to `oxabl_pipeline` (no salsa), so
`oxabl check` and the daemon share one implementation. The CLI makes it once a
run, at the first finding that needs it. The daemon keeps it as completion
does.

The fix is offered only when all of these are true:

- the listing is complete (not past its file limit, each entry listed);
- exactly one listed class has the simple name (case-insensitive), and its
  header scan found its declared name;
- no built-in class (#154 catalog) has the simple name;
- no `USING` of the file already names a class of that simple name;
- the `USING` edit can be placed (below), with conditions
  `OutsideConditionalBranch` and `WholeExpansion`.

**The `USING` edit** is the one of completion ("The `USING` edit"), made by
the same function from the root's own text:

- after the last `USING` statement at the top of the file: a line break, then
  `USING pkg.Name.`, at the indentation of that statement;
- with no `USING`: at the start of the line of the first statement, after a
  header comment: `USING pkg.Name.` and a line break;
- no edit when an include reference or a preprocessor directive comes before
  the first statement, or the first statement or the last `USING` is not own
  text.

All `NEW Name()` findings of one file get the same fix, so `--fix` adds one
`USING`.

## Out of scope

- Fixes that edit an include file, or more than one file (a shared
  `NO-UNDO` on the producer and every consumer).
- A fix for each finding of other rules. Each new fix is a new step that uses
  this model.
- `source.fixAll`, "fix all of this rule in the file", and fixes in the wasm
  client.
- Applying fixes again until no fix is left (`--fix` is one pass).

## The seams for source actions (#443)

Source actions (keyword case, abbreviations, organize `USING`, AppBuilder
markup) use this plan's parts:

- `Edit` and the token guard. A source action is a list of edits with an
  expected token change, like a fix. Keyword recasing keeps each token kind,
  so its expected stream is the stream before it.
- The mapping rules of "From expanded text to the file", for an action that
  reads the analysis (organize `USING` reads the resolver).
- The keyword case function.
- The `USING` edit and the class listing.
- The apply engine of `--fix` (dedupe, overlap, all or none, re-read before
  write, encoding). `oxabl fix --keyword-case` can call it with the edits of
  an action in place of the preferred fixes.
- `codeAction` dispatch: #443 adds `source.*` kinds next to `quickfix`, and a
  `codeAction/resolve` for the costly actions.
