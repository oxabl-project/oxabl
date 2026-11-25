# Format a selection

The language server answers `textDocument/rangeFormatting` and
`textDocument/rangesFormatting`. The VS Code extension needs no provider code.
`vscode-languageclient` registers the provider from the server capability. The
server sets `documentRangeFormattingProvider` to `{ "rangesSupport": true }`.
`lsp_types` has no field for `rangesSupport`, so the handshake writes it.

## What a selection formats

The formatter works per source line. It sets the indentation of each line from
the blocks around the line, and it keeps the text of the line. So the server
does not format a selection alone. It formats the whole file and keeps the
output of some lines. This is what `oxabl_formatter::format_ranges` does. The
shared `FormatPipeline::format_ranges` calls it.

A selection touches a statement when it overlaps the statement. A selection
that starts or ends inside a statement takes the whole statement. Two rules
choose the statements:

- The selection is inside one body when it starts after the code before the
  first statement of the body (the `:` of a header, `THEN`, `ELSE`) and ends
  before the code after the last one (`END`, `ELSE`). Only the statements of
  that body that the selection overlaps are touched. The rule repeats in the
  statements that it touches, so a selection inside a block in a procedure
  touches only the statements of that block.
- Any other selection touches the whole statement that it overlaps. This
  includes a selection that reaches the header or the `END` of a block, or that
  reaches the `ELSE` or a `WHEN` of a statement.

A selection with no extent stands for its source line. On a blank line, it
touches only a statement or a comment that runs across that line, such as a
string with a blank line in it. An own-line comment that the selection overlaps
is touched too. It gets the indentation that the whole
file gives it.

The edits cover the source lines from the first to the last thing touched. The
blank lines between them are part of the selection, so the blank-line rules
apply to them. The blank lines before the first and after the last thing
touched are not.

Three more rules keep the result the same as the whole-file result:

- A line takes its indentation from the first line of the statement that covers
  it. A statement that starts on a line of the edit and ends below it extends
  the edit to its end. For example, a selection in the `DO` block of
  `IF a THEN DO: ... END. ELSE ...` shares its first line with the `IF`, so the
  `ELSE` line moves with it.
- Some lines take their indentation relative to an earlier line, the *anchor*.
  A continuation line moves by the same amount as the first line of the
  statement that covers it, and the other lines of a multi-line comment move
  with its first line. When the anchor is not in the edit, the line keeps its
  leading white space, and only the rest of the line changes. If the line
  took its new indentation, the next whole-file format would move it again,
  because its anchor did not move. For example, in `x = 1. /* a` followed by
  ` b */` inside a `DO` block, the second line is a continuation of the `DO`.
  A selection of `x = 1.` does not include the `DO` line, so the second line
  keeps its indentation. The formatter gives the anchor of each line with its
  output.
- The edit of each line is the smallest change from the old line to the new
  line. A line that is already right gets no edit.

## Properties

- **Formatting a range and then the whole file gives the formatting of the
  whole file.** `crates/oxabl_formatter/tests/range.rs` checks this for the
  fixtures of the formatter, for awkward sources, and for each of them with its
  indentation removed, with an uneven indentation, and with a constant
  indentation added. It uses the `default_base` and the `oestandards` styles.
  The ranges are every span of whole lines, every byte as an empty range,
  random byte spans and pairs of them. The same test checks that the edits stay
  inside the lines from the first to the last top-level statement that the
  range overlaps.
- **The touched statements get their layout.** For each range, the first line
  of each statement that the range touches is the line that the whole-file
  format gives. The test also refuses a `no statement` answer for a range that
  covers code of a statement.
- **A selection of the whole file gives the formatting of the whole file.** The
  blank lines before the first and after the last statement and the final line
  ending are the exception: they belong to no statement.
- **The token stream does not change.** The result of each request goes through
  the semantic guard of the formatter, as the whole-file result does. When the
  guard trips, the answer is no edits.
- **No edit is outside the source lines of the touched statements.**

## When the answer is no edits

An answer with no edits and no message means that the lines have their layout
already. In each other case the server sends an empty list of edits and a
`window/logMessage` warning that says why. VS Code sends a range request at each
paste (`editor.formatOnPaste`) and at each save with
`editor.formatOnSaveMode` set to `modifications`. While the user types, the
buffer often has a parse error, and a changed range is often only a blank or
deleted line. So a refusal that comes from the buffer goes to the log only. A
refusal that comes from a defect in oxabl also gets a `window/showMessage`
warning:

| Case | Message | Popup |
|---|---|---|
| The file has a parse error or only parses after expansion | the reason that whole-file formatting gives, for example `file has parse errors; left unchanged (no partial formatting)` | no |
| The selection holds no statement and no comment on its own line: blank lines only, text between statements, a comment after code, or no range | `the selection holds no statement and no comment on its own line; left unchanged` | no |
| The whole-file format trips the guard | `formatting would have altered the token stream; left unchanged` | yes |
| The edits would change the token stream | `formatting the selection would have altered the token stream; left unchanged` | yes |
| The formatter panicked | the panic message; this is a bug in oxabl | yes |

A buffer that is not open, or `format = false` in `[workspace.features]`, gives
an empty list and no message, as `textDocument/formatting` does.

A selection in an include reference or a preprocessor directive is not refused:
the formatter lays these lines out in the whole file, so it lays them out in a
selection too. The formatter reads the raw text and never expands includes or
macros.

The client options (`tabSize`, `insertSpaces`) stay ignored, as they are for the
whole document. The resolved style owns the indentation.

## Cost

One request parses and formats the whole file, lexes it again for the selection
rules and for the guard, and walks the tree once. A file of 10,000 lines takes
about 15 to 20 ms in a release build, about the same as format document. The
cost does not depend on the size of the selection. Each range looks only at the
top-level statements and comments near it, and the server removes duplicate
ranges, so 2,000 ranges also take about 20 ms.

## Format on type

Format on type is not offered, and the server does not set
`documentOnTypeFormattingProvider`. The reasons:

- The refusals are noise. While a user types, the buffer often has a parse
  error, for example an open `DO:` with no `END` yet, and the formatter then
  refuses the whole file. Format on type would refuse at most key presses. A
  message at each refusal is too much, and silence makes the feature look
  broken at random.
- `.` is ambiguous. It also ends a decimal (`1.5`), a table and field name
  (`Customer.Name`) and a file name (`{inc.i}`), and it ends a statement only
  when white space or the end of the line follows it. The server must tell a
  statement end from the others at each key press.

The cost is not the reason: one request takes about the same time as format
document (see "Cost"). A later version can offer format on type when the
formatter can lay out a region whose blocks are not closed yet.
