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

A selection with no extent stands for its source line. An own-line comment that
the selection overlaps is touched too. It gets the indentation that the whole
file gives it.

The edits cover the source lines from the first to the last thing touched. The
blank lines between them are part of the selection, so the blank-line rules
apply to them. The blank lines before the first and after the last thing
touched are not.

Two more rules keep the result the same as the whole-file result:

- A line takes its indentation from the first line of the statement that covers
  it. A statement that starts on a line of the edit and ends below it extends
  the edit to its end. For example, a selection in the `DO` block of
  `IF a THEN DO: ... END. ELSE ...` shares its first line with the `IF`, so the
  `ELSE` line moves with it.
- The edit of each line is the smallest change from the old line to the new
  line. A line that is already right gets no edit.

## Properties

- **Formatting a range and then the whole file gives the formatting of the
  whole file.** `crates/oxabl_formatter/tests/range.rs` checks this for the
  fixtures of the formatter, for awkward sources, and for each of them with its
  indentation removed and with an uneven indentation. The ranges are every span
  of whole lines, every byte as an empty range, random byte spans and pairs of
  them. The same test checks that the edits stay inside the lines from the first
  to the last top-level statement that the range overlaps.
- **A selection of the whole file gives the formatting of the whole file.** The
  blank lines before the first and after the last statement and the final line
  ending are the exception: they belong to no statement.
- **The token stream does not change.** The result of each request goes through
  the semantic guard of the formatter, as the whole-file result does. When the
  guard trips, the answer is no edits.
- **No edit is outside the source lines of the touched statements.**

## When the answer is no edits

An answer with no edits and no message means that the lines have their layout
already. In each other case the server sends a `window/showMessage` warning, a
`window/logMessage` warning, and an empty list of edits:

| Case | Message |
|---|---|
| The file has a parse error, only parses after expansion, or trips the guard | the reason that whole-file formatting gives, for example `file has parse errors; left unchanged (no partial formatting)` |
| The selection holds no statement and no comment on its own line: blank lines only, text between statements, or a comment after code | `the selection holds no statement and no comment on its own line; left unchanged` |
| The edits would change the token stream | `formatting the selection would have altered the token stream; left unchanged` |
| The formatter panicked | the panic message; this is a bug in oxabl |

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
about 15 ms in a release build. Format document takes about 10 ms for the same
file. The cost does not depend on the size of the selection.

## Format on type

Format on type is not offered, and the server does not set
`documentOnTypeFormattingProvider`. The reasons:

- It is not cheap. A `.` that ends a statement would parse and format the whole
  file at each key press, because the indentation of a line needs the blocks
  around it.
- It is not safe to refuse in silence. While a user types, the buffer often has
  a parse error, and the formatter then refuses the file. A message at each
  key press is too much, and no message hides the refusal.
- `.` also ends a decimal, a table and field name, and a file name. The server
  must tell a statement end from them at each key press.

A later version can offer it when the formatter can work on one region of the
tree without the whole file.
