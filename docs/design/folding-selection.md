# Folding and selection ranges

The language server answers `textDocument/foldingRange` and
`textDocument/selectionRange` from one syntax pass. The VS Code extension needs
no provider code. `vscode-languageclient` registers both providers from the
server capabilities.

## Source

`oxabl_pipeline::document_syntax(source)` reads the raw buffer, as the outline
does (`docs/design/document-outline.md`). It does no preprocessing and no
include expansion. Every span is a byte range of that buffer.

This source rule has these results:

- Text from an include never creates a range in the includer. The include
  reference is one token of the includer.
- Every `&IF`, `&ELSEIF`, and `&ELSE` branch creates a folding range. This
  includes the branches that the preprocessor does not take. The dimming of
  those branches is a different feature.
- A preprocessor reference such as `{&name}` is a range of its own source
  text, never of its value. So a selection always maps to text in the buffer.
- Parse recovery keeps the ranges of the statements and expressions that the
  parser recovered. Comments, markers, and `&IF` branches come from the
  tokens, so a parse error does not remove them.

The language server caches the syntax facts by document URI and buffer version.
Folding and selection requests for the same version share one lex, one parse,
and one tree walk. An edit or close removes the cache entry.

## Folding

The pass returns these ranges when they cover more than one line:

- Each statement: blocks (`DO`, `REPEAT`, `FOR`, `CASE`, `CATCH`, `FINALLY`),
  classes, interfaces, procedures, functions, methods, constructors,
  destructors, temp-table definitions with their fields, and every other
  statement that covers more than one line (a long `ASSIGN`, `DEFINE BROWSE`
  or `FORM`). These ranges have no kind.
- Each run of two or more `DEFINE` statements in one statement list. This range
  has no kind. A run starts at a `DEFINE` of one line. So a temp-table
  definition at the start of a run keeps its own fold, and the run starts at
  the next `DEFINE`.
- Block comments, with kind `comment`.
- Each `&IF`, `&ELSEIF`, and `&ELSE` branch, with kind `region`. A branch ends
  before the directive that closes it. The pass reads the branches from the
  tokens, so a conditional in an expression also folds.
- Paired `&ANALYZE-SUSPEND` / `&ANALYZE-RESUME` markers, with kind `region`.
- Paired `// #region` / `// #endregion` markers, with kind `region`.

A client folds whole lines. These rules make sure that each range does what
the client shows:

- When the last line of a statement starts with its `END`, the range stops at
  the end of the line before. So a folded block shows its first line and its
  `END` line, as indentation folding does. A block that is only two lines long
  (`DO:` and `END.`) does not fold.
- When code follows the end of a range on its last line, the range stops at
  the end of the line before. So a fold never hides code such as the `&ELSE 3`
  in `2 &ELSE 3` or a statement after `b = 2.`. A comment that ends on that line
  can hide. A range that becomes one line is not sent.
- Only one range starts on each line: the innermost one. VS Code keeps only
  the first range that starts on a line, so the server makes that choice
  itself. In `IF a THEN DO: ... END. ELSE DO: ... END.`, the `DO` blocks fold
  and the `IF` statement does not, so the `ELSE DO:` line stays visible. A
  block comment never takes the line from another range. In
  `FOR EACH t: /* two` with the comment closed on the next line, the `FOR`
  block folds and the comment does not.

The pass counts lines as LSP does: `\n`, `\r\n`, and a lone `\r` each end a
line.

A client can send `rangeLimit` in its `foldingRange` capabilities (VS Code
sends 5000). When the file has more ranges, the server sends the outermost
ranges first, by nesting depth, and then in source order. So the end of a
large file keeps its routine folds. Without the limit, VS Code keeps the first
ranges in source order, and the end of the file has no folds.

When a server answers `foldingRange`, VS Code no longer uses the folding
markers of `language-configuration.json`. So the server returns the
`&ANALYZE` and `#region` ranges itself. An unpaired marker creates no range. A
duplicate span occurs once. "Fold All Block Comments" and "Fold All Regions" use the kinds.

The ranges come from the syntax, not from the indentation. Legacy code with
wrong indentation folds by its structure.

The server maps the byte spans with the negotiated position encoding. It sends
both line and character positions. A client that supports only line folding
can ignore the character positions.

## Selection

Each answer starts at the innermost range and links to its parents. The chain
can contain:

1. The token at the position, also a comment. When the position is between
   two tokens, a word (a name, a keyword, a literal, or a reference) wins over
   a closing `)` or `]`, which wins over other punctuation. The left token
   wins a tie. So a cursor at the end of `Name` in `Customer.Name.` selects
   `Name`, then `Customer.Name`. A cursor between `)` and `.` in
   `foo(a + b).` selects `)`, then `foo(a + b)`.
2. Each containing expression, from small to large.
3. Each containing statement, block, and routine, from small to large.
4. The whole file.

An equal range occurs once. Every parent contains the child range: the pass
does not add a range that does not contain the one before it. A chain has at
most 32 ranges. A deeper chain keeps the innermost 31 ranges and the file.
This limit keeps the nested JSON below the recursion limits of clients. A
request with several positions gets one chain for each position, in order.

The server converts each requested position to a byte offset with the
negotiated position encoding. It converts every returned span back with the
same encoding. CRLF text and non-ASCII text use the same mapping as diagnostics
and document symbols.

## Errors and cost

An unopened document gives `null`. Bad parameters give `InvalidParams`. A
contained panic gives `RequestFailed`, never an empty answer. The panic site
`syntax` (`oxabl_common::panic_sites::SYNTAX`) lets a test inject that panic.

The pass is linear in the tokens and tree nodes, apart from one sort of the
folds and of the selection spans. It finds the bounds of a line in the list of
line breaks with a binary search. It never scans a line once for each
statement, so one long line of many statements costs no more than the same
statements on many lines. The walk recurses once for each level of
the tree, so the parser limits (`Parser::MAX_NESTING`, `Parser::MAX_CHAIN`)
set its depth. Selection reads the cached spans for each position.

The bench `syntax/*` in `crates/oxabl_pipeline/benches/outline_bench.rs` runs
the pass on the files of the outline bench (6,000 and 8,400 lines). The
`parse/*` bench there gives the cost of the lex and parse alone. The stdio test
`a_large_file_answers_both_requests` runs both requests on a file of 33,000
lines, and it fails when a request takes more than 20 seconds in a debug
build. The stdio test `a_long_line_costs_no_more_than_many_lines` puts 150,000
statements on one line and on 150,000 lines. It fails when the one line takes
more than three times as long, plus 2 seconds. That file has 15,000 ranges, and 6,000 of them are not inside another range.
With VS Code's `rangeLimit` of 5000, the editor gets the first 5000 of those
6,000 ranges. A file with more top-level ranges than the limit cannot fold
all of them.
