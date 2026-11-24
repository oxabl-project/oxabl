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
  has no kind.
- Block comments, with kind `comment`.
- Each `&IF`, `&ELSEIF`, and `&ELSE` branch, with kind `region`. A branch ends
  before the directive that closes it. The pass reads the branches from the
  tokens, so a conditional in an expression also folds.
- Paired `&ANALYZE-SUSPEND` / `&ANALYZE-RESUME` markers, with kind `region`.
- Paired `// #region` / `// #endregion` markers, with kind `region`.

When the last line of a statement starts with its `END`, the range stops at
the end of the line before. So a folded block shows its first line and its
`END` line, as indentation folding does. A block that is only two lines long
(`DO:` and `END.`) does not fold.

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

1. The token at the position.
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
folds and of the selection spans. The walk recurses once for each level of
the tree, so the parser limits (`Parser::MAX_NESTING`, `Parser::MAX_CHAIN`)
set its depth. Selection reads the cached spans for each position.

The bench `syntax/*` in `crates/oxabl_pipeline/benches/outline_bench.rs` runs
the pass on the files of the outline bench (6,000 and 8,400 lines). The
`parse/*` bench there gives the cost of the lex and parse alone. The stdio test
`a_large_file_answers_both_requests` runs both requests on a file of 33,000
lines, and it fails when a request takes more than 20 seconds in a debug
build.
