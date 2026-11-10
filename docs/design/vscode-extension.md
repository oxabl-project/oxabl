# The VS Code extension: grammar, releases and self-update

The extension (`clients/vscode`) has three parts. A TextMate grammar
highlights ABL with no other extension and no `oxabl` binary. A language
client starts `oxabl lsp` for format-on-save and diagnostics when the binary
is there. A self-update keeps the extension current, because it is not on the
Marketplace. This note records how each part works and why.

## The grammar

### Source and generation

- The structure is hand-written in `resources/abl.tmLanguage.template.json`.
- `cargo run -p oxabl_codegen -- grammar` (also part of `all`) fills the
  template from the keyword data and writes
  `clients/vscode/syntaxes/abl.tmLanguage.json`. Never edit the output. The CI
  job `Grammar drift` fails when the committed grammar is not the generated
  one.
- Two placeholders, only inside JSON strings:
  - `{{word:NAME}}` — a regex for the keyword `NAME` and each legal
    abbreviation. Codegen fails when `NAME` is not a keyword.
  - `{{keywords:CATEGORY}}` — a regex for each spelling in a category.
  Both are tries in lower case, used inside `(?i)` patterns. Codegen fails on
  an unknown placeholder.

### Keyword data

- The keywords are the union of the keyword index
  (`resources/abl_keyword_index.html`, every `tr.row`), the reserved list
  (`abl_reserved_keywords.txt`) and the `[[add]]` entries of
  `keyword_overrides.toml`, less its `[[remove]]` entries. Only plain words
  take part.
- A keyword's abbreviation is the shortest one that a source gives and that
  is a prefix of the name (the index has typos, such as `DEFAUT-B`, which are
  ignored). Every prefix from the abbreviation to the full name is a
  spelling.
- Many unreserved index entries are attribute names that are also common
  variable names (`X`, `Y`, `NAME`, `KEY`, `LINE`). A name is not in the
  `other` category when it is unreserved, not in the overrides, documented
  only as an attribute, method, property or event, a single word, and not one
  of a few widget options (`HIDDEN`, `SENSITIVE`, `TOOLTIP`, `VISIBLE`,
  `WIDTH`, …). After `:` it is still highlighted as a member.
- Each spelling has one category; the first that holds it wins:
  `boolean`, `operator-logical`, `operator-comparison`, `operator-pattern`,
  `operator-arithmetic`, `control`, `type`, `modifier`, `handle`, `other`. The
  `function` set (the built-in functions of `builtins.rs` with their
  abbreviations, plus `DEFINED`) applies only before `(`.

### Rules that are easy to get wrong

- A keyword is not a keyword after `.`, before `.` plus a letter (a qualified
  name such as `customer.name` or `user.name`), or inside a longer identifier
  (`my-define`).
- vscode-textmate appends `\n` to every line. The `&GLOBAL-DEFINE` and
  `&MESSAGE` rules end at `(?=\n)`, and a `~` continuation consumes the
  newline, so the directive goes on to the next line.
- The ten keyword categories are one regex with a capture per category: the
  engine scans each line once for them instead of ten times (about 40%
  faster on large files, with the same tokens).
- The engine takes the match that starts first, and among those the rule
  listed first. A rule that starts with `^\s*` starts before the keyword it
  should lose to. So the label rule starts behind the indentation with
  `(?<=^[ \t]{0,200})` (both vscode-oniguruma 1 and 2 accept it), and an
  indented `DO:` stays a keyword. An unbounded `(?<=^\s*)` costs time
  quadratic in the indentation.
- Definitions (`DEFINE`, `PROCEDURE`, `FUNCTION`, `METHOD`, `CLASS`, …) match
  at the start of a line only. This keeps `DELETE PROCEDURE h` and
  `AS CLASS x` from naming a declaration.

### Known limits

- A definition whose name is on the next line gets plain keyword and
  identifier scopes.
- An unreserved keyword used as a name (a variable called `count`) shows as a
  keyword, and so does a label with a keyword's name (`total:`). LSP semantic
  tokens are the planned correction: the parser and the semantic model know
  what each name is.
- The grammar scopes three integer literals joined by `/` with no spaces
  (`10/2/3`) as a date, the same as `12/31/2026`.

### Tests

From `clients/vscode`:

- `pnpm run test:grammar` — assertion tests (`test/grammar/cases`) and
  snapshot tests (`test/grammar/snapshots`), with `vscode-tmgrammar-test`.
- `pnpm test` — includes `test/grammar/corpus.test.ts`: every regex
  compiles, and every ABL file in the repository tokenizes within a time
  budget, on vscode-textmate 9 and vscode-oniguruma 2, the engine VS Code
  uses.
- `xvfb-run -a pnpm run test:e2e` — the packaged VSIX in a downloaded VS Code:
  activation, the colours that the editor renders, Markdown code blocks, the
  missing-binary warning, the self-update against a fake GitHub API, and
  screenshots. It removes every `oxabl` from `PATH` and sets `VSCODE_CLI=1`
  (otherwise VS Code reads the login shell's `PATH` and starts a real
  language server), and fails when a scenario leaves a process running.

## Releases

- Release Please releases the extension as the component `oxabl_vscode`
  (package `clients/vscode`, `release-type: node`). Its tags are
  `oxabl_vscode-vX.Y.Z`. The component is not `oxabl-vscode`, because a tag
  `oxabl-vscode-v1.0.0` starts with `oxabl-v`, and a tool that looks for the
  `oxabl` crate's tags would take it.
- A commit that changes `clients/vscode` (including the generated grammar and
  schema) gives a new extension version.
- `clients/vscode/scripts/package-vsix.sh <out-dir>` writes
  `oxabl-vscode-<version>.vsix` and `oxabl-vscode-<version>.vsix.sha256`
  (`sha256sum` format), and fails when the VSIX misses a file the extension
  loads. CI runs it on every pull request and keeps the VSIX as an artifact.
  The release job `vsix` runs it for the tagged commit, checks that the tag
  names the packaged version, and uploads both files to the release.

## Self-update

VS Code updates only extensions from its gallery, and an extension installed
from a VSIX never updates. Open VSX is not read by VS Code, and a private
Marketplace needs a GitHub Enterprise plan. So the extension updates itself:

1. At most once a day, after activation, it lists the tags
   `GET /repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl_vscode-v`.
   (`/releases/latest` names the last crate released, not the extension.)
2. For the highest `X.Y.Z` above its own version it reads
   `/releases/tags/oxabl_vscode-vX.Y.Z`. A draft, a pre-release, or a release
   without both assets is no update.
3. It downloads the VSIX and the `.sha256`, verifies the digest, and installs
   the VSIX with `workbench.extensions.installExtension`. The new version
   runs after a window reload.

`oxabl.updates` is `auto` (the default; `notify` when VS Code's
`extensions.autoUpdate` is off), `notify` or `off`. `oxabl: Check for Updates`
checks at once. Every failure installs nothing. `OXABL_UPDATE_API_URL`
replaces `https://api.github.com` for the end-to-end tests, and only when it
names a loopback host.

The SHA-256 proves that the VSIX arrived intact. That it is ours rests on
HTTPS to GitHub: the checksum comes from the same release. The version inside
the VSIX is not compared with the tag; the release job checks that the tag
names the packaged version before it uploads.

If the `vsix` job fails, the release has no assets and no extension offers
it. Re-run the failed job in the same workflow run: a `workflow_dispatch` run
never uploads, and a new push does not create the release again.
