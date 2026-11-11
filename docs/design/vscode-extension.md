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
  - `{{keywords:CATEGORY}}` — a regex for each spelling in a category, or
    in the `function` or `reserved` set.
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
- The `reserved` set holds every spelling of a reserved keyword (marked in
  the index or in `abl_reserved_keywords.txt`). Like `function`, it owns no
  spellings: the grammar uses it only to tell a table name from a keyword.

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
- ABL lets an unreserved keyword name a table (`ITEM`, `MENU`, `RULE`). The
  `record-names` rule scopes the word after a record word as a name when it
  is not reserved and not a system handle:
  - always after `FIND` (and `FIRST`, `LAST`, `NEXT`, `PREV`, `CURRENT`),
    `EACH`, `FIRST`, `LAST`, `BUFFER`, `BUFFER-COPY`, `BUFFER-COMPARE`,
    `LIKE`, `AVAILABLE`, `AMBIGUOUS`, `LOCKED`, `CAN-FIND(`, `ROWID(` and
    `RECID(`. `FIND PREV` at the end of a line keeps `PREV` a keyword: it is
    the one unreserved `FIND` option;
  - after `FOR`, a list of names, but not `TEMP-TABLE` and not a word before
    `OF` (`FOR LOAD OF t`);
  - after `OF`, only before a reserved word, a lock or join option,
    punctuation, a comment or the end of the line. So `OF menu NO-LOCK` is a
    table and `OF MENU-ITEM m-exit` a widget;
  - after `CREATE`, `DELETE`, `RELEASE` and `VALIDATE`, only before `.`,
    `NO-ERROR`, `USING`, `VALIDATE` or `FOR TENANT`, and never
    `WIDGET-POOL`. So `DELETE OBJECT h` and `DELETE PROCEDURE THIS-PROCEDURE`
    keep their keywords.
  The keywords before the name keep their scopes: the rule hands them to
  `#keywords`.
- The `record-names` rule starts with `\b(?<![#$%&.-])`, not
  `(?<![\w#$%&.-])`. Before a letter the two match the same, but Oniguruma
  tries a pattern that starts with a look-behind at every position: the rule
  cost about 20% more time that way, and 3% with `\b`.

### Known limits

- A definition whose name is on the next line gets plain keyword and
  identifier scopes. So does a definition with a comment between its
  keywords (`DEFINE /* x */ VARIABLE n`): a regex cannot skip a comment that
  can nest.
- An unreserved keyword used as a name outside a record position (a
  variable called `count`, a field without its table such as `WHERE date =`)
  shows as a keyword, and so does a label with a keyword's name (`total:`).
  So does a table name on the line after its record word, and a table after
  `TO` in `BUFFER-COPY a TO item`. LSP semantic tokens are the planned
  correction: the parser and the semantic model know what each name is.
- The grammar scopes three integer literals joined by `/` with no spaces
  (`10/2/3`) as a date, the same as `12/31/2026`.

### Tests

From `clients/vscode`:

- `pnpm run test:grammar` — assertion tests (`test/grammar/cases`) and
  snapshot tests (`test/grammar/snapshots`), with `vscode-tmgrammar-test`.
  `snapshots/grammar-tour.p` is a tour of the grammar in one program that
  compiles: nested comments, comment markers in strings, escapes, numbers,
  preprocessor names and blocks nested six deep. Open it to see the
  highlighting.
- `pnpm test` — includes `test/grammar/corpus.test.ts`: every regex
  compiles, and every ABL file in the repository tokenizes within a time
  budget, on vscode-textmate 9 and vscode-oniguruma 2, the engine VS Code
  uses. It also includes `test/grammar/fuzz.test.ts`: pathological shapes,
  random token runs and mutated corpus files must stay inside a time budget
  per line and per file and a rule-stack depth. The time budgets grow on a
  slow machine: a calibration scan that does not use the grammar sets the
  factor. `OXABL_FUZZ_SECONDS` makes a longer run, `OXABL_FUZZ_SEED` replays
  one.
- `xvfb-run -a pnpm run test:e2e` — the packaged VSIX in a downloaded VS Code:
  activation, the colours that the editor renders, Markdown code blocks, a
  missing binary (a line in the output channel, no message), the self-update
  against a fake GitHub API, and screenshots. It removes every `oxabl` from
  `PATH` and sets `VSCODE_CLI=1` (otherwise VS Code reads the login shell's
  `PATH` and starts a real language server), and fails when a scenario leaves
  a process running.

## Releases

- Release Please releases the extension as the component `oxabl_vscode`
  (package `clients/vscode`, `release-type: node`). Its tags are
  `oxabl_vscode-vX.Y.Z`. The component is not `oxabl-vscode`, because a tag
  `oxabl-vscode-v1.0.0` starts with `oxabl-v`, and a tool that looks for the
  `oxabl` crate's tags would take it.
- A commit that changes `clients/vscode` (including the generated grammar and
  schema) gives a new extension version. Its changelog therefore also lists a
  crate commit that only regenerated the schema. The first release had no
  earlier tag, so its changelog lists every such commit since the start.
- A release of the extension alone also runs the `build` and `publish` jobs of
  the release workflow. `publish` skips crate versions that exist, so this
  costs runner time only.
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

1. At most once a day, while VS Code runs, it lists the tags
   `GET /repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl_vscode-v`.
   (`/releases/latest` names the last crate released, not the extension.)
2. For the highest `X.Y.Z` above its own version it reads
   `/releases/tags/oxabl_vscode-vX.Y.Z`. A draft, a pre-release, or a release
   without both assets is no update.
3. It downloads the VSIX and the `.sha256`, verifies the digest, and installs
   the VSIX with `workbench.extensions.installExtension`. The new version
   runs after a window reload.

The check runs at activation and then every 6 hours. The 24-hour stamp in
`globalState` keeps it to one check a day. Each automatic check reads the
setting and the extension mode again, and does nothing when the mode is `off`
or the extension is not in production mode.

The newest tag may be installed already and wait for a reload. Two signals
decide this, and both must hold:

- After an install, the extension stores
  `oxabl.updates.pendingReload = { version, dir }` in `globalState`. VS Code
  keeps that state per profile. `dir` is the extensions folder of this host
  (`dirname(extensionUri.fsPath)`). A local and a remote window of one
  profile share the record but have different folders, so a record written by
  the other host never counts here. Each host can replace or clear it, which
  costs at most a repeated install, never a lost update.
- The folder `oxabl-project.oxabl-X.Y.Z` of that version, or a higher one,
  exists in `dir` and is not in `.obsolete`. When `.obsolete` exists but
  cannot be read or parsed, no version counts as installed.

The check then reports `pending-reload`, and only if the record's version is
above the running version and at least the newest tag. In every other case a
newer tag is `available`. This matters for profiles: all profiles share one
extensions folder, so the folder alone is not enough. A profile without the
record installs the version that is already on disk, which only adds it to
that profile. The extension clears the record when the running version is not
older than it. For an install that the extension made, a release that is
pulled never makes the check install an older version over the newer one.
After a person installs an older version by hand in a profile that the
extension updated, that profile stays on it until a newer tag exists.

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
