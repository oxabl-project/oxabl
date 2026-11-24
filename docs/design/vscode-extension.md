# The VS Code extension: grammar, releases and self-update

The extension (`clients/vscode`) has five parts. A TextMate grammar
highlights ABL with no other extension and no `oxabl` binary. A language
client starts `oxabl lsp` for format-on-save, diagnostics and the document
outline when the binary
is there. A self-update keeps the extension current, because it is not on the
Marketplace. An installer puts the `oxabl` CLI on the machine. A build loop
runs `oxabl build` at open, on save and on changes on disk. This note records
how each part works and why.

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
- Two placeholders start a pattern that must not follow a word character:
  `{{boundary}}` before a word character and `{{non-boundary}}` before
  another character (`&`, `.`). See the rule about look-behinds below.
- A repository rule that includes itself can have `"x-nesting-limit": N`.
  Codegen then writes N copies of the rule (`name`, `name-2`, … `name-N`).
  Each copy includes the next where the rule included itself, and the last
  copy does not include itself. So the rule stack holds at most N of them.
  Codegen fails when the rule does not include itself, when N is less than 2,
  or when a copy's name is already a rule.

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
- No `match` or `begin` pattern starts with a look-behind. Oniguruma tries
  such a pattern at every position of every line. A rule that must not follow
  a word character starts with `{{boundary}}(?<![#$%&.-])`, not
  `(?<![\w#$%&.-])`. Before a word character the two accept the same text,
  and tokenizing is about twice as fast. `{{non-boundary}}` does the same
  before `&` or `.`. The placeholders are `\b` and `\B` plus the six
  characters `²`, `³`, `¹`, `¼`, `½` and `¾`: Oniguruma's `\b` takes them as
  word characters and its `[\w]` does not, so a plain `\b` would change the
  tokens after them (`test/grammar/cases/boundaries.p`). A test in
  `corpus.test.ts` fails when a pattern starts with a look-behind.
- Block comments and include references nest. Each opener pushes a rule, so
  without a limit, damaged text (`/*` or `{` many times) makes the rule stack
  deep and each line slower than the one before it. `comment-block` and
  `include` have `"x-nesting-limit": 50`. The `include` rule keeps its
  argument patterns in `include-arguments`, so its 50 copies stay small.
- A `&GLOBAL-DEFINE` or `&SCOPED-DEFINE` in the value of another one is a
  `match`, not a second `begin`. Otherwise each continued line
  (`&SCOPED-DEFINE a ~`) pushes one more define rule.
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
- The grammar counts at most 50 nested block comments and 50 nested include
  references. Past that depth, `/*` is comment text and `{` is argument
  text, and each `*/` or `}` closes one of the 50 levels. So in a comment
  nested deeper than 50 levels, the highlighting ends the comment too early.
  The compiler has no such limit. Real files nest fewer than 10 levels.

### Tests

From `clients/vscode`:

- `pnpm run test:grammar` — assertion tests (`test/grammar/cases`) and
  snapshot tests (`test/grammar/snapshots`), with `vscode-tmgrammar-test`.
  `snapshots/grammar-tour.p` is a tour of the grammar in one program that
  compiles: nested comments, comment markers in strings, escapes, numbers,
  preprocessor names and blocks nested six deep. Open it to see the
  highlighting.
- `pnpm test` — includes `test/grammar/nesting.test.ts`: nested comments
  and include references keep their scopes up to the limit of 50 levels, and
  the rule stack stops at it. It also includes `test/grammar/corpus.test.ts`: every regex
  compiles, and every ABL file in the repository tokenizes within a time
  budget, on vscode-textmate 9 and vscode-oniguruma 2, the engine VS Code
  uses, and no `match` or `begin` regex starts with a look-behind. It also includes `test/grammar/fuzz.test.ts`: pathological shapes,
  random token runs and mutated corpus files must stay inside a time budget
  per line and per file and a rule-stack depth of 64. Every shape runs at
  sizes up to 10,000 (`OXABL_FUZZ_SCALE`), the deep shapes (`/*`, `{` and
  `&SCOPED-DEFINE a ~` many times) too. The time budgets grow on a
  slow machine: a calibration scan that does not use the grammar sets the
  factor. `OXABL_FUZZ_SECONDS` makes a longer run, `OXABL_FUZZ_SEED` replays
  one.
- `node test/grammar/compare.mjs <old.json> <new.json> [dir ...]` — tokenizes
  the repository's ABL files (or the given files and directories) with two
  grammars, counts the lines whose tokens differ and times both. Use it for a
  change that must keep every token. `--counts-only` prints no file name and no
  source text.
- `xvfb-run -a pnpm run test:e2e` — the packaged VSIX in a downloaded VS Code:
  activation, the colours that the editor renders, Markdown code blocks, a
  missing binary (a line in the output channel, no message), the self-update
  against a fake GitHub API, the install of the CLI and the build loop against
  a fake `oxabl` (see [The build loop](#the-build-loop)), the outline (Outline
  view, breadcrumbs and "Go to Symbol in File") and the ABL Schema view from a
  real `oxabl` binary, and screenshots. The `outline` scenario needs `OXABL_E2E_OXABL` and
  `OXABL_E2E_RUNTIME` and fails without them. It removes every `oxabl` from
  `PATH` and sets `VSCODE_CLI=1` (otherwise VS Code reads the login shell's
  `PATH` and starts a real language server), and fails when a scenario leaves
  a process running.

## The schema view

The **ABL Schema** view in the Explorer shows the `.df` files of
`[workspace.schema]` from the language server's `oxabl/schema` request, with a
filter, "Open in .df", copy commands and a details page per table. It asks the
server only after it is visible for the first time, so activation does not
read the schema. See [schema-browser.md](schema-browser.md).

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

After the extension's own release, the same check asks a managed binary
(`updates.ts` calls `CliManager.checkForUpdate`, in `binary.ts`). It runs
whatever became of the extension's release, and its failure is reported on
its own. A binary that is not managed, that has no `self-update`, or (for an
automatic check) that runs with `OXABL_NO_SELF_UPDATE` is not asked. The
binary answers `oxabl self-update --check --json` (`{installed, latest,
update, managed}`; any other shape is an error, and nothing is installed).
`auto` runs `oxabl self-update`, shows a status bar message and calls
`onChanged`, which restarts the language server and reconfigures the builds;
other modes ask with Update, Release Notes (the `oxabl-vX.Y.Z` release) and
Skip This Version (`oxabl.updates.cliSkipped`). The binary keeps its own daily
background check for use outside VS Code; both take the binary's update lock,
so they never install at once.

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
it. Re-run the failed job in the same workflow run. If the job did not run at
all (Release Please failed after it created the release), run the workflow by
hand with the input `tag` set to the release tag (`oxabl_vscode-vX.Y.Z`): the
job then builds that tag and uploads. A manual run without `tag` uploads
nothing, and a new push does not create the release again.

## The CLI and the build loop

The modules follow one rule: a module with logic has no `vscode` import, and a
thin layer holds the editor calls. Pure: `host.ts` (which hosts get a binary),
`server.ts` (where the binary is), `net.ts` (HTTP to GitHub, shared with the
self-update), `install.ts` (download, verify, install), `cli.ts` (calls to the
binary), `scheduler.ts` (coalescing), `progress.ts` (events and status text),
`roots.ts` (which root owns a path). Editor layer: `binary.ts` (prompts and
progress of the install), `builds.ts` (processes, watchers, status bar,
diagnostics) and `extension.ts`.

### Where it runs

`extensionKind` is `["workspace"]`, so in a WSL, SSH or container window the
extension runs on the remote host, where the files and the binary are.
`host.ts` classifies the host from `process.platform`, `process.arch` and
`vscode.env.remoteName`: Linux x86_64 (local or remote) is supported; a local
Windows window gets the message "oxabl does not run on Windows. Open this file
in a window connected to WSL for ABL language features." with the offer to
reopen the folder in WSL (the WSL extension's `remote-wsl.reopenInWSL`, else a
link to the README); any other platform or CPU gets a message to run
`cargo install oxabl`. These messages show with or without an `oxabl.toml`,
like the install prompt. The extension never
downloads a Windows binary: the language server needs Unix sockets.

`capabilities.untrustedWorkspaces` is `limited`. A build runs the worker
command of `oxabl.toml`, which is code from the folder. So the build loop
starts only in a trusted workspace (and when trust is granted,
`onDidGrantWorkspaceTrust`), and not even `--print-config` runs before that.
`oxabl.server.path` and `oxabl.build.args` are restricted settings: a folder
cannot name the program or its arguments before it is trusted.

### Finding and installing the binary

The order is `oxabl.server.path`, `${OXABL_INSTALL_DIR:-~/.local/bin}/oxabl`,
then `PATH`. The install path comes before `PATH` because it is where the
installer and `oxabl self-update` write.

With no binary, one prompt offers the install when the extension starts, that
is when an ABL file opens or the workspace has an `oxabl.toml` (`Don't Ask
Again` is kept in `globalState`). The prompt shows without an `oxabl.toml` too,
because the outline needs no setup. Everything that needs a project stays
silent without one: the language server turns diagnostics and formatting off,
and the build loop finds no root, so it runs nothing and shows nothing. The install follows the
contract that `install.sh` and `oxabl self-update` share: newest plain
`oxabl-vX.Y.Z` tag (`/git/matching-refs/tags/oxabl-v`; the extension's own tags
start with `oxabl_vscode-v`, so they never match), a release that is not a
draft or pre-release and has both assets, then download into a staged file in
the target directory, SHA-256, `chmod`, `--version` must say the expected
version, and one `rename`. The install record
`${XDG_DATA_HOME:-~/.local/share}/oxabl/install.json` is written the same way,
with `installer: "vscode"`. Only a binary named by the record (compared after
`realpath`) is *managed* and updated; the extension never touches another.
The HTTP code (redirects, size limits, loopback-only `http:` for the test
hook `OXABL_UPDATE_API_URL`) is the code of the extension's own update. A
timeout counts idle time, so a slow download of the binary does not fail.

Before each build the extension runs `oxabl --version` and compares it with
the version the language client started with. `oxabl self-update` replaces the
file under a running server, which keeps its old inode; the extension
restarts the client when the version differs. The new `oxabl lsp` then finds
the `oxabl daemon` of the old binary still serving the root: the registration
names the version of the binary, so it stops that older daemon (`SIGTERM`,
`SIGKILL` after one second) and starts one of its own build, which reads
`oxabl.toml` with the new rules. The other clients of the stopped daemon (a
second window, the desktop app) lose their connection and start again. A newer
daemon is not stopped: `oxabl lsp` shows a warning that names both versions
and the `kill` command that stops it, then exits. A replaced daemon is written
to the output channel (`window/logMessage`). A daemon also exits 10 minutes
after its last client (`OXABL_DAEMON_IDLE_TIMEOUT`, see `docs/overview.md`), so
a closed window does not keep it. The extension also checks once per binary
version that `oxabl build --help` mentions `--yield`. A binary without it is
updated when it is managed, and otherwise one message asks for a newer oxabl.

### The build loop

A *root* is a folder with an `oxabl.toml` in the workspace. It builds when
`oxabl build --root R --print-config --json` exits 0, has a value for
`build.workers.command`, and has a place for the r-code: a value for
`build.output` (`build.rcode` absent or `directory`), or `build.rcode` =
`beside-source` (`oxabl.build.args` go to this call too: an overlay can supply
the worker command). Another value of `build.rcode` does not build: the
extension does not know where that r-code goes. The same output gives the
directories whose changes are ignored: `build.output` (in `directory` mode
only), `build.outputs.*`, `build.state` and `build.store`. With
`beside-source`, the build writes r-code into the source directories, so no
source directory is on that list. Any other change under `.git` is ignored too.

When the call fails (for example, a binary older than a key of `oxabl.toml`
refuses the file with exit code 2), the root does not build. The extension
writes the reason to both output channels and shows it once as an error
notification, `oxabl build: cannot read the build setup: <reason>`.

The order of the updates for `build.rcode`: the extension first, then the
binary, then the `oxabl.toml` files. An older extension reads `build.output`
only, so it does not build a root with `beside-source`: saves start no build
and nothing tells why. An older binary refuses `build.rcode` (unknown keys are
errors), which the newer extension shows as above. Both cases build nothing,
so no build writes r-code to a place that the project does not expect.

Triggers are the build at open, saves, and a file watcher on
`**/*.{p,w,cls,i,v,df}`, `**/oxabl.toml` and `**/.oxabl/cache.toml`. A `.r`
file is never a trigger, so r-code that a build writes beside the sources
starts no build (the e2e scenario `build-beside-source`). The language server
registers no glob that matches a `.r` file either, and an `.r` event that a
client sends anyway recomputes no open buffer (`crates/oxabl_lsp/tests/watcher_e2e.rs`). The
watcher is there because agents, `git` and other tools edit files without a
save. A save is also seen by the watcher. The extension keeps the modification
time and size of each file at its last event, so the second event of one save
is dropped: without that, the watcher event of a save arrives while the build
of that save runs and cancels it.

#### Coalescing

`scheduler.ts` follows what rust-analyzer's flycheck and watch-mode compilers
do. A request starts a trailing debounce (`oxabl.build.debounce`, default
500 ms) with a maximum wait of 5 s from the first request, so a constant
stream of writes cannot starve the build. At most one build runs per root, and
at most one request is pending. A request while a build runs sends that build
one `SIGINT`; when it has exited and the debounce has settled, the one pending
build starts. Ten saves in a row, or an agent that writes 200 files, give at
most one cancelled build and one new build. The newest change always wins,
because the pending build starts after the last change.

Events from the watcher get 600 ms more. VS Code gives file events to an
extension in batches about half a second apart; the e2e scenario with 200
files written on disk showed three batches between 0.5 s and 1 s apart, which
a debounce of 500 ms or less splits into several builds.

`SIGINT` goes to the `oxabl` process, never to workers: `oxabl build` lets busy
workers finish their unit and stops them (`docs/build-wrapper.md`).

#### Yield

The extension runs `oxabl build --events --yield --wait-lock 3600`. `--yield`
marks the lock so that any newer `oxabl build` (a terminal, an agent, another
VS Code window) may stop this build with one `SIGINT` instead of waiting for
it or failing with exit code 3. The editor's build is always the one to give
way: it is a background job and the person or the agent that started another
build wants that result. The build never stops a build that did not ask for
it. The preempted build exits 130, and nothing tells it from a cancel, so the
extension knows whether it sent the signal. An exit 130 that it did not cause
means another process took over: the status bar says so and one more build is
scheduled. That build waits for the lock (`--wait-lock`) and finds little to
do. When two windows keep stopping each other, the pause before the next
attempt doubles from 1 s to 30 s.

#### What it shows

Two status bar items. VS Code renders an item again when its text changes,
which starts the rotation of a `~spin` icon again; the progress text changes
with every unit, so the spinner is an item of its own whose text never changes,
shown while a build is preparing, running or waiting elsewhere (the e2e
scenario `build-open` checks that it stays one element with one animation).
The other item has the text (`preparing`, then `done/total · N% cached`, then a check
mark for 3 s, an error count that stays until the next success, a warning
with the first line of stderr, or "running elsewhere"). A unit counts once,
also when the retry pass compiles it again. The percent is the share of the
workspace that needs no compile: the units up to date (`units - rebuild` of
`build-started`) plus the units a cache gave, over `units`, rounded down so
that 100% means nothing compiles. It is not the hit rate of this build: after
a save the build rebuilds only the edited units, which no cache can hold, so
that rate is 0% at almost every build. The tooltip holds only what
`build-started` fixes (the units to rebuild, the workspace and the units up to
date before the build), because VS Code redraws an open hover at every change
of it, and a count per unit made it flash without a pause. When no unit was up
to date before the build (`rebuild` equals `units`) and the workspace has 500
units or more, the tooltip also says that this is the first build of the project
and that it can take 10 minutes or more. The signal is in the events of oxabl, so
it needs no guess about files. A build that rebuilds everything for another
reason, such as a change of the environment, gets the same text. The notification
says "N of M units rebuilt" with the hit rate of the build.

Each build passes `--report <global storage>/reports/<hash of root>.json`
(or uses the `--report` of `oxabl.build.args`). After exit 0 or 1 the
extension counts the units without a compile key: a plan reason `unkeyable` or
`unverified`, or a report note "no key from what the compiler read". It warns
once per workspace, root and reason (kept in `workspaceState`, so VS Code does
not repeat it at every start) when the reason names a missing setting (`needs [`)
or a tenth of the workspace has no key: such a project compiles that share at
every save. Encrypted source has no key by nature, so a few such units only
show in the tooltip and the log. Compiler messages go to a diagnostic
collection `oxabl build`: each unit's set is replaced at its `unit-finished`
and cleared at its `unit-fetched` (a cache gives no messages), and a cancelled
build keeps the sets of the units it did not reach. One notification shows when
the first build of the window ends; later builds show the status bar only. A
success is told once per root (`workspaceState`), because "N units up to date"
at every start tells nothing; a failure is told in every window. On
deactivate every running build gets `SIGINT`, and `SIGKILL` after 5 s.

### Tests

The scenarios in `test/e2e/scenarios/build.mjs` run the real extension against
a *fake* `oxabl` (`lib/fake-oxabl.mjs`): a script that answers `--version`,
`build --help`, `--print-config` and a minimal `lsp`, plays a scripted event
stream for `build --events`, and logs each call and each signal it gets. They
check what a person sees (status bar, notifications, the Problems view) and
what the binary was asked to do. The install scenarios use the fake GitHub
API with the fake binary as the release asset. Every VS Code of the suite
gets an install directory and a data directory inside its profile, so no
scenario meets the `oxabl` of the machine.
