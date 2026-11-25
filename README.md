# oxabl - Oxidized ABL Tooling

oxabl is high-performance tooling for ABL. Build, check, format and edit with no OpenEdge runtime needed, except to compile.

> oxabl is still in early phases. Everyone is welcome to try it, but not all features are stable, and bugs are going to occur.

## Install

> Currently, oxabl only targets Linux x86_64. On Windows it runs through WSL only, not natively. It has not been tested on Mac or any ARM hardware.

On Linux x86_64, this installs the release binary in `~/.local/bin`. It updates itself later (oxabl self-update):

```bash
curl -fsSL https://raw.githubusercontent.com/oxabl-project/oxabl/master/scripts/install.sh | sh
```


Or to build from source (this build doesn't update itself or use the remote cache unless you add `--features remote-cache,self-update`):


```bash
cargo install oxabl
oxabl check path/to/src     # lint and report formatting drift
oxabl format path/to/src    # format in place
```

### VSCode (VSIX) Extension

Download the latest VSIX file from the [releases page](https://github.com/oxabl-project/oxabl/releases) and install in VSCode via *Extensions -> Three dot menu -> Install from VSIX -> Select the file*. You will be prompted to install the latest version of the CLI as well, if you don't already have it.

On Windows, install the VSIX while your WSL window is open, as the extension runs inside WSL.

### Try it

[Visit the homepage to try oxabl in your browser (experimental)](https://oxabl.org/#playground). Analysis and formatting run on a single file in your browser; include files and schema-based checks need the CLI or language server.

### Agentic Setup

Paste this into your coding agent from the root of your ABL project to have AI set up your project with oxabl:

```text
Set up oxabl for this ABL project. Read
https://github.com/oxabl-project/oxabl/blob/master/docs/overview.md first, then write an oxabl.toml at the project root:
- include_paths: our PROPATH, in order (check our .pf/.ini files, build scripts or startup parameters)
- workspace.schema: our .df schema dumps
- workspace.preprocessor: our OpenEdge version, OS and database names
- build.workers: a command that starts a batch _progres session (in our container if we use one), using our existing .pf for database connections

Never put passwords in oxabl.toml. Check your work with
`oxabl build --print-config`, `oxabl check .` and `oxabl build --plan`, and fix any errors before you finish. Ask me if anything is unclear.
```

### Coding agents

Oxabl ships a skill that teaches a coding agent (Claude Code, Codex, pi, OpenCode, Copilot and others) to use its commands without exploring. It is built into the binary and matches its version:

```sh
oxabl skill install            # one copy in ~/.local/share/oxabl/skills/oxabl, linked from ~/.agents/skills/oxabl and ~/.claude/skills/oxabl
oxabl skill install --check    # exit 1 when the store or a link is missing, foreign or out of date
```

The install is for your user, so every project sees the skill. `oxabl skill show` prints the skill. After `oxabl self-update`, the store updates with the binary. A link that points elsewhere, or a real directory, is never overwritten: `--force` moves it out of the skills folders to `~/.local/share/oxabl/skill-backups/` (agents would load a backup there as a second skill) and links. The docs site also serves [llms.txt](https://docs.oxabl.org/llms.txt), a Markdown copy of every page, and a read-only MCP server at `https://docs.oxabl.org/mcp`. The VS Code command **oxabl: Install agent skill** does the same.

## Features

### Build

oxabl is a full build tool. It parses your ABL code base, generates a dependency graph, builds the code, and caches the results.

oxabl keeps several OpenEdge sessions alive in a pool of workers, started by a command you set in `oxabl.toml`, to build ABL files. The workers compile in parallel, allowing faster compiling.

Thanks to the graph, files are always compiled in the appropriate order.

### Cache

As mentioned, oxabl uses a build cache. The cache keys each compile unit with its dependencies, environment (including the OpenEdge version), propath, compile options, and content. This makes the cache safe to use between environments and different versions of the same codebase.

The cache can also be hosted remotely on an oxabl-cache server, allowing your entire team and CI to pull from an already-compiled cache, speeding up your compile times dramatically.

### Parser

oxabl has its own ABL parser. Essentially a compiler-frontend for ABL, oxabl's lexer tokenises the source, its preprocessor expands include files and preprocessor directives, and the parser then generates an AST.

The parser is written in Rust, allowing us to analyse ABL code as fast as possible.

This speed and correctness allows for all of the features oxabl supports.

### Check / Lint

oxabl is shipped with a small set of essential lint rules baked in. We have several categories of lint rules:

- Core (default, always on)
    - undefined-symbol
    - unused-variable
    - assigned-but-never-read
    - unknown-table-or-field
    - type-mismatch-assignment
    - block-var-used-outside
- Transactions (strict.transactions, TX)
    - implicit-transaction
    - procedure-wide-transaction
    - transaction-spans-ui
    - transaction-spans-call
    - scope-widened
    - missing-no-undo
- Locks (strict.locks, LK)
    - default-lock
    - lock-outlives-transaction
    - lock-upgrade-in-loop
- Database access (strict.access, DB)
    - find-in-loop
- Handles and memory (strict.handles, HD)
    - handle-not-deleted
    - delete-not-in-finally
    - persistent-procedure-leak
    - memptr-not-freed
    - missing-widget-pool
- Unknown values (strict.nulls, NL)
    - field-after-failed-find
    - unknown-in-comparison
    - unknown-propagation
    - nullable-db-field
- Calls and shared state (strict.calls, CL)
    - signature-mismatch
    - unresolved-call
    - run-value and dynamic-invoke
    - shared-state

The strict groups are off by default and in preview. `enable = ["strict"]` in `[workspace.lint]` turns on all six strict groups, or you can toggle each group on its own.

### Format

oxabl's initial formatter is safe by design and doesn't re-write code. It only corrects indentation and collapses runs of blank lines, plus keyword case and expanding or abbreviating keywords when a style guide turns them on. It supports a non-destructive default and two style guides, `oestandards` and `consultingwerk`.

The formatter is non-destructive, never rewrites or changes code, and will refuse to change the file if the formatted code differs from the original in anything other than layout and keyword spelling.

Future versions will support line wrapping, sorting USINGs, and rewriting comment text.

### Flexible Clients

oxabl's core is available in several clients, including a CLI, LSP, and WebAssembly. Each client must create the same results before release for each supported feature, to ensure they are consistent, and they all share the same engine to accomplish this. The clients are purposefully thin, leaving most of the work to the shared oxabl engine.

### Language Server (LSP) and VSCode Extension (VSIX)

The language server provides many language features with no OpenEdge runtime requirement, meaning it's leaner, faster, and runs on developer PCs without an OpenEdge install. The same checks run in CI through `oxabl check`.

LSP features:

- Diagnostics while you type
- Format document
- Document symbols (outline and breadcrumbs)
- Workspace symbols
- Go to definition
- Find references and call hierarchy
- Go to implementation, go to super method and type hierarchy (classes and interfaces, across files that are not open; an answer that can be short says so)
- Signature help and parameter-name inlay hints
- Clickable links on {includes}, RUN targets, and USING
- Database-effect code lenses (only with strict database rules enabled)
- List all files that include a specific include file, and pick one as the include's context
- Expand a file to resolve its pre-processing directives and include files.

VSCode Extension Only:

- Syntax highlighting (TextMate grammar)
- Language setup (file type, comment toggling, brackets, folding markers)
- Build integration (runs oxabl build on open, save and file changes, relies on the CLI, not the LSP)
- Automatic updates of the extension from GitHub Releases
- Offers to install the CLI, then keeps it updated

In VSCode, you will receive in-line diagnostics for enabled lint rule groups, compile errors after a build, and any syntax errors you make while typing. Syntax can be checked on key-stroke as we don't require the OpenEdge runtime to check for correctness.

Other editors, such as Zed, Helix, or Neovim, can make use of the LSP as well, but don't have extensions yet. Cursor and other VSCode forks can install the VSIX.

## Example `oxabl.toml`

```toml
# oxabl.toml — put this at the root of your ABL code.
# Every section is optional. Paths are relative to this file.

[workspace]
name = "my-app"

[workspace.features]
# Switch off what you don't use yet. Both default to true.
diagnostics = true
format = true

[workspace.sources]
# Your PROPATH, in order. "$DLC/..." entries resolve under `dlc` below;
# "$LIB:<name>" names a procedure library from [build.libraries].
include_paths = [".", "src", "$DLC/tty", "$DLC/src", "$DLC"]
# Which files are compile roots (default: p, w, cls, v).
root_extensions = ["p", "w", "cls"]
# Source encoding (default "utf-8"). For mixed trees, keep utf-8 and set a fallback.
encoding = "utf-8"
fallback_encoding = "windows-1252"
# Where the OpenEdge include sources live on this machine
# (or set OXABL_DLC_SOURCES instead).
dlc = "/usr/dlc"

[workspace.preprocessor]
# What {&OPSYS}, {&WINDOW-SYSTEM}, {&PROVERSION} etc. expand to.
opsys = "UNIX"                # or "WIN32"
window-system = "TTY"         # or "MS-WINDOWS"
batch-mode = true
process-architecture = 64
proversion = "12.8"
# Logical database names, in connection order.
databases = ["sports2000"]

[workspace.schema]
# .df files (or folders of them) for table and field checks.
files = ["schema"]

[workspace.lint]
# Core rules are always on. "strict" turns on every strict.* group,
# or name groups one at a time: strict.transactions, strict.locks,
# strict.access, strict.handles, strict.nulls, strict.calls.
enable = ["strict.locks", "strict.nulls"]
unused-variable = "info"      # change one rule's severity, or "off"

[[workspace.lint.overrides]]
files = ["legacy/**"]
disable = ["strict"]

[workspace.style]
# Formatter settings. Leave out for the safe default (layout only).
keyword_case = "Uppercase"    # Uppercase, Lowercase or Preserve
indent_size = 4

# --- Build (needs OpenEdge) -------------------------------------------------

[build]
# Put r-code beside each source, or set `output = "build/rcode"` instead.
rcode = "beside-source"
exclude = ["**/templates"]

[build.libraries]
# Procedure libraries referenced as $LIB:<name> in include_paths.
# "OpenEdge.Net" = "/usr/dlc/tty/netlib/OpenEdge.Net.pl"

[build.workers]
# The command that starts one OpenEdge worker. oxabl fills in {root} and {program}.
command = ["_progres", "-b", "-q", "-pf", "build.pf", "-p", "{program}"]
# count = 4                   # default: one per CPU, at most 8
unit_timeout = "10m"
```

If OpenEdge is installed inside a container instead of directly on the host, you can use a build block like this:

```toml
[build.workers]
# Each worker is a batch OpenEdge session inside the running container
# "openedge-dev". -i keeps stdin open (oxabl talks to workers over it);
# -w {root} makes the session start in the workspace, as oxabl requires.
command = [
  "docker", "exec", "-i", "-w", "{root}", "openedge-dev",
  "_progres", "-b", "-q", "-pf", "/app/build.pf", "-p", "{program}",
]
# Where this workspace is mounted inside the container.
# Leave this out if it's mounted at the same path as on the host.
root = "/app"
unit_timeout = "10m"
```

## References

- [Full documentation](https://github.com/oxabl-project/oxabl/blob/master/docs/overview.md)
- [OE Standards Style Guide](https://github.com/alextrs/oestandards)
- [Consultingwerk's ABL Coding Standards](https://consultingwerk.atlassian.net/wiki/spaces/SCL/pages/2910257155/ABL+Coding+Standards+-+single+page)

## License

[MIT](https://github.com/oxabl-project/oxabl/blob/master/LICENSE)
