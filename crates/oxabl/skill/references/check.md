# oxabl check and oxabl format

Read this when you parse `oxabl check --json`, adopt oxabl on legacy code with
a baseline, check include files, or pick a format style.

## check

```sh
oxabl check src/orders/post.p --json
oxabl check src --no-format
oxabl check src --no-preprocess
oxabl check src/orders/post.p -I src/inc --schema db/schema.df
oxabl check src --include-findings --json
```

- One path: a file or a directory. A directory walks the root extensions
  (default `p`, `w`, `cls`, `v`; `[workspace.sources].root_extensions`) and
  its `.i` files for context-free strict rules only.
- Preprocessing is on by default. `--no-preprocess` skips includes and `&IF`
  and floods `LINT0001`; use it only when include paths are not set up.
- `--no-lint` hides lint findings; parse and semantic errors still fail the
  gate. `--no-format` skips the drift channel.
- `--include-findings`: also report findings inside includes, once per
  including program, at the include's own path and line. They count toward
  the exit code. Not combinable with `--baseline`.
- Exit: `0` clean; `1` any finding, drift or failed file; `2` usage or
  `oxabl.toml` error, a missing baseline, or nothing to check (both
  `[workspace.features]` off, or no ABL files found).

### JSON (`version` 3)

```text
{ "version": 3, "files_checked": 1, "lint_enabled": true,
  "diagnostics": [ { "path": "src/update.p", "code": "LK001", "severity": "warning",
      "source": "lint", "message": "the `FOR` phrase reads `Customer` with no lock phrase, so it takes SHARE-LOCK, the default",
      "help": "The record is changed later in its scope: add `EXCLUSIVE-LOCK`, the lock the change takes.",
      "span": { "start": 63, "end": 71 },
      "start": { "byte": 63, "line": 4, "column": 10 },
      "end": { "byte": 71, "line": 4, "column": 18 } } ],
  "format_enabled": true, "format": { "drifted": ["src/update.p"], "drifted_count": 1 },
  "preproc": [], "unjudged_symbols": 0, "failures": [] }
```

- `path` is as you passed it (relative to your current directory). Lines and
  columns are one-based. `help` is present only when the rule gives one.
- `severity`: `error`, `warning`, `info` or `hint`. All of them fail the gate.
- `failures`: files that could not be read or analysed (exit 1).
- `preproc`: include and preprocessor coverage warnings (no exit effect).
- Optional keys: `include_files_checked`, `include_diagnostics`
  (`--include-findings`), `baseline` (`--baseline`; then `diagnostics` holds
  only new findings), `fragment_roots`.

### Include files

- `oxabl check inc/defs.i` checks the include alone, as a fragment: names it
  does not define are treated as external and the unused-variable rules stay
  silent. stderr says so in a `note:`.
- A finding in an include from a context-free strict rule is reported in the
  include, not in each program that includes it.

### Baseline

```sh
oxabl check src --baseline oxabl-baseline.json --update-baseline
oxabl check src --baseline oxabl-baseline.json
```

- `--update-baseline` writes every current finding (it creates the file),
  drops fixed ones, keeps entries for files outside this run.
- A gated run reports only new findings and prints
  `baseline: N new, N baselined, N stale` on stderr. Stale entries do not fail.
- Matching uses rule code, workspace-relative path and a hash of the flagged
  line, so moving code does not unmatch it.
- A missing baseline file is exit 2. Update the baseline only when asked.

### Features switched off

`[workspace.features]` `diagnostics = false` or `format = false` turns a
channel off; `check` prints a `note:` on stderr. No flag turns it back on.

## format

```sh
oxabl format src/orders/post.p
oxabl format src/orders/post.p --check
oxabl format src/orders/post.p --stdout
oxabl format src/orders/post.p --style oestandards
```

- One path. A directory formats `.p/.w/.cls/.v` under it (not `.i`); name an
  `.i` file to format it.
- In place it prints nothing and exits 0. `--check` writes nothing, prints
  `<file>: would reformat` and exits 1 when a file would change.
- A file it cannot format faithfully (parse errors, or a change beyond
  layout) is left byte-for-byte unchanged with a message, and the exit code
  stays 0, also with `--check`.
- Style: `--style oestandards|consultingwerk|<file.toml>` beats
  `[workspace.style]` in `oxabl.toml`, which beats the safe default. Use the
  project's style; do not pass `--style` unless asked.
- It does not wrap long lines, rename, reorder or rewrite comments.

Docs: https://docs.oxabl.org/toolchain/commands/check/,
https://docs.oxabl.org/toolchain/commands/format/
