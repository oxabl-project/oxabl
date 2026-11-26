# The workspace pass after an edit

The daemon keeps one workspace pass for each workspace root: the reverse
dependency graph (`ReverseGraph`), the dynamic calls of the workspace and the
rows of the symbol search. Find references, the call hierarchy, the reference
lenses, `oxabl/impact` and `workspace/symbol` read it. A full pass analyses
each file of the workspace one time. On the generated `erp` workspace (10,386
roots) it costs about 230 CPU seconds.

This document tells how the daemon keeps that cost away from an edit.

## When a pass runs

A request runs a pass when the stored pass is not current
(`ensure_workspace` in `crates/oxabl_daemon/src/methods.rs`):

- The configuration or the schema of the session changed. The pass is full.
- An open buffer changed. The pass runs only when the buffers now give other
  text for a file that the pass read or looked for (below). Else the stored
  pass stays and the daemon marks it current.
- A file changed on disk (the stamps of the freshness check). The pass
  analyses only the files that changed and the files that read them.
- `oxabl/reindex`. The pass is full.

A pass that completes after a buffer moved is not dropped when the pass read
every file as the buffers give it now.

## The parts of a pass

Each file gives one part to a pass (`crates/oxabl_daemon/src/pass.rs`):

- its rows of the graph (`oxabl_pipeline::FilePart`): the subjects it depends
  on, with the kind and the span of each edge, the references it could not
  resolve, why its edge set is incomplete, and its dynamic calls;
- its rows of the symbol search;
- the `SHARED` names it produces;
- its inputs: the paths that its analysis and its `SHARED` scan read, the paths
  they looked for and did not find, and the directories they listed.

The pass reads through an `ObservingFileSystem`, which records the inputs of
each analysis. That includes the include files at any depth and the class and
program files whose facts the analysis took from the index, also when the
index read them for another file. A layer under it keeps, for each path read,
the hash (BLAKE3) of the text and the disk stamp before the read.

The daemon keeps the parts of the last pass. `ReverseGraph::from_parts` joins
them into the graph that `ReverseGraph::build_with` gives for the same text.

## The next pass

1. Walk the workspace for its files.
2. For each input of a kept part, find out whether it changed:
   - a path that a buffer holds: compare the hash of the buffer;
   - a path on disk with the same stamp: not changed;
   - a path on disk with a new stamp: read it and compare the hash. A save of
     the text that the pass read changes no part;
   - a path that the pass looked for and did not find: changed when it is
     there now;
   - a directory that the pass listed: changed when its stamp changed;
   - a file that could not be read: always changed.
3. Analyse again each new file and each file with a changed input. Drop the
   parts of removed files.
4. Scan the files of step 3 for the `SHARED` names they produce. When the map
   of `SHARED` producers is not the same, run a full pass: each file can read
   that map, and no part records which names it asked about.
5. Join the parts. When no file was analysed and the file list is the same,
   the daemon keeps the graph and the symbol rows that it has. A reference lens
   that holds the pass (`references::Pass`) then stays current.

## Fail closed

A pass is full when:

- there is no kept pass, or its session generations of the configuration or
  the schema are not the current ones;
- the configuration that the pass resolves from disk is not the same as the
  one of the kept pass (a hash of its `Debug` text), also when no client said
  that `oxabl.toml` changed;
- the map of `SHARED` producers changed.

Two writes during one pass can give two analyses different text of one file.
The part keeps the first hash, so the next pass analyses again each file that
read the file.

## Cost

An edit of one file analyses the file and the files that read it. A file that
`RUN`s a program reads the program (the index reads its parameters), so an
edit of a program also analyses its callers. An edit of an include analyses
each unit that includes it. An edit of a class analyses the files that read
the class, also through inheritance.

On the `erp` preset (`docs/performance.md`), an edit of a program that two
files run analyses 3 files, and the next find references takes 1.4 s and 3.4
CPU seconds, against 21 s and 229 CPU seconds with a full pass. An edit of a
program that 570 files read analyses 571 files (3.9 s, 34 CPU seconds). The
kept parts take about 60 MB.

## Tests

`crates/oxabl/tests/incremental_pass_lsp.rs` runs the real `oxabl lsp`. A test
build of the daemon writes one line to stderr for each pass when
`OXABL_TEST_PASS_COUNTS` is set: `oxabl-test: workspace pass analysed <n> of
<m> files, kept <bytes> bytes`. The tests check:

- an open of a file with its disk text analyses no file;
- an edit of a procedure file, a class and an include analyses at most four of
  47 files, and the references, the incoming calls and the lens titles are the
  answers of a new daemon on the same text;
- an edit of a `SHARED` producer gives the answers of a new daemon;
- a save of the edited text, and a rewrite of a file with the same text,
  analyse no file;
- a file changed on disk, and an include that could not be read and then
  reads, give the answers of a new daemon;
- a pass after a change of `oxabl.toml` that no client reported is full.
