# The workspace pass after an edit

The daemon keeps one workspace pass for each workspace root: the reverse
dependency graph (`ReverseGraph`), the dynamic calls of the workspace and the
rows of the symbol search. Find references, the call hierarchy, the reference
lenses, `oxabl/impact` and `workspace/symbol` read it. A full pass analyses
each file of the workspace one time. On the generated `erp` workspace (10,386
roots) it costs about 80 CPU seconds.

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

A `workspace/didChangeWatchedFiles` event starts a pass only through these
rules. The server registers plain globs, and a client such as VS Code applies
them to every workspace folder, so the server of one folder also gets the
events of the other folders. The server drops an event for a file that the
session does not read (`WatchScope` in `crates/oxabl_lsp/src/watch_scope.rs`).
It keeps an event when the path is:

- under the workspace root;
- under an include path, a `.df` file or schema directory, or a directory of
  `[workspace.sources].directories`, also when it lies outside the root;
- an `oxabl.toml` in the root or in a directory above it;
- a file that an analysis read (an indexed file or an include), an open buffer,
  a file in the directory of an open buffer, or an include of an open buffer.

A known boundary: with `root_relative_include_paths = ["."]`, a class file whose
package segments name the directories of the workspace root itself (`CLASS
alpha.Foo:` in `<parent>/alpha`) searches from the directory above the root. A
file that only the workspace pass reads there, and no open buffer, is not a
place, so its event is dropped. Making `["."]` keep every event would defeat
the filter for the common setup.

A client that declares no root, and a configuration with a
`root_relative_include_paths` entry that is absolute or leaves the directory of
its file, keep every event. Without this filter, a `.df` or `oxabl.toml` event
of a sibling folder re-resolved the configuration, republished the diagnostics
of every open buffer and made the next pass full.

A pass that completes after a buffer moved is not dropped when the pass read
every file as the buffers give it now.

## The parts of a pass

Each file gives one part to a pass (`crates/oxabl_daemon/src/pass.rs`):

- its rows of the graph (`oxabl_pipeline::FilePart`): the subjects it depends
  on, with the kind and the span of each edge, the references it could not
  resolve, why its edge set is incomplete, and its dynamic calls;
- its rows of the symbol search, which the pass keeps one by one and shares
  with the next pass (a row of a file that was not analysed again is not
  copied);
- its inputs: the paths that its analysis read, the paths it looked for and
  did not find, and the directories it listed;
- when the pass built the map of `SHARED` producers (below): the `SHARED` names
  the file produces, and the inputs of its scan, added to its inputs.

The pass reads through an `ObservingFileSystem`, which records the inputs of
each analysis. That includes the include files at any depth and the class and
program files whose facts the analysis took from the index, also when the
index read them for another file. A layer under it keeps, for each path read,
the hash (BLAKE3) of the text and the disk stamp before the read.

The daemon keeps the parts of the last pass. `ReverseGraph::from_parts` joins
them into the graph that `ReverseGraph::build_with` gives for the same text.
The daemon also keeps the graph of the last pass. After an edit,
`ReverseGraph::updated` starts from that graph and replaces the rows of the
files that were analysed again, and of the files that are gone, in the lists of
only the subjects that those files name now or named before. Every other list
of rows is shared with the earlier graph. The graph is equal to the one that
`from_parts` gives (`an_updated_graph_is_the_graph_of_the_whole_join` compares
them after 300 edits). When two files have one normalised path, the join is the
whole join.

The graph names each file by a `GraphFileId`: the place of its path in one
table of the graph, which is sorted in the order of `cmp_paths` and has no
repeats. The order of two ids is therefore the order of their paths, and the
rows of a subject, the sets of a search and the sorts of an answer work on
integers. A path appears only when the graph answers a caller
(`ReverseGraph::file_path`, `ReverseGraph::dependents`). The lists of rows are
shared with the earlier graph only when both graphs have one table of files.
When a file is added or removed, `updated` gives each row the id of the same
path in the new table, and drops the rows of a file that the new table lacks.

### The map of `SHARED` producers

A pass builds the map of `SHARED` producers on the first question of any
analysis (`DeferredProducers`), and not before. A pass in which no file asks
for a producer never scans the files for `SHARED` names. The pass gives the
map its own scan of each file (`DeferredProducers::with_scan`), so it keeps the
names and the inputs of each scan.

- When the pass built the map, each kept part has its `SHARED` names and the
  inputs of its scan. The next pass scans the files it analyses again, and
  compares the map with the kept one.
- When the pass did not build the map, no analysis read it, so no kept part
  depends on it. The next pass gives its analyses a new lazy map. When one
  asks, the pass scans every file, and keeps the names and the inputs for the
  next pass.

### The table of paths

Each input is a path with a record of what the pass read there: the hash of
the text and the disk stamp, that the path was looked for and not found, or the
stamp of the directory when it was listed. The records are kept for the paths
that a kept part reads and for no others. The pass cuts the table to those
paths before it checks the inputs and when it ends, so:

- a record of a path that no kept file reads cannot go stale. Such a record was
  not checked, and a later file that read the path again would have taken the
  hash of text that is gone as the hash of what it read;
- the table has at most one record for each input of a kept part. It does not
  grow with the paths that edits read once and forgot, such as the include
  candidates of a name that was typed and removed.

The table is shared between passes while no path comes or goes, and the
records are shared one by one, so a pass does not copy the paths or the stamps.

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
   parts of removed files, and the paths that no kept part reads.
4. When the earlier pass built the map of `SHARED` producers, scan the files of
   step 3 for the `SHARED` names they produce. When the map is not the same,
   run a full pass: each file can read that map, and no part records which
   names it asked about.
5. Join the parts. When no file was analysed and the file list is the same,
   the daemon keeps the graph and the symbol rows that it has. A reference lens
   that holds the pass (`references::Pass`) then stays current.

## What follows a pass

In a session that has asked for a type hierarchy or for go to implementation
once, a pass that the daemon stores starts the build of the subtype index on a
thread of its own (`crates/oxabl_daemon/src/subtypes.rs`,
`docs/design/type-hierarchy.md`). In a session that never asked, a pass starts
no build, so `edit_pass_bench` times the pass alone (it checks that no build
started). The build reads the facts of the class files
and holds a database snapshot, so it follows the rules of this document for
background work: it never holds the sessions lock, a write to a buffer stops
it at the next file (it does not wait for it), and a request for another
feature does not wait for it. An edit does not start a pass by itself. The next
request that needs a pass runs it, and the pass then starts a build for the new
graph.

## Fail closed

A pass is full when:

- there is no kept pass, or its session generations of the configuration or
  the schema are not the current ones;
- the configuration that the pass resolves from disk is not the same as the
  one of the kept pass (`ConfigKey`), also when no client said that
  `oxabl.toml` changed;
- the map of `SHARED` producers changed.

`ConfigKey` names each field of the configuration that an analysis reads:
the include paths, the preprocessor settings, the file extensions, the
decoding, the lint settings, the features and the schema. The schema is a
hash of its tables, sequences and files in their order, so a schema that was
parsed and one that the schema cache gave back have the same key. A new field of
the configuration needs a line there.

Two writes during one pass can give two analyses different text of one file.
The part keeps the first hash, so the next pass analyses again each file that
read the file.

The freshness check (`oxabl/freshness`, and the check of each find references
request) also looks for the paths that the pass looked for and did not find.
A path that is there now makes the pass stale, with the same rate limit as the
walk for added files. So a new include earlier on the include path, which no
count of root files can see, makes the next request run a pass.

A directory stamp is taken when the directory is first listed: before a
listing, or, for a directory below the first one of a case-insensitive lookup,
as soon as the lookup returns. A file that is added to such a directory during
that lookup is not seen.

## Cost

An edit of one file analyses the file and the files that read it. A file that
`RUN`s a program reads the program (the index reads its parameters), so an
edit of a program also analyses its callers. An edit of an include analyses
each unit that includes it. An edit of a class analyses the files that read
the class, also through inheritance.

On the `erp` preset (`docs/performance.md`), an edit of a program that two
files run analyses 3 files, and the next find references takes 1.4 s and 2.5
CPU seconds, against 8.2 s and 80 CPU seconds with a full pass. An edit of a
program that 570 files read analyses 571 files (2.8 s, 15.5 CPU seconds).
The kept parts take about 55 MB, as many after an edit as after the cold pass.
The schema key (`ConfigKey`) took 15 ms to build on the 1,230 tables of the
`erp` schema, because the hasher gave BLAKE3 a few bytes at a time. It gives
the bytes in blocks of 16 KiB now (`BufferedBlake3`).

What a pass costs after the analysis, whatever the edit, is the fixed cost.
The parts of it, on the `edit_pass` bench (`crates/oxabl_daemon/benches/edit_pass_bench.rs`,
1,200 roots, an edit of a program that two files run, about 29 ms before and
about 17 ms after, on one thread):

- the join of the graph from all the parts: 7 ms, now 2 ms;
- the walk for the files of the workspace: 4 ms, now 0.7 ms. The walk asks for
  the canonical path of each file only when it met a symbolic link, because
  without a link no file has a second spelling;
- the rows of the symbol search are shared and no longer copied at each pass;
- the size estimate of the graph, 1.3 ms, is made when `oxabl/reindex` asks
  and not at each pass;
- what is left is in system calls and in the copy of the table of kept parts:
  the check of each input (3 ms), the stamps of the tracked files (1.3 ms), the
  stamps that the next freshness check reads (1.3 ms), and the configuration
  (a pass resolves `oxabl.toml` and the schema again, which is needed to see a
  change that no client reported).

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

`crates/oxabl/tests/watched_files_scope_lsp.rs` sends the events of a sibling
folder (a program, an include, a `.df` file and an `oxabl.toml`) and checks
that the server publishes nothing, runs no pass and sends no `workspace/*/refresh`
request (the session advertises every refresh capability; a control test shows
that an event for the root's own files does send one). It also checks that an
event still reaches the session for a file on an include path outside the
root, for a `.df` file outside the root, for an `oxabl.toml` above the root, and
for an include in a subdirectory of an open buffer outside the root that no
configured place names (kept by the include dependencies of the buffer). No
black-box test shows the registry of files an analysis read (`has_read`) alone:
an open buffer is analysed in the same session. The place rules are also tested
one by one in `watch_scope.rs`.

`crates/oxabl/tests/incremental_pass_script_lsp.rs` runs a series of edits
(a buffer edit, a write, a removal, a buffer opened or closed) through the
real `oxabl lsp`, and compares the references after each step with a new
daemon on the same text. It tests:

- an include or a file that no kept file reads for a while, and that is read
  again as it was in the first pass, with the include in a buffer, on disk,
  and as a root file that is removed and made again;
- a path that the pass looked for and did not find, and that is made later: an
  include, an include ahead of the one in use, a program and a class (no pass
  has to be asked for by another change), and a buffer opened at such a path;
- a directory that a case-insensitive lookup listed, and in which a file is
  made;
- the bound of the table: after 30 rounds of an include of a name that exists
  nowhere, and its removal, the bytes of the kept parts are those of the first
  rounds;
- a schema from the schema cache and a parsed one: the passes after the cache
  is removed and filled again are not full.

Each step also compares the workspace symbols and the incoming calls of each
open file with the new daemon: the pass keeps their rows, and a search over
files does not read them, so a row of a file that is gone shows only there.

`crates/oxabl_daemon/tests/incremental_graph.rs` asks the daemon in the process
for `oxabl/impact`, because no language server request reads the edges that it
tests. It tests a file that starts to produce a `SHARED` name that a consumer
used with no producer (the consumer read no file, so only the map of the
producers says that it changed), a path that two include paths both have and
that a `RUN` only checks for, and that is removed, and the same path when
another file reads it later and its text changes.
