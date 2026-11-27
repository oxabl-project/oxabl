# Parse speed: oxabl and Proparse

`compare.sh` parses one ABL tree with three parsers and prints, for each, the wall time of every pass, the peak RSS of the process and the files that failed:

| Tool | Version | What the harness calls |
| --- | --- | --- |
| oxabl | the checkout | `oxabl conformance --preprocess -j <threads>` |
| Proparse (Riverside) | 3.11.1, from Maven Central (`eu.rssw.openedge.parsers:proparse`) | `riverside/src/main/java/RiversideBench.java`: one `RefactorSession` for each thread, then `new ParseUnit(...).parse()` for each file, as Riverside's SonarQube plugin does. This is the Proparse line that Riverside's VS Code extension bundles. |
| Proparse (Consultingwerk) | 5.1.0, the `proparse.java.zip` release asset (SHA-256 pinned) | `consultingwerk/CwBench.java`: `Environment` settings, `Schema.loadSchema`, then `new ParseUnit(file, charset).parse()` for each file. This is the Proparse line that the SmartComponent Library loads. |

Each tool preprocesses (include files, `&IF`, `{&name}`), lexes and parses every compile unit below `--root` (`.p`, `.w`, `.cls`, `.v`) into a syntax tree. None of them runs semantic analysis here. Riverside's line is licensed LGPL-3.0, Consultingwerk's EPL-2.0 or LGPL-3.0; the harness downloads them and redistributes nothing.

## Run it

```sh
nix shell nixpkgs#jdk21 nixpkgs#maven nixpkgs#time -c \
  scripts/bench/compare/compare.sh --root <tree> --propath <tree>,<dlc>/src \
    --df <tree>/schema/app.df --charset UTF-8 --threads 16 --passes 3
```

`--propath` is the include search path of all three tools, comma separated. `--df` names each database schema; the database name is the file name. The output goes below `target/bench-compare`. Run `--threads 1` too: the Consultingwerk line always runs on one thread, so a one-thread run is the only one where all three do the same work in the same way.

## Read the numbers

- **Compare warm passes.** Proparse runs in a JVM, and its first pass includes class loading and JIT compilation. Each pass after the first is a warm pass in the same process; give the median of those. oxabl is a native binary: each run is a new process, and its time includes the start.
- **Memory is the peak RSS of the process.** The JVM grows its heap to the default maximum (a quarter of RAM) before it collects hard, so the Java peak is what a user gets by default, not the least the parser needs. A run with `-Xmx` shows the floor and its cost in time; say which one you publish.
- **Count the failed files of each tool.** A parser that stops early on a file it does not understand reads as fast. Check that the tree compiles with OpenEdge, so a failure is the parser's and not the code's.
- Run on a quiet machine and record the load. Wall time follows the load of the whole machine.
- `CwBench` sets `opsys` to `unix` and `proversion` to `12.8`; change them for another target.

## Known limits

- Riverside's `.df` reader stops with a `NullPointerException` on an index with an empty field name (`INDEX-FIELD ""`). Remove that index from a copy of the `.df` before the run.
- Benchmark results of OpenEdge itself are subject to Progress's licence terms. This harness runs no OpenEdge code; keep it that way.
