# Fuzz targets

`cargo-fuzz` targets for the lexer, the preprocessor and parser, the lint
pipeline, the formatter, the document outline and navigation (go to definition
and document links). The crate is outside the workspace, so
`cargo build --workspace` needs no nightly toolchain. The CI job `Fuzz targets`
formats and lints the crate on stable, so the targets keep compiling; it does
not run them.

The workflow `.github/workflows/fuzz.yml` runs them: every night at 03:17 UTC,
15 minutes per target, each target starting from the corpus the previous night
grew, plus the grammar fuzz of the VS Code client with a new seed. Any artifact
(a crash, a timeout, an out-of-memory input) fails the job and is uploaded as
`fuzz-findings-<target>`. A pull request that changes `fuzz/` or the workflow
runs it for 60 seconds per target; `workflow_dispatch` takes the seconds.

| Target | What it checks |
| --- | --- |
| `lex` | `tokenize` never panics, ends with one `Eof`, and its spans are ordered and on character boundaries. |
| `preprocess_parse` | The raw parse, and the parse of the preprocessed text, never panic. |
| `lint` | `LintPipeline::run` on one file (preprocess, parse, analyse, every rule) never panics. |
| `format` | `try_format_source` never panics, and formatting its own output changes nothing. |
| `outline` | `outline` never panics, its spans and names obey the rules of `docs/design/document-outline.md`, and a second run gives the same outline. |
| `navigate` | On a fuzzed file, `.df` and include (`source \0 schema \0 include`) in a small workspace, go to definition at every offset and the document links never panic, every target is a file of the workspace, and every span is inside its file and on character boundaries. The schema browser's tree of the fuzzed `.df` holds the same tables and fields as the loader, both ways, also when an earlier file declares the tables it extends; every span of it is inside the file and on character boundaries; the language server's `byte_to_position` places every range where an independent count of lines and UTF-16 units does; and the tree serializes. |

Each target runs its input with a stack of at least the analysis stack
(`oxabl_common::stack`, 64 MiB), as the CLI and the daemon do: on a smaller
stack, a nest that the parser accepts can overflow and show as a false crash.
`run.sh` raises `ulimit -s`, so the input runs directly on libFuzzer's thread. It also builds with debug assertions (`-a`), so a `debug_assert` in the parser or a pass is a finding.
With a smaller limit, for example a target started by hand, a worker thread on
the analysis stack runs it, at about 0.5 ms more per input (`src/lib.rs`).

```bash
cargo install --locked cargo-fuzz --root target/tools   # once, into the checkout
export PATH=$PWD/target/tools/bin:$PATH
fuzz/seed.sh                      # corpus: every ABL file of the repository, plus deep and exponential shapes
fuzz/run.sh lint 600              # 10 minutes; limits: 2 GB RSS, 10 s per input, 64 KB inputs
cargo +nightly build --release --manifest-path fuzz/Cargo.toml --bin slowest
fuzz/target/release/slowest -n 10 fuzz/corpus/lint/*   # the slowest inputs of a corpus, per stage
```

Findings land in `fuzz/artifacts/<target>`; minimise one with
`cargo +nightly fuzz tmin <target> <file>`. `fuzz/findings/` keeps the minimised
inputs of the findings in the table below. `crates/oxabl/tests/format_stability.rs`
formats each input of `findings/format` twice through `oxabl format --stdout`
and fails when the second pass changes the output.

| Input | Target | Result |
| --- | --- | --- |
| `findings/parser/stack-overflow-nested-parens-6kb` | `lint`, `format`, `preprocess_parse` | Stack overflow in the expression parser. |
| `findings/parser/stack-overflow-18kb` | same | Stack overflow in the statement parser. |
| `findings/format/bare-cr-not-stable.abl` | `format` | The first format gives `"\r\n"`, the second `""`: the formatter kept a `\r` at the end of a line, and the line ending it wrote after it made a new `\r\n`. The formatter now reads a run of `\r` before a `\n` as part of the line ending, as the lexer reads `\r` as whitespace. |
| `findings/format/string-newline-lone-period.abl` | `format` | The first format indents the lone `.` by one space, the second removes it. The statement `F` starts on a line that begins inside the string, so that line keeps its bytes, but its continuation line moved as if the line moved to its depth. A continuation line now moves by nothing when the first line of its statement keeps its bytes. |
| `findings/daemon/stack-overflow-from-edits.cls` | `oxabl lsp` (see `scripts/soak`) | 416 `(` in a document kill the daemon. |
| `findings/daemon/macro-doubling-1gb.p` | `oxabl check` | 28 `&SCOPED-DEFINE` lines that double each other: 1.3 GB, and 3 GB at 30 lines. |
| `findings/outline/define-no-valid-symbol-6b` | `outline` | `&scop` plus a vertical tab: the `PreprocessorDefine` name is only whitespace, so `clean_name` gives `None` and the walker's `debug_assert` in `make` fires. |
| `findings/outline/name-outside-statement-24b` | `outline` | An untyped parameter list (`METHOD VOID Ru(n  ):`): the parser names the parameter `$0` with a span up to the next token, past the end of the parameter statement, and `named` asserts the name is inside the statement. |
| `findings/outline/parser-comment-tilde-2b` | `outline` (and any target built with debug assertions) | `~&`: a `Kind::Comment` token starts with `~`, and `classify_comment` in `oxabl_parser` asserts on it. Not in the outline code: the lexer left the token start on a stray `~`. The lexer keeps a stray `~` in the next token, on purpose: the formatter's guard compares token text, so it sees a lost tilde. `classify_comment` now skips leading tildes. |
| `findings/outline/parser-comment-tilde-blank-4b` | `outline` (and any target built with debug assertions) | `~ //`: the same assertion. The lexer also keeps the blanks after a stray `~` in the comment token. `classify_comment` now skips the blanks after a leading tilde too. |
