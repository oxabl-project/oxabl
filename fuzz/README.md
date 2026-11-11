# Fuzz targets

`cargo-fuzz` targets for the lexer, the preprocessor and parser, the lint
pipeline and the formatter. The crate is outside the workspace, so
`cargo build --workspace` needs no nightly toolchain.

| Target | What it checks |
| --- | --- |
| `lex` | `tokenize` never panics, ends with one `Eof`, and its spans are ordered and on character boundaries. |
| `preprocess_parse` | The raw parse, and the parse of the preprocessed text, never panic. |
| `lint` | `LintPipeline::run` on one file (preprocess, parse, analyse, every rule) never panics. |
| `format` | `try_format_source` never panics, and formatting its own output changes nothing. |

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
inputs of the findings in the table below.

| Input | Target | Result |
| --- | --- | --- |
| `findings/parser/stack-overflow-nested-parens-6kb` | `lint`, `format`, `preprocess_parse` | Stack overflow in the expression parser. |
| `findings/parser/stack-overflow-18kb` | same | Stack overflow in the statement parser. |
| `findings/format/bare-cr-not-stable.abl` | `format` | The first format gives `"\r\n"`, the second `""`. |
| `findings/daemon/stack-overflow-from-edits.cls` | `oxabl lsp` (see `scripts/soak`) | 416 `(` in a document kill the daemon. |
| `findings/daemon/macro-doubling-1gb.p` | `oxabl check` | 28 `&SCOPED-DEFINE` lines that double each other: 1.3 GB, and 3 GB at 30 lines. |
