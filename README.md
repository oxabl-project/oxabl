# Oxabl

High-performance tooling for Progress ABL.

*No affiliation with Progress.*

## Wouldn't it be nice if...

- You could lint an ABL code base in an instant?
- You could format an ABL code base in an instant?
- You had a language server that understood ABL?
- You could write your own lint rules for ABL?
- A Visual Studio Code extension for ABL worked with no dependency on the AVM, just code and schema?
- ABL had an incremental build system, instead of recompiling everything, or compiling selected files and missing their dependencies?
- You could generate a source map of your code base, including dead code and over-complicated code?
- You could write stricter ABL that prevents memory leaks and bad transaction scopes, deterministically?
- You could parse ABL, generate an AST, use semantic analysis, and build new tools on top of it for whatever you need?

**Well, all of that and more is here with Oxabl.**

## What is Oxabl?

Oxabl is a suite of developer tools made of 24 crates. The umbrella crate, `oxabl`, wraps the individual crates, such as `oxabl_parser`, into one toolchain for everything an ABL developer needs.

`oxabl` formats your code to a style guide, lints it with strict rules, and runs static and semantic analysis on your code base.

`oxabl` is also becoming a build tool. Together with an AVM runtime, it will compile your ABL code bases faster than ever, with multi-threading and incremental compilation.

## Try it

```sh
cargo install oxabl
oxabl check path/to/src     # lint and report formatting drift
oxabl format path/to/src    # format in place
```

## Is Oxabl ready?

Nearly. Several parts of Oxabl are stable, but there are still few lint rules, the formatter has some small quirks, and the Visual Studio Code extension is still in early development.

More detail: [docs/overview.md](docs/overview.md).
