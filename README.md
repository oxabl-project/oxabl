# Oxabl

High performance oxidized tooling for Progress ABL, written in Rust.

## Unofficial

No affiliation with Progress.

## Status

The first library will be `oxabl_parser`.

Requirements:
- `oxabl_lexer`: Work has started in `crates/oxabl_lexer`.
  - It's able to produce tokens against a variety of simple tests.
  - Needs real-world tests and more test coverage.
  - Has a few TODO items left, but overall, seems to be producing tokens with correct kinds, offsets, and values.
- `source_map`: Work has started in `crates/oxabl_common`.
  - It's able to produce line and column numbers from byte offsets stored in tokens.
  - Needs test coverage.
  - If it can generate accurate line and column numbers, it's done. But it hasn't been tested at all. Looks good in theory.
- `oxabl_ast`: Not started
- `oxabl_parser`: Not started (well, the building blocks of it have been, i.e. lexer and source map.)

## Roadmap

**Goal**: A high performance suite of command line tools and libraries to make ABL development blazingly fast and more effective.

- `oxabl_parser` - foundation for understanding ABL code.
  - `oxabl_lexer` and the `oxabl_ast` make up the foundation of the foundation.
- `oxabl_fmt`    - CLI tool for formatting ABL code.
- `oxabl_lint`   - CLI tool for linting ABL code.
- `oxabl_minify` - CLI tool for removing dead code, shortening syntax, and code obfuscation.
- `oxabl_build`  - CLI tool for *assisting* in the compilation of ABL.
- `oxabl_run`    - CLI tool for *assisting* in the running of ABL.
- `oxabl_test`   - CLI tool for *assisting* in the testing of ABL.

**Disclaimer**: There is no long-term plan to take all of these stand-alone libraries and executables and create a cohesive experience. For now, it will be ductaping things together. Perhaps an `oxabl` CLI?

**Assisting?**: Some of these are stand alone executables or libraries that *assist* the developer working with ABL, they don't do everything. ABL is closed source, and you cannot compile ABL to byte code without the ABL compiler. That being said, you can make the process faster and more enjoyable. Because you need the AVM and compiler at the end of the day, what Oxabl can accomplish is limited.

## CodeGen

We generate code for all the keywords and operators to use within the project. Use these commands to generate the code:

```rust
cargo run -p oxabl_codegen -- <command> >> <file>
```

Valid commands are:
- kind
  - generates the `kind.rs` file for the lexer.
- atoms
  - generate the atom list used by `build.rs` for the lexer.
- match
  - generate the keyword match function used by the lexer.
- all
  - all of the above
- summary or nothing
  - outputs status and usage

You need to provide a file, as these functions simply return strings, they don't manipulate files directly.

**They also do not clear files** so running the command against a file that already has contents will just concat the contents, potentially duplicating code. Be aware of your actions. **Use >* to overwrite the file contents.
