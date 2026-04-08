# Oxabl

[![CodSpeed](https://img.shields.io/endpoint?url=https://codspeed.io/badge.json)](https://codspeed.io/oxabl-project/oxabl?utm_source=badge)

High performance oxidized tooling for Progress ABL, written in Rust.

## Unofficial

No affiliation with Progress.

## Status

The first library will be `oxabl_parser`.

Requirements:
- `oxabl_lexer`: MVP has been completed in `crates/oxabl_lexer`.
  - Produces tokens against all known ABL keywords, primitive datatypes, operators, and identifiers.
  - Comprehensive test coverage.
  - Run against a realistc 390kb syntactically correct ABL file and correctly tokenized it.
  - Benchmarks and token dumps in `crates/oxabl_lexer/benches` and `crates/oxabl_lexer/examples` using a test file in `resources/bench_keywords.abl`.
- `source_map`: MVP has been completed in `crates/oxabl_common`.
  - It's able to produce line and column numbers from byte offsets stored in tokens.
  - Test coverage
  - Used in our token dumps and benchmarks, appears to be accurate.
- `oxabl_ast`: Implemented in `crates/oxabl_ast`
  - Defines literals, statements, expressions, variable definitions, control flow, and data types.
- `oxabl_parser`: Actively developed in `crates/oxabl_parser` with 147 tests
  - Parses expressions with proper operator precedence
  - Parses statements: DEFINE VARIABLE, VAR, assignments, DO blocks (with counting), IF/THEN/ELSE, REPEAT, LEAVE, NEXT, RETURN, CASE, FIND, FOR EACH, PROCEDURE
  - Parses postfix operations: method calls, member access, array access, field access

Current Work: RUN statement (in progress), DISPLAY and MESSAGE statements next.

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

**Disclaimer**: There is no long-term plan to take all of these stand-alone libraries and executables and create a cohesive experience. For now, it will be duct-taping things together. Perhaps an `oxabl` CLI?

**Assisting?**: Some of these are stand alone executables or libraries that *assist* the developer working with ABL, they don't do everything. ABL is closed source, and you cannot compile ABL to byte code without the ABL compiler. That being said, you can make the process faster and more enjoyable. Because you need the AVM and compiler at the end of the day, what Oxabl can accomplish is limited.

## Benchmarks

As a high performance oriented library, Oxabl is focused on hitting low numbers and keeping them low across versions.

Benchmarks are run with `cargo bench -p <crate>`. Each crate has its own benchmark so we can track the performance of individual components in the toolset.

These are not sanitized benchmarks — they were run on real hardware with normal background processes, similar to how a developer would actually use the tools.

### Intel i7-8550U Laptop

**Hardware:** Intel Core i7-8550U (8) @ 4.00 GHz, 15.37 GiB RAM, Linux 6.19.10-arch1-1

#### Source Map (`oxabl_common`)

| Benchmark                | Time (min)  | Time (avg)  | Time (max)  | Throughput (avg)  |
| ------------------------ | ----------- | ----------- | ----------- | ----------------- |
| source_map/construction  | 30.489 µs   | 32.131 µs   | 33.730 µs   | 513.57 MiB/s      |
| source_map/lookup        | 123.78 ns   | 129.75 ns   | 136.13 ns   | 38.536 Melem/s    |

#### Lexer (`oxabl_lexer`)

| Benchmark            | Time (min)  | Time (avg)  | Time (max)  | Throughput (avg)  |
| -------------------- | ----------- | ----------- | ----------- | ----------------- |
| lexer/keywords       | 296.83 µs   | 311.32 µs   | 328.06 µs   | 53.004 MiB/s      |
| lexer/strings        | 35.716 µs   | 38.389 µs   | 41.186 µs   | 52.368 MiB/s      |
| lexer/comments       | 17.277 µs   | 18.973 µs   | 21.219 µs   | 129.89 MiB/s      |
| lexer/numeric        | 40.489 µs   | 44.638 µs   | 48.754 µs   | 37.580 MiB/s      |
| lexer/preprocessor   | 44.151 µs   | 46.808 µs   | 49.424 µs   | 54.888 MiB/s      |

#### Parser (`oxabl_parser`)

| Benchmark              | Time (min)  | Time (avg)  | Time (max)  | Throughput (avg)  |
| ---------------------- | ----------- | ----------- | ----------- | ----------------- |
| parser/full_program    | 375.37 µs   | 399.65 µs   | 425.98 µs   | 41.290 MiB/s      |
| parser/expressions     | 134.78 µs   | 148.92 µs   | 165.12 µs   | 15.459 MiB/s      |
| parser/declarations    | 79.781 µs   | 89.965 µs   | 102.17 µs   | 28.536 MiB/s      |
| parser/control_flow    | 123.64 µs   | 132.17 µs   | 140.54 µs   | 17.664 MiB/s      |
| parser/oo_abl          | 113.30 µs   | 120.84 µs   | 128.50 µs   | 32.666 MiB/s      |
| parser/temp_tables     | 67.884 µs   | 73.143 µs   | 78.881 µs   | 35.191 MiB/s      |
| parser/procs_funcs     | 148.33 µs   | 155.05 µs   | 161.11 µs   | 22.395 MiB/s      |

### Token Dumps

**Full token dump**:
`cargo run -p oxabl_lexer --example dump_tokens`

**Just errors**
`cargo run -p oxabl_lexer --example dump_tokens -- --errors`

**Just summary**
`cargo run -p oxabl_lexer --example dump_tokens -- --summary`

## Optimizations

I don't know anything about these techniques! But I'm excited to learn.

I consider the Lexer "production-grade" with the current benchmarks, it's more than within the realm of being usable for developer tooling, if you ran it on-save in your editor, it would only be ms to tokenize the entire file, which is more-or-less instant in an editor. Still, I love optimizing things, so we're certainly going to aim for better. Why not tokenize the entire codebase on save?? (Jokes)

Here's what's on the roadmap for the lexer:

- **Perfect Hash Table**
  - create a "perfect hash table", which could drop our 1600+ keyword comparison (which eats up 93% of our lexing time) to a 1-2 hash lookups + bounds check.
  - **Priority:** High, impacts the process we spend the most amount of time in.
- **Skip case conversion**
  - ABL treats upper and lowercase as valid for keywords, so we are converting everything to lowercase, which requires an allocation.
  - We could inline a case-insensitive comparison.
  - **Priority:** High, impacts the process we spend the 2nd most amount of time in.
- **Arena Allocation**
  - Allocate many small objects into a single buffer and free everything at once instead of individual deallocations.
  - Tokens are short lived, so this eliminates an allocation and deallocation for every single token, and improves caching.
  - Instead of pushing a new token, allocate all tokens into an Arena, then drop the whole thing after parsing.
  - **Priority:** Medium, not as complex as some optimizations while still offering decent returns.
- **SIMD scanning**
  - Process more bytes at once using CPU vector instructions instead of looping byte-by-byte.
  - **Priority:** Low, could be another significant speed-up, but after implementation, lexer will be harder to maintain, so leave it for now.
- **Branchless state machines**
  - Replace if/match with lookup tables
  - Build a table of `transitions[state][byte] -> next_state` and index directly, `current_state = table[current_state][byte]
  - **Priority:** None, might not be worth our effort.

## Contributing

### CI

Every push and PR to `master` runs the following checks (all must pass):

- `cargo check` — compilation
- `cargo test` — full test suite
- `cargo fmt --check` — formatting
- `cargo clippy -D warnings` — lints

### Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/) format. This drives the automated release process:

- `feat: add X` — new feature (bumps minor version)
- `fix: correct Y` — bug fix (bumps patch version)
- `feat!: breaking change` — breaking change (bumps major, or minor while pre-1.0)
- `chore:`, `docs:`, `refactor:`, `test:` — won't trigger a release, but appear in the changelog

### Releases

Releases are fully automated via [Release Please](https://github.com/googleapis/release-please):

1. Merge PRs with conventional commit messages into `master`
2. Release Please accumulates commits and maintains an open release PR with a generated changelog and version bumps across all `Cargo.toml` files
3. When the release PR is merged, a GitHub Release and git tag are created automatically

No manual version bumping is needed.

## CodeGen

We generate code for all the keywords and operators to use within the project. Use these commands to generate the code:

```rust
cargo run -p oxabl_codegen -- <command>
```

Valid commands are:
- `kind`
  - generates the `kind.rs` file for the lexer.
- `atoms`
  - generates the `build.rs` file for the lexer.
- `summary`
  - outputs status and usage
- No command
  - generates all files

Commands write generated files directly to their target locations. Generated files include a "DO NOT EDIT" header. No manual file redirection is needed.
