# Oxabl

High performance oxidized tooling for Progress ABL, written in Rust.

## Unofficial

No affiliation with Progress.

## Status

The first library will be `oxabl_parser`.

Requirements:
- `oxabl_lexer`: MVP has been completed in `crates/oxabl_lexer`.
  - Produces tokens against all known ABL keywords, primitive datatypes, operators, and identifiers.
  - Fairly comprehensive test coverage at 27 tests to verify we can produce tokens in a variety of scenarios.
  - Benchmarks and token dumps in `crates/oxabl_lexer/benches` and `crates/oxabl_lexer/examples` using a test file in `resources/bench_keywords.abl` provide a 'real-world' example, a 17KB file containing 2421 tokens can be tokenized in ~1.9ms.
- `source_map`: Work has started in `crates/oxabl_common`.
  - It's able to produce line and column numbers from byte offsets stored in tokens.
  - Needs test coverage.
  - Used in our token dumps and benchmarks, appears to be accurate.
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

## Benchmarks

As a high performance oriented library, Oxabl is focused on hitting low numbers and keeping them low across versions.

Benchmarks are run under `bench` with `cargo bench -p <lib>` such as `oxabl_lexer`. Each library will have a benchmark so we can track the performance of individual components in the toolset.

### Lexer

- **Comparison:** I haven't used any other ABL lexers while working as an ABL developer, so I don't really know how to use what's out there. If you have access to an ABL lexer and can run a benchmark, please provide those numbers, it's much appreciated!

**Benchmark:**
| Test Name               | Time (min) | Time (avg) | Time (max) | Throughput Min | Throughput Avg | Throughput Max |
| ----------------------- | ---------- | ---------- | ---------- | -------------- | -------------- | -------------- |
| lexer/tokenize_keywords | 1.8565 ms  | 1.9489 ms  | 2.0620 ms  | 8.0026 MiB/s   | 8.4672 MiB/s   | 8.8886 MiB/s   |
| lexer/tokenize_full     | 1.8369 ms  | 1.8853 ms  | 1.9364 ms  | —              | —              | —              |

~8.5MiB/s throughput is pretty good for a handrolled lexer MVP, so we're aiming for ~8.5MiB/s or higher from here on. The long term goal is to *increase* this number. A release should never *decrease*  without good reason. But we're only human.

**Full token dump**:
`cargo run -p oxabl_lexer --example dump_tokens`

**Just errors**
`cargo run -p oxabl_lexer --example dump_tokens -- --errors`

**Just summary**
`cargo run -p oxabl_lexer --example dump_tokens -- --summary`

## Optimizations

I don't know anything about these techniques! But I'm excited to learn. Here's what's on the roadmap for the lexer:

- **SIMD scanning**
  - Process more bytes at once using CPU vector instructions instead of looping byte-by-byte.
  - **How it sounds to a noob:** Pretty cool.
- **Branchless state machines**
  - replace if/match with lookup tables
  - Build a table of `transitions[state][byte] -> next_state` and index directly, `current_state = table[current_state][byte]
  - **How it sounds to a noob:** A lot of work for diminishing returns? Idk.
- **Arena Allocation**
  - Allocate many small objects into a single buffer and free everything at once instead of individual deallocations.
  - Tokens are short lived, so this eliminates an allocation and deallocation for every single token, and improves caching.
  - Instead of pushing a new token, allocate all tokens into an Arena, then drop the whole thing after parsing.
  - **How it sounds to a noob:** Neat, makes sense.

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
