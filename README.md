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
  - Needs test coverage.
  - Used in our token dumps and benchmarks, appears to be accurate.
- `oxabl_ast`: Started in `crates/oxabl_ast`
  - Defines literals
  - Needs expressions
- `oxabl_parser`: Started in `crates/oxabl_parser`
  - Started simple literal parsing
  - Expressions next

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

Benchmarks are run under `bench` with `cargo bench -p <lib>` such as `oxabl_lexer`. Each library will have a benchmark so we can track the performance of individual components in the toolset.

### Lexer

- **Comparison:** I haven't used any other ABL lexers while working as an ABL developer, so I don't really know how to use what's out there. If you have access to an ABL lexer and can run a benchmark, please provide those numbers, it's much appreciated!

**Benchmark:**
| Test Name               | Time (min) | Time (avg) | Time (max) | Throughput Min | Throughput Avg | Throughput Max |
| ----------------------- | ---------- | ---------- | ---------- | -------------- | -------------- | -------------- |
| lexer/tokenize_keywords | 1.4692 ms  | 1.4762 ms  | 1.4838 ms  | 11.121 MiB/s   | 11.179 MiB/s   | 11.232 MiB/s   |
| lexer/tokenize_full     | 1.4929 ms  | 1.5098 ms  | 1.5262 ms  | 10.812 MiB/s   | 10.929 MiB/s   | 11.054 MiB/s   |

~11MiB/s throughput is pretty good for a handrolled lexer MVP, so we're aiming for ~11MiB/s or higher from here on. The long term goal is to *increase* this number. A release should never *decrease*  without good reason. But we're only human.

I haven't run this benchmark in an optimized environment- it's running in WSL2, on a Windows PC with lots of browers and tasks running, similar to how it would run if a developer were actually using it. I'm sure these numbers could be much higher on a better PC with less background noise.

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

You need to provide a file, as these functions simply return strings, they don't manipulate files directly.

**They also do not clear files** so running the command against a file that already has contents will just concat the contents, potentially duplicating code. Be aware of your actions. *Use `>`* to overwrite the file contents.
