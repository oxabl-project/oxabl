# TigerStyle in oxabl

This is the coding style for the Rust code in this repository. It follows [TigerStyle](https://github.com/tigerbeetle/tigerbeetle/blob/main/docs/TIGER_STYLE.md), the style of TigerBeetle, changed where a compiler front end and a language server are different from a database.

The design goals have an order: **safety, then performance, then developer experience**. When two options are open, take the one that makes a wrong program harder to write. Say why in the code or in the commit.

Each rule names what holds it: a lint, a test, or review. A rule that only review holds is still a rule.

## Status

The style is new here. The code does not meet all of it yet.

- Already true: the parser has a nesting limit, parsing threads get a known stack size, hot paths do not allocate, fuzzing runs each night, CI keeps the HTTP stack out of the default build, and each new hot path gets a benchmark.
- Not true yet: the lints in [The lints](#the-lints) are not on. They come in one at a time, with the procedure in [Adoption](#adoption).
- New and changed code follows this document now.

## Safety

### Control flow

- Use simple, explicit control flow.
- Recursion needs a depth limit that the code states, checks and tests. The parser stops at `Parser::MAX_NESTING` and `Parser::MAX_CHAIN`, so each pass over its tree has a known depth. A new recursive pass uses that limit or carries its own counter. *Held by:* `crates/oxabl/tests/deep_nesting.rs`, review.
- Split a compound condition into nested `if` and `else`. State an invariant positively: write `if index < count`, not `if index >= count`. For each `if`, decide if it needs an `else`. *Held by:* review. Clippy's `collapsible_if` and `collapsible_else_if` are off for this reason.

### Limits

- Put a limit on everything. Each loop, queue, channel, cache, retry and input has a fixed upper bound.
- Use `sync_channel` or another bounded channel with a stated capacity. Do not use an unbounded channel.
- A loop that must not end, such as an event loop, says so with an assertion.
- *Held by:* clippy `disallowed_methods`, review.

### Integers

- Stored data, wire types, offsets and ids use `u32` or `u64`. Use `usize` only where the code indexes.
- Convert between widths with `try_from` or `From`. Do not use `as` to make a number narrower or to change its sign.
- *Held by:* clippy `cast_possible_truncation`, `cast_sign_loss`, `cast_possible_wrap`.

### Assertions

An assertion finds a programmer error. An operating error, such as a file that is not there, is not an assertion: handle it.

- Assert arguments, return values, preconditions, postconditions and invariants. Aim for two assertions for each function, on average.
- Assert a property in two places where you can: before the code writes a value and after the code reads it back.
- Write one assertion for each condition: `assert!(a); assert!(b);`, not `assert!(a && b);`.
- Assert the relations between constants at compile time: `const _: () = assert!(…);`.
- Assert what must be true and what must not be true.
- Use `assert!`. Use `debug_assert!` only where a benchmark shows the cost of the assertion.
- A failed assertion is a panic. `catch_panic` stops the request or the file that failed, not the server. Start each thread that parses or analyses with `stack::builder()` or `stack::run`, because `catch_panic` cannot stop a stack overflow.
- *Held by:* review.

### Memory

TigerStyle allocates all memory at startup. oxabl cannot: it reads source trees of unknown size. These rules apply instead.

- Do not allocate on a hot path. Borrow, or reuse a buffer. The lexer matches keywords in a stack buffer for this reason.
- Give a buffer or a collection its capacity when you know the size.
- *Held by:* benchmarks, review.

### Functions

- A function has at most 70 lines.
- To split a function, keep the branches in the parent and move straight-line work to helpers. Keep changes of state in the parent. Make the helpers pure.
- Declare a variable where the code first uses it. Calculate or check a value next to its use.
- *Held by:* clippy `too_many_lines`.

### Errors

- Handle each error. Do not ignore a `Result`.
- Do not use `.unwrap()` outside tests. Use `.expect("…")` only for an invariant, and write the reason in the message.
- Do not write `let _ =` for a `Result`.
- Do not leave `todo!`, `unimplemented!` or `dbg!` in the code.
- *Held by:* rust `unused_must_use`; clippy `unwrap_used`, `expect_used`, `let_underscore_must_use`, `todo`, `unimplemented`, `dbg_macro`.

### Unsafe code

- Each `unsafe` block has a `// SAFETY:` comment that states the invariant. Add an assertion of the invariant where you can write one.
- *Held by:* rust `unsafe_code`; clippy `undocumented_unsafe_blocks`.

### Outside events

- Do not do the work in the callback that receives a message. Put the message in a bounded queue. Let the loop of the daemon or the language server take messages in batches and check for a cancel at known points.
- Give each option to a library call at the call site. Do not use `..Default::default()` for a setting that changes the result.
- *Held by:* review.

## Performance

- Make a cost sketch before you build. For a new subsystem or a hot path, estimate the bytes read for each file, the allocations for each token, the rows for each SQLite transaction and the round trips to the remote cache. Put the sketch in the plan.
- Make the slowest resource faster first: network, then disk, then memory, then CPU. Weigh each by how often the code uses it. Batch the work.
- Move a hot loop into a free function with primitive arguments. Then the compiler and the reader see what the loop touches.
- Add a benchmark for each new hot path, in its own fixture. Measure the base before the change. See `CLAUDE.md`, "Benchmarks".

## Developer experience

### Names

- Find the correct nouns and verbs. Do not abbreviate: write `source` and `target`, not `src` and `dst`.
- A name has one meaning. Prefer a noun that you can also use in a document.
- Put units and qualifiers last, with the most important word first: `latency_ms_max`, `offset_bytes`, `token_count`.
- An index, a count and a size are different things. Where they meet, give them types, such as `ByteOffset(u32)` and `LineIndex(u32)`. Convert between them on purpose.
- Say what a division does with the remainder: `div_ceil`, `div_euclid`, `checked_div`.
- Name a helper after its caller: `read_sector` and `read_sector_callback`. A callback is the last parameter.
- Acronyms follow the Rust convention: `LspState`, not `LSPState`.

### Structure

- Put the public API at the top of a file, the helpers below it and the tests last. In a type, put the fields first, then the nested types, then the methods.
- If a caller can mix up two parameters of the same type, use a struct with named fields or different types. *Held by:* clippy `too_many_arguments`, `fn_params_excessive_bools`.
- Do not copy a value into a second variable, and do not keep an alias of it. Pass a value larger than 16 bytes by reference. *Held by:* clippy `large_types_passed_by_value`, `large_enum_variant`.
- Use the simplest return type: nothing, then `bool`, then a number, then `Option`, then `Result`.
- Do not put an `.await` between a check and the use of what was checked. *Held by:* clippy `await_holding_lock`, review.

### Text

- A comment is a sentence: a capital letter, a full stop, a space after the slashes. A comment says why.
- Write commit messages that explain. The pull request text is not in `git blame`; the commit is. Use conventional commits and put the reason in the body.
- A line has at most 100 columns. `rustfmt` uses 100 but does not break long strings and comments, so check those lines yourself.
- Run `cargo fmt`.

### Dependencies and tools

- A new dependency needs a written reason in the pull request. Prefer `std`. The CI checks that keep `reqwest`, `rustls` and `tokio` out of the default build stay.
- Write new automation in Rust, as a `cargo xtask` command, not as a shell script. Move the scripts in `scripts/` to Rust over time. `scripts/install.sh` stays a shell script, because it runs before a toolchain is there.
- Do not leave technical debt. Do the work correctly the first time. Solve a blocking problem when you find it.

## Tests

- Test valid input, invalid input, and the border between them.
- Fuzzing is the last defence. It shows that a bug is there. It does not show that no bug is there. Understand the code first, then write that knowledge as assertions.

## The lints

This is the target. See [Adoption](#adoption) for how each lint comes in.

```toml
# Cargo.toml (workspace root). Each crate takes these with `[lints] workspace = true`.
[workspace.lints.rust]
unsafe_code = "deny"
unused_must_use = "deny"

[workspace.lints.clippy]
all = { level = "deny", priority = -1 }
pedantic = { level = "warn", priority = -1 }
too_many_lines = "deny"
unwrap_used = "deny"
expect_used = "warn"
let_underscore_must_use = "deny"
cast_possible_truncation = "deny"
cast_sign_loss = "deny"
cast_possible_wrap = "deny"
disallowed_methods = "deny"
undocumented_unsafe_blocks = "deny"
todo = "deny"
unimplemented = "deny"
dbg_macro = "deny"
large_types_passed_by_value = "warn"
fn_params_excessive_bools = "warn"
collapsible_if = "allow"
collapsible_else_if = "allow"
```

```toml
# clippy.toml
too-many-lines-threshold = 70
too-many-arguments-threshold = 5
allow-unwrap-in-tests = true
allow-expect-in-tests = true
disallowed-methods = [
    { path = "std::sync::mpsc::channel", reason = "unbounded: use sync_channel with a stated capacity" },
]
```

## Adoption

A lint comes in with a ratchet. It does not come in with a blanket `#[allow]`.

1. Count the findings of the lint for each crate. Commit the counts.
2. CI fails when a count goes up. A count can only go down.
3. Fix one crate at a time. Start with the lexer and the parser.
4. When the count of a crate is zero, set the lint to `deny` for that crate and delete the count.

The tools:

- `tigerstyle-ratchet.txt` holds the counts, one line for each crate and lint.
- `cargo xtask tigerstyle` runs clippy over the libraries and binaries of the workspace with the full configuration of `xtask/tigerstyle/clippy.toml` and a `-W` for each lint above, and fails when a count is higher than the file. `cargo xtask ci clippy` runs it after clippy, so CI runs it too.
- `cargo xtask tigerstyle --update` writes the current counts to the file. Run it when a count went down, and commit the file in the same change.
- The root `clippy.toml` holds only the settings that every crate meets today. A crate at zero (`oxabl_lexer`, `oxabl_parser`) has its own `clippy.toml` with the full configuration and `[lints] workspace = true` in its `Cargo.toml`.

One `#[allow]` on one item is permitted when the item cannot meet the rule. It needs a comment that gives the reason.
