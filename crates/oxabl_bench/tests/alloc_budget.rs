//! Allocation budgets of one lex and one parse of `resources/lsp_bench.p`.
//!
//! The counts are the same on every run of one toolchain. A change that
//! allocates more fails here. A change that allocates less fails too, so its
//! pull request lowers the budget and the gain is kept. The numbers are for
//! the system allocator on x86-64 Linux with the stable Rust of CI: a new Rust
//! release can move them (a different growth of `Vec`, for example). Then
//! re-measure with `cargo test -p oxabl_bench --test alloc_budget -- --nocapture`
//! and set the budgets to the printed numbers.

use oxabl_bench::alloc::{Counting, Usage, measure};
use oxabl_bench::fixtures;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// The band around a budget, as a share of it, on both sides: room for a small
/// difference between toolchains and no more. A gain larger than this must
/// lower the budget.
const BAND: f64 = 0.005;

struct Budget {
    what: &'static str,
    allocations: u64,
    bytes: u64,
}

fn check(budget: &Budget, usage: Usage) -> Vec<String> {
    let mut problems = Vec::new();
    for (name, measured, limit) in [
        ("allocations", usage.allocations, budget.allocations),
        ("bytes", usage.bytes, budget.bytes),
    ] {
        let what = budget.what;
        if measured as f64 > limit as f64 * (1.0 + BAND) {
            problems.push(format!(
                "{what}: {measured} {name}, over the budget of {limit} by more than {}%",
                BAND * 100.0
            ));
        } else if (measured as f64) < limit as f64 * (1.0 - BAND) {
            problems.push(format!(
                "{what}: {measured} {name}, below the budget of {limit} by more than {}%: \
                 a gain, so lower the budget to {measured} in this pull request",
                BAND * 100.0
            ));
        }
    }
    problems
}

#[test]
fn lex_and_parse_of_lsp_bench_stay_in_budget() {
    let source = fixtures::resource("lsp_bench.p");
    // One run first, so the atom table and the lazy statics are not counted.
    {
        let tokens = tokenize(&source);
        let _ = Parser::new(&tokens, &source).parse_program();
    }
    let (tokens, lex) = measure(|| tokenize(&source));
    let (program, parse) = measure(|| Parser::new(&tokens, &source).parse_program());
    assert_eq!(tokens.len(), 20_311, "the input changed");
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    eprintln!("lex: {lex:?}\nparse: {parse:?}");

    let mut problems = check(
        &Budget {
            what: "lex",
            allocations: LEX_ALLOCATIONS,
            bytes: LEX_BYTES,
        },
        lex,
    );
    problems.extend(check(
        &Budget {
            what: "parse",
            allocations: PARSE_ALLOCATIONS,
            bytes: PARSE_BYTES,
        },
        parse,
    ));
    assert!(problems.is_empty(), "{}", problems.join("\n"));
}

// Measured on 20,311 tokens: 0.008 allocations and 61 bytes for each token in
// the lex, 0.58 allocations and 194 bytes for each token in the parse.
const LEX_ALLOCATIONS: u64 = 169;
const LEX_BYTES: u64 = 1_245_667;
const PARSE_ALLOCATIONS: u64 = 11_877;
const PARSE_BYTES: u64 = 3_943_096;
