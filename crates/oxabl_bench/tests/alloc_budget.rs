//! Allocation budgets of one lex and one parse of `resources/lsp_bench.p`.
//!
//! A change that allocates more fails here. A change that allocates much less
//! fails too, so its pull request lowers the budget and the gain is kept. The
//! numbers are for the system allocator on x86-64 Linux.

use oxabl_bench::alloc::{Counting, Usage, measure};
use oxabl_bench::fixtures;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// A measured number may be this much above its budget (noise between
/// toolchains), and the budget is stale when the number is this much below it.
const ABOVE: f64 = 0.02;
const BELOW: f64 = 0.10;

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
        if measured as f64 > limit as f64 * (1.0 + ABOVE) {
            problems.push(format!(
                "{what}: {measured} {name}, over the budget of {limit}"
            ));
        } else if (measured as f64) < limit as f64 * (1.0 - BELOW) {
            problems.push(format!(
                "{what}: {measured} {name}, far below the budget of {limit}: lower the budget"
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
