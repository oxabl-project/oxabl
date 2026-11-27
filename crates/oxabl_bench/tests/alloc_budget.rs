//! Allocation budgets of one lex and one parse of `resources/lsp_bench.p`, and
//! of one parse of the schema fixture `sp2k.df`. The parse also has a budget
//! for the bytes that the program it returns keeps, with the capacity that its
//! vectors do not use.
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
    /// The bytes still live after the call, for a call that returns what it
    /// made.
    retained: Option<u64>,
}

fn check(budget: &Budget, usage: Usage) -> Vec<String> {
    let mut problems = Vec::new();
    let retained = u64::try_from(usage.retained).unwrap_or(0);
    for (name, measured, limit) in [
        ("allocations", usage.allocations, Some(budget.allocations)),
        ("bytes", usage.bytes, Some(budget.bytes)),
        ("retained bytes", retained, budget.retained),
    ] {
        let Some(limit) = limit else {
            continue;
        };
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
            retained: None,
        },
        lex,
    );
    problems.extend(check(
        &Budget {
            what: "parse",
            allocations: PARSE_ALLOCATIONS,
            bytes: PARSE_BYTES,
            retained: Some(PARSE_RETAINED),
        },
        parse,
    ));
    assert!(problems.is_empty(), "{}", problems.join("\n"));
    parse_of_sp2k_df_stays_in_budget();
}

/// Called from the test above, not a test of its own: the counter is global,
/// so two tests that run in parallel would count each other's allocations.
fn parse_of_sp2k_df_stays_in_budget() {
    let text = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../oxabl_schema/fixtures/sp2k.df"),
    )
    .unwrap();
    let file = oxabl_common::FileId::new(1);
    // One run first, so the atom table and the lazy statics are not counted.
    let _ = oxabl_schema::parse_df(&text, file);
    let (outcome, usage) = measure(|| oxabl_schema::parse_df(&text, file));
    assert_eq!(outcome.tables.len(), 25, "the input changed");
    eprintln!("parse_df: {usage:?}");
    let problems = check(
        &Budget {
            what: "parse_df",
            allocations: DF_ALLOCATIONS,
            bytes: DF_BYTES,
            retained: None,
        },
        usage,
    );
    assert!(problems.is_empty(), "{}", problems.join("\n"));
}

// Measured on 20,311 tokens: one allocation (the token vector) and 61 bytes for
// each token in the lex, 0.04 allocations and 92 bytes for each token in the
// parse. The program keeps 58 bytes for each token, 14 of them capacity that
// its vectors do not use.
const LEX_ALLOCATIONS: u64 = 1;
const LEX_BYTES: u64 = 1_241_136;
const PARSE_ALLOCATIONS: u64 = 894;
const PARSE_BYTES: u64 = 1_877_634;
const PARSE_RETAINED: u64 = 1_186_066;

// The parse of `sp2k.df` (the strings the model keeps, and no string for a
// keyword or an attribute word).
const DF_ALLOCATIONS: u64 = 1_848;
const DF_BYTES: u64 = 358_902;
