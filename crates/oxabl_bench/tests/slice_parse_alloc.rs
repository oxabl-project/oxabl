//! A parse of a slice of the tokens of a file allocates what the parse of
//! that text alone allocates.
//!
//! The scan for `NEW SHARED` producers parses each `DEFINE NEW SHARED`
//! statement of a file from a slice of the tokens of the file, with the whole
//! text as the source. A parser that sizes a buffer from the length of the
//! source then pays for the whole file once for each statement, and this test
//! fails on that. All input is synthetic.

use oxabl_bench::alloc::{Counting, measure};
use oxabl_lexer::{Kind, Token, tokenize};
use oxabl_parser::Parser;

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// The tokens from which the parser sizes its pools before it parses.
const PRESIZED_FROM_TOKENS: usize = 64;

/// One statement with more tokens than [`PRESIZED_FROM_TOKENS`].
fn wide_definition() -> String {
    let mut text = String::from("DEFINE NEW SHARED TEMP-TABLE ttWide NO-UNDO\n");
    for n in 0..24 {
        text.push_str(&format!("    FIELD f{n} AS INTEGER\n"));
    }
    text.push_str("    INDEX main IS PRIMARY f0.\n");
    text
}

/// Whether the statement at the start of `tokens` parses.
fn parses(tokens: &[Token], source: &str) -> bool {
    Parser::new(tokens, source).parse_statement().is_ok()
}

#[test]
fn a_parse_of_a_token_slice_does_not_pay_for_the_rest_of_the_text() {
    let alone = wide_definition();
    let whole = format!("{alone}{}", "MESSAGE \"more\".\n".repeat(20_000));
    let alone_tokens = tokenize(&alone);
    let mut whole_tokens = tokenize(&whole);
    let end = alone_tokens.len() - 1;
    assert!(end >= PRESIZED_FROM_TOKENS, "{end} tokens");
    assert_eq!(alone_tokens[end].kind, Kind::Eof);
    // The first token after the statement stands in for the end of the text,
    // as in the scan for producers.
    assert_eq!(whole_tokens[end].start, alone_tokens[end].start);
    whole_tokens[end].kind = Kind::Eof;
    let slice = &whole_tokens[..=end];

    // One run first, so the lazy statics are not counted.
    assert!(parses(&alone_tokens, &alone));
    assert!(parses(slice, &whole));
    let (alone_parses, of_alone) = measure(|| parses(&alone_tokens, &alone));
    let (slice_parses, of_slice) = measure(|| parses(slice, &whole));
    assert!(alone_parses && slice_parses);
    eprintln!("alone: {of_alone:?}\nslice: {of_slice:?}");
    assert_eq!(
        (of_slice.allocations, of_slice.bytes),
        (of_alone.allocations, of_alone.bytes),
        "the parse of the statement in a text of {} bytes, and alone",
        whole.len()
    );
}
