//! The formatter lexes a file once for its caller and once for the candidate it
//! prints, and no more. A full lex that comes back costs 7% of `oxabl check`
//! on a file and the same share of `oxabl format`.
//!
//! A test build of `oxabl_lexer` (the `test-counters` feature) counts the lexers
//! built and the bytes they were given. The counters belong to the process, so
//! all cases run in one test, one after the other. All sources are synthetic
//! ABL.

use oxabl_formatter::{format_with_tokens, try_format_ranges, try_format_source};
use oxabl_lexer::{lex_counts, tokenize};
use oxabl_parser::Parser;
use oxabl_style::StyleGuide;

/// Mis-indented, with no blank line, so that the formatter has edits to make and
/// lexes no single line.
fn tight() -> String {
    let unit = "\
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
PROCEDURE doWork:
DEFINE INPUT PARAMETER pi AS INTEGER NO-UNDO.
DO iCount = 1 TO pi:
IF iCount MODULO 2 = 0 THEN
MESSAGE \"even\".
ELSE
MESSAGE \"odd\".
END.
END.
";
    unit.repeat(30)
}

/// The same with blank lines after block openers, which the blank-line pass
/// looks at one line at a time.
fn spaced() -> String {
    tight()
        .replace("\nDO iCount", "\n\nDO iCount")
        .replace("\nEND.", "\n\nEND.")
}

/// `(lexers built, bytes lexed, calls of tokenize)` by `run`.
fn counted<T>(run: impl FnOnce() -> T) -> (T, usize, usize, usize) {
    let (l0, b0, t0) = lex_counts();
    let out = run();
    let (l1, b1, t1) = lex_counts();
    (out, l1 - l0, b1 - b0, t1 - t0)
}

#[test]
fn the_formatter_lexes_the_source_once_and_the_candidate_once() {
    let style = StyleGuide::default_base();

    // A caller that lexes and parses, then formats through `try_format_source`.
    let src = tight();
    let (out, lexes, bytes, tokenizes) = counted(|| try_format_source(&src, &style).unwrap());
    assert_ne!(out, src, "the fixture must have a change to make");
    assert_eq!(
        lexes, 2,
        "try_format_source must lex the source one time and the candidate one time"
    );
    assert_eq!(
        bytes,
        src.len() + out.len(),
        "the two lexes must be of the whole source and the whole candidate"
    );
    assert_eq!(tokenizes, 2, "no pass may collect tokens besides those two");

    // With blank runs, the blank-line pass lexes single lines. It walks the lexer
    // and collects no token vector for them, and together they stay below one
    // more lex of the file.
    let src = spaced();
    let (out, lexes, bytes, tokenizes) = counted(|| try_format_source(&src, &style).unwrap());
    assert!(
        lexes > 2,
        "the fixture must make the blank-line pass lex lines"
    );
    assert_eq!(
        tokenizes, 2,
        "the blank-line pass must not collect a token vector for each line"
    );
    let extra = bytes - (src.len() + out.len());
    assert!(
        extra < src.len(),
        "a third lex of the file ({extra} bytes beyond the source and the candidate, file {})",
        src.len()
    );

    // A caller that holds the tokens passes them: only the candidate is lexed.
    let src = tight();
    let tokens = tokenize(&src);
    let program = Parser::new(&tokens, &src).parse_program();
    let (out, lexes, bytes, _) = counted(|| format_with_tokens(&src, &tokens, &program, &style));
    let out = out.unwrap();
    assert_eq!(lexes, 1, "format_with_tokens must lex the candidate only");
    assert_eq!(bytes, out.len());

    // A range: the source, the whole-file candidate for the layout, and the
    // candidate of the edits.
    let range = 0..src.len() / 2;
    let (edits, lexes, _, _) =
        counted(|| try_format_ranges(&src, &style, std::slice::from_ref(&range)).unwrap());
    assert!(!edits.is_empty(), "the range must have edits");
    assert_eq!(
        lexes, 3,
        "try_format_ranges must lex the source one time and two candidates"
    );
}
