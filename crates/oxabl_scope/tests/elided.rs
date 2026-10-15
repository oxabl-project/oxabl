//! Reads around references that the preprocessor replaced with no text
//! (`ScopeOptions::elided`).

use oxabl_scope::{Lock, LockMode, ScopeOptions};

const SCHEMA: &str = "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\n";

/// The lock of each read of `source`, analysed with an elided reference at
/// each `@` (removed from the text).
fn locks(source: &str, fragment: bool) -> Vec<Lock> {
    let mut text = String::new();
    let mut elided = Vec::new();
    for c in source.chars() {
        if c == '@' {
            elided.push(text.len() as u32);
        } else {
            text.push(c);
        }
    }
    let schema = oxabl_schema::test_support::schema_from_df(SCHEMA);
    let tokens = oxabl_lexer::tokenize(&text);
    let program = oxabl_parser::Parser::new(&tokens, &text).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let options = ScopeOptions {
        elided: &elided,
        fragment,
        ..ScopeOptions::default()
    };
    oxabl_scope::analyze_tokens(&program.statements, &text, &tokens, Some(&schema), &options)
        .reads()
        .iter()
        .map(|r| r.lock)
        .collect()
}

const HIDDEN: Lock = Lock::Unknown(oxabl_scope::UnknownReason::UnexpandedInclude);

#[test]
fn an_elided_reference_after_the_record_phrase_hides_the_lock() {
    let src = "FIND FIRST Customer WHERE Customer.CustNum = 1 @.\nFIND FIRST Customer@.\n";
    assert_eq!(locks(src, false), [HIDDEN, HIDDEN]);
}

#[test]
fn a_visible_lock_phrase_or_an_earlier_reference_does_not() {
    let src = "FIND FIRST Customer @NO-LOCK.\nFIND FIRST @Customer.\nFIND FIRST Customer.@\n";
    assert_eq!(
        locks(src, false),
        [
            Lock::Explicit(LockMode::NoLock),
            Lock::Default,
            Lock::Default
        ]
    );
}

#[test]
fn in_a_fragment_a_reference_anywhere_in_the_read_hides_the_lock() {
    let src = "FIND FIRST @Customer.\nFOR EACH Customer WHERE Customer.CustNum = 1@:\nEND.\n";
    assert_eq!(locks(src, true), [HIDDEN, HIDDEN]);
    assert_eq!(locks(src, false), [Lock::Default, HIDDEN]);
}

#[test]
fn a_reference_inside_a_string_does_not() {
    let src = "FIND FIRST Customer WHERE STRING(Customer.CustNum) = \"a@b\".\n";
    assert_eq!(locks(src, false), [Lock::Default]);
}
