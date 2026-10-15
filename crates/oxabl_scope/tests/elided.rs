//! Reads around references that the preprocessor replaced with no text
//! (`ScopeOptions::elided`).

use oxabl_scope::{Lock, LockMode, ScopeOptions, UnknownReason, Written};

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

/// `written` of each read of `source`, analysed with an include that did not
/// arrive at each `#` (removed from the text).
fn written(source: &str) -> Vec<Written> {
    let mut text = String::new();
    let mut includes = Vec::new();
    for c in source.chars() {
        if c == '#' {
            includes.push(text.len() as u32);
        } else {
            text.push(c);
        }
    }
    let schema = oxabl_schema::test_support::schema_from_df(SCHEMA);
    let tokens = oxabl_lexer::tokenize(&text);
    let program = oxabl_parser::Parser::new(&tokens, &text).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let options = ScopeOptions {
        elided: &includes,
        elided_includes: &includes,
        ..ScopeOptions::default()
    };
    oxabl_scope::analyze_tokens(&program.statements, &text, &tokens, Some(&schema), &options)
        .reads()
        .iter()
        .map(|r| r.written)
        .collect()
}

const MAY_WRITE: Written = Written::Unknown(UnknownReason::UnexpandedInclude);

#[test]
fn an_include_after_the_read_in_its_scope_may_write_the_record() {
    let src = "FIND FIRST Customer NO-LOCK.\n#DISPLAY \"x\".\n";
    assert_eq!(written(src), [MAY_WRITE]);
    assert_eq!(written(&src.replace('#', "")), [Written::No]);
}

#[test]
fn an_include_before_the_read_or_outside_its_scope_does_not() {
    let before = "#FIND FIRST Customer NO-LOCK.\n";
    assert_eq!(written(before), [Written::No]);
    let outside = "DO FOR Customer:\n  FIND FIRST Customer NO-LOCK.\nEND.\n#DISPLAY \"x\".\n";
    assert_eq!(written(outside), [Written::No]);
}

#[test]
fn an_include_before_the_read_in_an_iterating_block_of_the_scope_may_write() {
    let src = "REPEAT:\n  #FIND NEXT Customer NO-LOCK.\nEND.\n";
    assert_eq!(written(src), [MAY_WRITE]);
    let flat = "DO:\n  #FIND FIRST Customer NO-LOCK.\nEND.\n";
    assert_eq!(written(flat), [Written::No]);
}

#[test]
fn a_visible_write_after_the_read_stays_a_write() {
    let src = "FIND FIRST Customer.\n#Customer.CustNum = 1.\n";
    assert_eq!(written(src), [Written::Yes]);
}
