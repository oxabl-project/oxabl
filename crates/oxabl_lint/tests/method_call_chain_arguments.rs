//! End-to-end coverage for reads made by the arguments of a `:`-qualified
//! method call, wherever the call sits in a member-access chain.
//!
//! An argument is a read whether the call ends the chain (`a:f(x)`) or is
//! followed by more access (`a:f(x):g`, `a:f(x):g()`). When it is not credited,
//! the variable is reported as unused (LINT0002) or as a dead store (LINT0006).

use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0002, LINT0006, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

fn findings(source: &str) -> Vec<(String, String)> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(
        program.errors.is_empty(),
        "parse errors for {source:?}: {:?}",
        program.errors
    );
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0002 || d.code.0 == LINT0006)
        .map(|d| (d.code.0.to_string(), d.message))
        .collect()
}

fn assert_clean(source: &str) {
    let found = findings(source);
    assert!(found.is_empty(), "expected no findings: {found:?}");
}

#[test]
fn argument_of_call_ending_the_chain_is_read() {
    assert_clean(
        "\
DEFINE VARIABLE oSvc AS Progress.Lang.Object NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
cName = \"x\".
MESSAGE oSvc:Lookup(cName).
",
    );
}

#[test]
fn argument_of_call_followed_by_property_is_read() {
    assert_clean(
        "\
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
cName = \"Progress.Lang.Object\".
cType = Progress.Lang.Class:GetClass(cName):TypeName.
MESSAGE cType.
",
    );
}

#[test]
fn argument_of_call_followed_by_call_is_read() {
    assert_clean(
        "\
DEFINE VARIABLE oSvc AS Progress.Lang.Object NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE oRes AS Progress.Lang.Object NO-UNDO.
cName = \"x\".
oRes = oSvc:Lookup(cName):Accept().
MESSAGE oRes.
",
    );
}

#[test]
fn arguments_in_the_middle_of_a_long_chain_are_read() {
    assert_clean(
        "\
CLASS Chains:
    METHOD PUBLIC CHARACTER MethodName(INPUT poObj AS Progress.Lang.Object, INPUT pcName AS CHARACTER, INPUT plFlag AS LOGICAL):
        RETURN poObj:GetClass():GetMethod(pcName, plFlag):Name.
    END METHOD.
END CLASS.
",
    );
}

#[test]
fn unused_argument_next_to_a_chain_is_still_reported() {
    let found = findings(
        "\
DEFINE VARIABLE oSvc AS Progress.Lang.Object NO-UNDO.
DEFINE VARIABLE cUsed AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUnused AS CHARACTER NO-UNDO.
MESSAGE oSvc:Lookup(cUsed):Name.
",
    );
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains("cUnused"), "{found:?}");
}
