//! End-to-end coverage for the OpenEdge 12 short-form `VAR` statement with a
//! name list or a `CLASS` type, and for `USING`-imported classes whose short
//! name is a keyword.

use oxabl_common::{FileId, LintSeverityMap};
use oxabl_lexer::tokenize;
use oxabl_lint::lint_file;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

/// Full pipeline; returns the diagnostic codes and messages.
fn lint(source: &str) -> Vec<(String, String)> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "parse errors for {source:?}: {:?}",
        program.errors
    );
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema)
        .with_lint_severities(LintSeverityMap::default());
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| (d.code.0.to_string(), d.message))
        .collect()
}

#[test]
fn every_name_of_a_var_list_is_declared() {
    let diags = lint(
        "VAR LOGICAL a, b.\n\
         VAR CLASS Foo.Bar baz.\n\
         a = TRUE.\n\
         b = a.\n\
         MESSAGE b VALID-OBJECT(baz).",
    );
    assert!(
        !diags.iter().any(|(c, _)| c == "LINT0001"),
        "unexpected undefined symbol: {diags:?}"
    );
}

#[test]
fn unused_name_in_a_var_list_is_reported() {
    let diags = lint("VAR INTEGER used, spare.\nMESSAGE used.");
    let unused: Vec<_> = diags.iter().filter(|(c, _)| c == "LINT0002").collect();
    assert_eq!(unused.len(), 1, "{diags:?}");
    assert!(unused[0].1.contains("spare"), "{diags:?}");
}

#[test]
fn keyword_named_imported_class_declares_clean() {
    let diags = lint(
        "USING Progress.Reflect.Method.\n\
         USING Progress.Reflect.Property.\n\
         DEFINE VARIABLE oMethod AS Method NO-UNDO.\n\
         DEFINE VARIABLE oProperty AS Property EXTENT NO-UNDO.\n\
         MESSAGE VALID-OBJECT(oMethod) VALID-OBJECT(oProperty[1]).",
    );
    assert!(diags.is_empty(), "{diags:?}");
}
