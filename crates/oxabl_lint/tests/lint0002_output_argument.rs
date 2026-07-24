//! End-to-end LINT0002 coverage for the OUTPUT-argument false positive.
//!
//! A variable whose only appearance is as a write-back (`OUTPUT` /
//! `INPUT-OUTPUT`) argument to a `RUN` is genuinely used — the callee writes
//! into it — so `unused-variable` must stay silent. These tests drive
//! synthetic ABL through the full pipeline (tokenize → parse → analyze →
//! lint) so the fix is pinned at the integration level, not just per-pass.

use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0002, unused_variable};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

fn lint0002(source: &str) -> Vec<String> {
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
    unused_variable::run(&program.statements, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0002)
        .map(|d| d.message)
        .collect()
}

/// Same pipeline, but keeps each diagnostic's start offset so a test can tell
/// two same-named bindings apart.
fn lint0002_spans(source: &str) -> Vec<(String, u32)> {
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
    unused_variable::run(&program.statements, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0002)
        .map(|d| (d.message, d.span.span.start))
        .collect()
}

#[test]
fn output_argument_only_variable_is_not_unused() {
    // The reported shape: the callee's signature requires an out-param this
    // call site discards.
    let src = "\
DEFINE VARIABLE v-error AS CHARACTER NO-UNDO.
RUN calc.p (INPUT \"ctx\", OUTPUT v-error).
";
    let diags = lint0002(src);
    assert!(
        diags.is_empty(),
        "OUTPUT-argument-only variable must not be flagged unused: {diags:?}"
    );
}

#[test]
fn input_output_argument_only_variable_is_not_unused() {
    let src = "\
DEFINE VARIABLE v-count AS INTEGER NO-UNDO.
RUN calc.p (INPUT-OUTPUT v-count).
";
    let diags = lint0002(src);
    assert!(
        diags.is_empty(),
        "INPUT-OUTPUT-argument-only variable must not be flagged unused: {diags:?}"
    );
}

#[test]
fn truly_unused_neighbour_still_warns() {
    // Discrimination: the skip is targeted at the OUTPUT-argument variable,
    // not at every variable in the file.
    let src = "\
DEFINE VARIABLE v-error AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-spare AS CHARACTER NO-UNDO.
RUN calc.p (INPUT \"ctx\", OUTPUT v-error).
";
    let diags = lint0002(src);
    assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
    assert!(
        diags[0].contains("v-spare"),
        "the truly-unused variable must be the one reported: {diags:?}"
    );
}

#[test]
fn same_name_in_sibling_scope_is_not_exempted() {
    // The skip is keyed on the resolved SymbolId, not the name. Two distinct
    // `v-tmp` bindings in sibling procedures must be judged separately: only
    // the one actually passed as an OUTPUT argument is exempt. A name-keyed
    // mis-attribution would silently swallow the second diagnostic here,
    // which the distinct-name discrimination test above cannot catch.
    let src = "\
PROCEDURE a:
  DEFINE VARIABLE v-tmp AS CHARACTER NO-UNDO.
  RUN calc.p (OUTPUT v-tmp).
END PROCEDURE.
PROCEDURE b:
  DEFINE VARIABLE v-tmp AS CHARACTER NO-UNDO.
END PROCEDURE.
";
    let diags = lint0002_spans(src);
    assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
    let (msg, start) = &diags[0];
    assert!(msg.contains("v-tmp"), "{diags:?}");
    let proc_b = src.find("PROCEDURE b:").unwrap() as u32;
    assert!(
        *start > proc_b,
        "the diagnostic must point at procedure b's binding (offset > {proc_b}), got {start}"
    );
}

#[test]
fn locally_assigned_never_read_variable_still_warns() {
    // Regression through the full pipeline: `write_count` is not the signal,
    // so a plain local assignment keeps warning.
    let src = "\
DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
v-total = 1.
";
    let diags = lint0002(src);
    assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
    assert!(
        diags[0].contains("v-total"),
        "assigned-but-never-read variable must still warn: {diags:?}"
    );
}
