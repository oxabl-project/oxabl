//! Reads inside the skipped tail of a *modelled* statement.
//!
//! `DISPLAY … WITH FRAME f TITLE x` parses to a real `Display` node and skips
//! its option tail. The identifiers of that tail ride on
//! `Statement::skipped_tail`, and the resolve pass marks the symbols they name
//! so the count-gated rules (LINT0002, LINT0006) decline to judge them. Each
//! test pairs a tail form with a variable only that tail reads.

use oxabl_common::{FileId, LintSeverityMap};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0002, LINT0003, LINT0006, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

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
        .with_lint_severities(LintSeverityMap::new());
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| (d.code.0.to_string(), d.message))
        .collect()
}

fn flags(diags: &[(String, String)], code: &str, name: &str) -> bool {
    diags.iter().any(|(c, m)| c == code && m.contains(name))
}

/// `v` is assigned and then read only by `tail`; neither rule may report it.
fn assert_tail_read_is_credited(tail: &str) {
    let src = format!(
        "DEFINE VARIABLE v-tail AS CHARACTER NO-UNDO.\nDEFINE VARIABLE v-other AS INTEGER NO-UNDO.\nv-tail = \"x\".\n{tail}\n"
    );
    let diags = lint(&src);
    assert!(
        !flags(&diags, LINT0006, "v-tail"),
        "dead store reported for {tail:?}: {diags:?}"
    );
    assert!(
        !flags(&diags, LINT0002, "v-tail"),
        "unused variable reported for {tail:?}: {diags:?}"
    );
}

#[test]
fn display_with_frame_title() {
    assert_tail_read_is_credited("DISPLAY v-other WITH FRAME f-main TITLE \"R: \" + v-tail.");
}

#[test]
fn display_frame_before_with() {
    assert_tail_read_is_credited("DISPLAY v-other FRAME f-main TITLE \"R: \" + v-tail.");
}

#[test]
fn return_error_with_unparsed_value() {
    assert_tail_read_is_credited(
        "PROCEDURE p:\n  RETURN ERROR v-tail NO-ERROR NO-ERROR.\nEND PROCEDURE.",
    );
}

#[test]
fn run_trailing_content() {
    assert_tail_read_is_credited("RUN p.p NO-ERROR v-tail ).");
}

#[test]
fn stream_io_value_clause() {
    assert_tail_read_is_credited("OUTPUT TO \"out.txt\" PAGE-SIZE VALUE(v-tail).");
}

#[test]
fn define_variable_view_as_tail() {
    assert_tail_read_is_credited(
        "DEFINE VARIABLE v-fill AS CHARACTER NO-UNDO VIEW-AS FILL-IN SIZE 20 BY 1 LABEL v-tail.",
    );
}

#[test]
fn define_frame_phrase() {
    assert_tail_read_is_credited("DEFINE FRAME f-x v-other WITH TITLE v-tail.");
}

#[test]
fn assign_frame_field_list() {
    assert_tail_read_is_credited("ASSIGN FRAME f-x v-tail.");
}

#[test]
fn assign_pair_missing_equals_tail() {
    assert_tail_read_is_credited("ASSIGN v-other = 1 v-tail.");
}

/// The tail only silences. A name that resolves to nothing, or a variable the
/// tail does not mention, still gets its findings.
#[test]
fn a_tail_does_not_hide_an_unrelated_dead_store() {
    let src = "\
DEFINE VARIABLE v-dead AS INTEGER NO-UNDO.
DEFINE VARIABLE v-shown AS INTEGER NO-UNDO.
v-dead = 1.
DISPLAY v-shown WITH FRAME f-main TITLE \"x\".
";
    let diags = lint(src);
    assert!(flags(&diags, LINT0006, "v-dead"), "{diags:?}");
}

#[test]
fn an_unresolvable_tail_name_raises_no_undefined_symbol() {
    let src = "\
DEFINE VARIABLE v-shown AS INTEGER NO-UNDO.
DISPLAY v-shown WITH FRAME f-main TITLE \"x\" + v-ghost + tbl.fld.
";
    let diags = lint(src);
    assert!(!flags(&diags, LINT0001, "v-ghost"), "{diags:?}");
    assert!(diags.iter().all(|(c, _)| c != LINT0003), "{diags:?}");
}
