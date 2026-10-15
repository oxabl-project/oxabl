//! End-to-end coverage for `assigned-but-never-read` (LINT0006).
//!
//! A value computed into a variable that nothing consumes is a dead store, and
//! the interesting line is the assignment rather than the `DEFINE`. These tests
//! drive synthetic ABL through the full pipeline (tokenize → parse → analyze →
//! lint) so the split from LINT0002 is pinned at the integration level, and the
//! include case goes through real preprocessor expansion so the write genuinely
//! arrives from another file.

use std::path::PathBuf;

use oxabl_common::{FileId, LintSeverityMap, Severity};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0002, LINT0006, lint_file};
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

/// Full pipeline, returning `(code, message, start offset)` per diagnostic.
fn lint_all(source: &str, severities: LintSeverityMap) -> Vec<(String, String, u32)> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(
        program.errors.is_empty(),
        "parse errors for {source:?}: {:?}",
        program.errors
    );
    let schema = Schema::empty();
    let ctx =
        AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_lint_severities(severities);
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| (d.code.0.to_string(), d.message, d.span.span.start))
        .collect()
}

fn of_code<'a>(diags: &'a [(String, String, u32)], code: &str) -> Vec<&'a (String, String, u32)> {
    diags.iter().filter(|(c, _, _)| c == code).collect()
}

#[test]
fn computed_then_discarded_value_reports_at_the_assignment() {
    // The finding this rule exists for: a non-trivial calculation stored into a
    // variable nothing ever consumes. Reported today as a bland "unused
    // variable" pointing at a declaration far from the actual mistake.
    let src = "\
PROCEDURE compute-total:
  DEFINE VARIABLE v-qty AS INTEGER NO-UNDO INITIAL 4.
  DEFINE VARIABLE v-price AS DECIMAL NO-UNDO INITIAL 2.5.
  DEFINE VARIABLE v-total AS DECIMAL NO-UNDO.
  v-total = v-qty * v-price.
END PROCEDURE.
";
    let diags = lint_all(src, LintSeverityMap::new());
    let dead = of_code(&diags, LINT0006);
    assert_eq!(dead.len(), 1, "expected one LINT0006: {diags:?}");
    assert!(dead[0].1.contains("v-total"), "{diags:?}");

    // The span is the assignment, not the DEFINE.
    let write_at = src.find("v-total = v-qty").unwrap() as u32;
    assert_eq!(dead[0].2, write_at, "must point at the write: {diags:?}");

    // And the old rule stays out of it — one symbol, one diagnostic.
    assert!(
        of_code(&diags, LINT0002).is_empty(),
        "LINT0002 must not double-report the dead store: {diags:?}"
    );
}

#[test]
fn reads_that_live_only_in_comments_still_report() {
    // Comments never resolve, so a variable whose only "uses" are commented out
    // is genuinely write-only and must still be reported. This is the shape that
    // shows up when someone disables a block of code and forgets the producer.
    let src = "\
DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
v-total = 42.
/* MESSAGE v-total. */
// MESSAGE v-total.
";
    let diags = lint_all(src, LintSeverityMap::new());
    let dead = of_code(&diags, LINT0006);
    assert_eq!(dead.len(), 1, "expected one LINT0006: {diags:?}");
    assert_eq!(
        dead[0].2,
        src.find("v-total = 42").unwrap() as u32,
        "{diags:?}"
    );
}

#[test]
fn configuring_the_rule_off_silences_only_that_rule() {
    // Same source produces both findings: a dead store and a stray declaration.
    let src = "\
DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
DEFINE VARIABLE v-spare AS INTEGER NO-UNDO.
v-total = 1.
";
    let baseline = lint_all(src, LintSeverityMap::new());
    assert_eq!(of_code(&baseline, LINT0006).len(), 1, "{baseline:?}");
    assert_eq!(of_code(&baseline, LINT0002).len(), 1, "{baseline:?}");

    let mut off = LintSeverityMap::new();
    off.set(LINT0006, None);
    let configured = lint_all(src, off);
    assert!(
        of_code(&configured, LINT0006).is_empty(),
        "LINT0006 configured off must vanish: {configured:?}"
    );
    assert_eq!(
        of_code(&configured, LINT0002).len(),
        1,
        "its sibling must survive: {configured:?}"
    );

    // Severity is remappable too, not just on/off.
    let mut as_error = LintSeverityMap::new();
    as_error.set(LINT0006, Some(Severity::Error));
    let tokens = tokenize(src);
    let mut parser = Parser::new(&tokens, src);
    let program = parser.parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, src, &schema).with_lint_severities(as_error);
    let sem = analyze_file(&program.statements, &ctx);
    let escalated = lint_file(&program.statements, &sem, &ctx);
    let dead: Vec<_> = escalated.iter().filter(|d| d.code.0 == LINT0006).collect();
    assert_eq!(dead.len(), 1, "{escalated:?}");
    assert_eq!(dead[0].severity, Severity::Error, "{escalated:?}");
}

#[test]
fn a_write_arriving_through_an_expanded_include_still_reports() {
    // The write is not always local to the routine's visible text. Stage a real
    // include so the assignment genuinely arrives through expansion, then lint
    // the expanded text — which is what the CLI does.
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/inc/set-total.i"), "v-total = 99.\n");
    let main_src = "\
DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
{set-total.i}
";
    let include_paths = vec![PathBuf::from("/inc")];
    let preprocessor = Preprocessor::new(&fs, &include_paths);
    let expanded = preprocessor
        .process(FileId::new(1), main_src)
        .expect("preprocessing should succeed");
    assert!(
        expanded
            .diagnostics
            .iter()
            .all(|d| !d.message.contains("PREPROC007")),
        "include must resolve, else this test proves nothing: {:?}",
        expanded.diagnostics
    );
    let text = expanded.to_text();
    assert!(
        text.contains("v-total = 99"),
        "expansion should have inlined the write, got: {text:?}"
    );

    let diags = lint_all(&text, LintSeverityMap::new());
    let dead = of_code(&diags, LINT0006);
    assert_eq!(
        dead.len(),
        1,
        "a write from an include is still a write: {diags:?}"
    );
    assert!(dead[0].1.contains("v-total"), "{diags:?}");
    assert!(
        of_code(&diags, LINT0002).is_empty(),
        "LINT0002 must stay silent: {diags:?}"
    );
}

/// `(line, message)` of each LINT0006 finding, with line numbers 1-based.
fn dead_store_lines(src: &str) -> Vec<(usize, String)> {
    let diags = lint_all(src, LintSeverityMap::new());
    of_code(&diags, LINT0006)
        .into_iter()
        .map(|(_, msg, at)| (src[..*at as usize].matches('\n').count() + 1, msg.clone()))
        .collect()
}

#[test]
fn message_update_target_reports_at_its_own_line() {
    let src = "\
DEFINE VARIABLE v-answer AS LOGICAL NO-UNDO.
MESSAGE \"Proceed?\" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-answer.
";
    let found = dead_store_lines(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].0, 2, "{found:?}");
    assert!(found[0].1.contains("v-answer"), "{found:?}");
}

#[test]
fn persistent_and_async_run_handles_report_at_the_run() {
    let src = "\
DEFINE VARIABLE h-per AS HANDLE NO-UNDO.
DEFINE VARIABLE h-async AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET h-per.
RUN slow.p ASYNCHRONOUS SET h-async.
";
    let found = dead_store_lines(src);
    let lines: Vec<usize> = found.iter().map(|f| f.0).collect();
    assert_eq!(lines, vec![3, 4], "{found:?}");
}

#[test]
fn buffer_compare_result_reports_at_the_statement() {
    let src = "\
DEFINE VARIABLE v-res AS CHARACTER NO-UNDO.
DEFINE BUFFER b1 FOR customer.
DEFINE BUFFER b2 FOR customer.
BUFFER-COMPARE b1 TO b2 SAVE RESULT IN v-res.
";
    let found = dead_store_lines(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].0, 4, "{found:?}");
}

#[test]
fn created_handle_reports_at_the_create() {
    let src = "\
DEFINE VARIABLE h-qry AS HANDLE NO-UNDO.
CREATE QUERY h-qry.
";
    let found = dead_store_lines(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].0, 2, "{found:?}");
}

#[test]
fn a_write_inside_an_on_trigger_reports_inside_the_trigger() {
    let src = "\
DEFINE VARIABLE v-n AS INTEGER NO-UNDO.
ON CHOOSE OF btn-ok ANYWHERE DO:
  v-n = 1.
END.
";
    let found = dead_store_lines(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].0, 3, "{found:?}");
}

#[test]
fn a_statement_position_target_resolves_in_its_own_routine() {
    // The dead `v-x` is in `a`. The `v-x` of `b` is read. Resolving by spelling
    // alone would report the write in `b`.
    let src = "\
PROCEDURE b:
  DEFINE VARIABLE v-x AS LOGICAL NO-UNDO.
  MESSAGE \"?\" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-x.
  IF v-x THEN RETURN.
END PROCEDURE.
PROCEDURE a:
  DEFINE VARIABLE v-x AS LOGICAL NO-UNDO.
  MESSAGE \"?\" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-x.
END PROCEDURE.
";
    let found = dead_store_lines(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].0, 8, "{found:?}");
}

#[test]
fn a_write_inside_a_skipped_statement_is_unknown_not_dead() {
    // `PROMPT-FOR` and `GET-KEY-VALUE` are not modelled: the rule cannot tell
    // what they do to `v-x`, so it stays silent.
    for form in [
        "PROMPT-FOR v-x.",
        "GET-KEY-VALUE SECTION \"s\" KEY \"k\" VALUE v-x.",
    ] {
        let src = format!("DEFINE VARIABLE v-x AS INTEGER NO-UNDO.\nv-x = 1.\n{form}\n");
        let found = dead_store_lines(&src);
        assert!(found.is_empty(), "{form}: {found:?}");
    }
}

#[test]
fn an_import_target_is_a_write() {
    let src = "DEFINE VARIABLE v-x AS CHARACTER NO-UNDO.\nIMPORT UNFORMATTED v-x NO-ERROR.\n";
    let found = dead_store_lines(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].0, 2, "{found:?}");

    let src = "DEFINE VARIABLE v-x AS CHARACTER NO-UNDO.\nIMPORT STREAM s ^ v-x.\nMESSAGE v-x.\n";
    assert!(dead_store_lines(src).is_empty());
}

#[test]
fn update_reads_its_targets_and_set_only_writes_them() {
    let src = "DEFINE VARIABLE v-x AS INTEGER NO-UNDO.\nv-x = 1.\nUPDATE v-x WITH FRAME f.\n";
    assert!(dead_store_lines(src).is_empty());

    let src = "DEFINE VARIABLE v-x AS INTEGER NO-UNDO.\nSET v-x WITH FRAME f.\n";
    let found = dead_store_lines(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].0, 2, "{found:?}");
}
