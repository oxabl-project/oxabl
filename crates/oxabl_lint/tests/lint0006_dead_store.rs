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
