//! End-to-end coverage for crediting reads and writes inside
//! recognized-but-unmodelled statement forms (#128).
//!
//! Around thirty ABL statement forms are matched by their leading keyword and
//! then skipped wholesale. They credit no reads and no writes, so the three
//! count-gated rules — `unused-variable` (LINT0002), `block-var-used-outside`
//! (LINT0005) and `assigned-but-never-read` (LINT0006) — used to report
//! variables that a `PUT` or a `SET` plainly touches. The parser now emits
//! `StatementKind::Skipped` carrying the identifiers it passed over, the resolve
//! pass best-effort-resolves them into
//! `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT`, and all three rules treat
//! that mark as a reason not to fire.
//!
//! These tests drive synthetic ABL through the full pipeline (tokenize → parse →
//! analyze → lint), which is the layer a user actually experiences. The
//! coverage-retention fixture is the important one: over-crediting is only "the
//! safe direction" if the rules still report the defects they should, and this
//! file checks that rather than asserting it.

use oxabl_common::{FileId, LintSeverityMap};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0002, LINT0005, LINT0006, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, SymbolFlags, analyze_file};

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

/// Names of the declared variables carrying the unmodelled-touch flag.
fn flagged_variables(source: &str) -> Vec<String> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    sem.symbols
        .iter()
        .filter(|(_, s)| {
            s.flags
                .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
        })
        .map(|(_, s)| s.name.as_ref().to_string())
        .collect()
}

// ---------------------------------------------------------------------------
// Acceptance examples
// ---------------------------------------------------------------------------

/// AE1 — the issue's reproduction. A normal assignment plus a read that only a
/// `PUT` performs looked like a dead store.
#[test]
fn ae1_put_read_is_not_a_dead_store() {
    let src = "\
DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
v-total = 42.
PUT v-total.
";
    let diags = lint_all(src, LintSeverityMap::new());
    assert!(
        of_code(&diags, LINT0006).is_empty(),
        "PUT read must suppress the dead store: {diags:?}"
    );
}

/// AE2 — a variable whose only appearance anywhere is inside a skipped form.
#[test]
fn ae2_put_only_variable_is_not_unused() {
    let src = "\
DEFINE VARIABLE v-only-put AS INTEGER NO-UNDO.
PUT v-only-put.
";
    let diags = lint_all(src, LintSeverityMap::new());
    assert!(
        of_code(&diags, LINT0002).is_empty(),
        "PUT-only variable must not be reported unused: {diags:?}"
    );
}

/// AE3 — the write-side case, and the reason the fix is bidirectional rather
/// than reads-only.
///
/// The in-block *modelled* assignment is load-bearing: `is_hazard` gates on
/// `write_count > 0` before anything else, so a variable whose only write
/// arrives through a skipped statement has `write_count == 0` and the rule
/// already declines. Without the in-block write this fixture would pass before
/// the fix as well as after, and R7 would be untested. See
/// `ae3_fixture_is_non_vacuous` below.
#[test]
fn ae3_set_write_outside_block_is_not_a_block_var_hazard() {
    let diags = lint_all(AE3_SRC, LintSeverityMap::new());
    assert!(
        of_code(&diags, LINT0005).is_empty(),
        "SET write outside the block must suppress the hazard: {diags:?}"
    );
}

const AE3_SRC: &str = "\
DO:
  DEFINE VARIABLE v-choice AS CHARACTER NO-UNDO.
  v-choice = \"a\".
END.
SET v-choice WITH FRAME f-main.
DISPLAY v-choice.
";

/// The non-vacuity check AE3 depends on, expressed as a property of the fixture
/// rather than as a claim in a comment: the variable really is block-hoisted,
/// really is written inside the block, and really is read outside it. Those are
/// the three facts that make `is_hazard` reach its flag test at all, so if this
/// holds and AE3 is silent, the flag is what silenced it.
#[test]
fn ae3_fixture_is_non_vacuous() {
    let tokens = tokenize(AE3_SRC);
    let mut parser = Parser::new(&tokens, AE3_SRC);
    let program = parser.parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, AE3_SRC, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    let (_, sym) = sem
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_ref() == "v-choice")
        .expect("v-choice declared");

    assert!(
        sym.write_count > 0,
        "the modelled in-block write must count"
    );
    assert!(
        sym.flags.contains(SymbolFlags::READ_OUTSIDE_BLOCK),
        "the DISPLAY read must be seen as outside the block"
    );
    assert!(
        !sym.flags.contains(SymbolFlags::WRITE_OUTSIDE_BLOCK),
        "the SET write must remain invisible to the counts — that is the whole \
         premise; if the parser started modelling SET this fixture is obsolete"
    );
    assert!(
        sym.flags
            .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT),
        "the SET must mark the symbol — otherwise AE3 passes for the wrong reason"
    );
}

/// AE4 — an unresolvable harvested name must not reach LINT0001. This is a
/// property of the lookup path: the harvest resolves through
/// `lookup_statement_ident`, which cannot write the `references` side table that
/// LINT0001 reads.
#[test]
fn ae4_unresolvable_harvested_name_stays_silent() {
    let src = "PUT some-name-that-does-not-exist.\n";
    let diags = lint_all(src, LintSeverityMap::new());
    assert!(
        of_code(&diags, LINT0001).is_empty(),
        "harvested names must not reach LINT0001: {diags:?}"
    );
    assert!(
        diags.is_empty(),
        "no diagnostics at all expected: {diags:?}"
    );
}

/// AE5 — the counterweight. Suppression must be caused by an unmodelled
/// statement, not by the mere existence of the mechanism.
#[test]
fn ae5_genuine_dead_store_still_reported() {
    let src = "\
DEFINE VARIABLE v-dead AS INTEGER NO-UNDO.
v-dead = 1.
";
    let diags = lint_all(src, LintSeverityMap::new());
    let dead = of_code(&diags, LINT0006);
    assert_eq!(dead.len(), 1, "expected the dead store: {diags:?}");
    assert!(dead[0].1.contains("v-dead"), "{diags:?}");
}

// ---------------------------------------------------------------------------
// Direction coverage: read-only, write-only, bidirectional forms
// ---------------------------------------------------------------------------

/// `EXPORT` reads its operands.
#[test]
fn read_only_form_export_suppresses_the_dead_store() {
    let src = "\
DEFINE VARIABLE v-line AS CHARACTER NO-UNDO.
v-line = \"x\".
EXPORT v-line.
";
    assert!(
        of_code(&lint_all(src, LintSeverityMap::new()), LINT0006).is_empty(),
        "EXPORT read must suppress LINT0006"
    );
}

/// `GET-KEY-VALUE ... VALUE v` writes its target. Nothing reads it afterwards,
/// so without the flag LINT0002 would report it as never referenced.
#[test]
fn write_only_form_get_key_value_suppresses_unused() {
    let src = "\
DEFINE VARIABLE v-reg AS CHARACTER NO-UNDO.
GET-KEY-VALUE SECTION \"app\" KEY \"path\" VALUE v-reg.
";
    assert!(
        of_code(&lint_all(src, LintSeverityMap::new()), LINT0002).is_empty(),
        "GET-KEY-VALUE write must suppress LINT0002"
    );
}

/// `COPY-LOB` both reads and writes.
#[test]
fn bidirectional_form_copy_lob_suppresses_both_rules() {
    let src = "\
DEFINE VARIABLE v-src AS LONGCHAR NO-UNDO.
DEFINE VARIABLE v-dst AS LONGCHAR NO-UNDO.
COPY-LOB FROM v-src TO v-dst.
";
    let diags = lint_all(src, LintSeverityMap::new());
    assert!(
        of_code(&diags, LINT0002).is_empty() && of_code(&diags, LINT0006).is_empty(),
        "COPY-LOB must suppress both rules: {diags:?}"
    );
}

// ---------------------------------------------------------------------------
// Coverage retention (R14) — the suppression must not blanket a file
// ---------------------------------------------------------------------------

/// A UI-heavy fixture of exactly the shape that worried us: frame-based
/// `ENABLE` / `UPDATE` / `DISPLAY` lists plus an `EDITING:` block, naming most
/// of the file's variables. Three genuine defects are seeded on variables that
/// appear in *no* unmodelled statement, and all three must still be reported.
///
/// This is what turns "over-crediting is the safe direction" from an assertion
/// into something the suite checks. If a future change to the harvest filter
/// starts crediting scope rather than statements, these three go quiet.
const UI_HEAVY_SRC: &str = "\
DEFINE VARIABLE v-cust-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cust-city AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-edit-flag AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-never-used AS INTEGER NO-UNDO.
DEFINE VARIABLE v-dead-store AS INTEGER NO-UNDO.
DEFINE VARIABLE v-shown AS INTEGER NO-UNDO.

v-dead-store = 17.

DO:
  DEFINE VARIABLE v-block-only AS INTEGER NO-UNDO.
  v-block-only = 3.
END.

ENABLE v-cust-name v-cust-city WITH FRAME f-cust.
UPDATE v-cust-name v-cust-city WITH FRAME f-cust EDITING: v-edit-flag = TRUE. END.
DISPLAY v-shown.
MESSAGE v-block-only.
";

#[test]
fn r14_ui_heavy_fixture_still_reports_all_three_seeded_defects() {
    let diags = lint_all(UI_HEAVY_SRC, LintSeverityMap::new());

    let unused = of_code(&diags, LINT0002);
    assert!(
        unused.iter().any(|d| d.1.contains("v-never-used")),
        "LINT0002 lost its seeded defect: {diags:?}"
    );

    let dead = of_code(&diags, LINT0006);
    assert!(
        dead.iter().any(|d| d.1.contains("v-dead-store")),
        "LINT0006 lost its seeded defect: {diags:?}"
    );

    let block = of_code(&diags, LINT0005);
    assert!(
        block.iter().any(|d| d.1.contains("v-block-only")),
        "LINT0005 lost its seeded defect: {diags:?}"
    );
}

/// The other half of R14, checked directly rather than through the rules: the
/// harvest must credit statements, not scope. If every declared variable in a
/// UI-heavy file carried the flag, the rules would be off for that file
/// wholesale and the test above would only be measuring luck.
#[test]
fn r14_not_every_declared_variable_carries_the_flag() {
    let flagged = flagged_variables(UI_HEAVY_SRC);
    for unflagged in ["v-never-used", "v-dead-store", "v-block-only"] {
        assert!(
            !flagged.iter().any(|n| n == unflagged),
            "{unflagged} must not be flagged; flagged = {flagged:?}"
        );
    }
    assert!(
        flagged.iter().any(|n| n == "v-cust-name"),
        "the ENABLE/UPDATE list must be flagged; flagged = {flagged:?}"
    );
}

// ---------------------------------------------------------------------------
// Boundaries
// ---------------------------------------------------------------------------

/// R8: LINT0001's output is unchanged by this work. An undefined name inside a
/// skipped statement was invisible to it before and stays invisible; a real
/// undefined name in a modelled statement is still reported.
#[test]
fn lint0001_output_is_unchanged_around_skipped_statements() {
    let src = "\
DEFINE VARIABLE v-known AS INTEGER NO-UNDO.
PUT v-ghost-in-skip.
v-known = v-ghost-in-expression.
";
    let diags = lint_all(src, LintSeverityMap::new());
    let undefined = of_code(&diags, LINT0001);
    assert_eq!(
        undefined.len(),
        1,
        "expected exactly one LINT0001: {diags:?}"
    );
    assert!(
        undefined[0].1.contains("v-ghost-in-expression"),
        "the reported name must be the one in the modelled statement: {diags:?}"
    );
}

/// The deferred class, pinned. `DISPLAY … WITH FRAME f` parses to a real
/// `Display` node and discards its option tail, so a read living in that tail is
/// still invisible and `v-title-part` is still falsely reported.
///
/// This test asserts the *bug*, on purpose. It is the visible marker for #134,
/// and it will fail — loudly, in the right file — when that work lands.
#[test]
fn deferred_modelled_tail_class_is_still_a_false_positive() {
    let src = "\
DEFINE VARIABLE v-title-part AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shown AS INTEGER NO-UNDO.
v-title-part = \"Orders\".
DISPLAY v-shown WITH FRAME f-main TITLE \"Report: \" + v-title-part.
";
    let diags = lint_all(src, LintSeverityMap::new());
    let dead = of_code(&diags, LINT0006);
    assert!(
        dead.iter().any(|d| d.1.contains("v-title-part")),
        "#134 has landed — delete this test and credit the tail read instead: {diags:?}"
    );
}

/// Severity config is orthogonal to the new suppression: turning a rule off
/// still turns it off.
#[test]
fn severity_off_still_suppresses() {
    let mut severities = LintSeverityMap::new();
    severities.set(LINT0006, None);
    let src = "\
DEFINE VARIABLE v-dead AS INTEGER NO-UNDO.
v-dead = 1.
";
    assert!(
        of_code(&lint_all(src, severities), LINT0006).is_empty(),
        "explicit `off` must still suppress"
    );
}

// ---------------------------------------------------------------------------
// Forms drained out of the suppression
// ---------------------------------------------------------------------------

#[test]
fn delete_object_credits_its_handle_and_suppresses_nothing() {
    // `DELETE OBJECT` used to skip to the statement end and harvest every
    // identifier it passed over, which marked the whole file's named symbols
    // touched-by-something-unmodelled. It is head-parsed now: the operand is a
    // real expression, the handle is credited a read, and no symbol carries the
    // flag — so the count-gated rules judge the file again.
    let src = "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE VARIABLE v-unused AS INTEGER NO-UNDO.
h = ?.
DELETE OBJECT h NO-ERROR.
";
    assert!(
        flagged_variables(src).is_empty(),
        "nothing may be suppressed: {:?}",
        flagged_variables(src)
    );
    let diags = lint_all(src, LintSeverityMap::new());
    // The handle is read, so it is not a dead store...
    assert!(
        !of_code(&diags, LINT0006)
            .iter()
            .any(|(_, m, _)| m.contains("h") && !m.contains("v-unused")),
        "the deleted handle is read: {diags:?}"
    );
    // ...and the untouched variable beside it is judged again, which is the
    // suppression actually draining rather than merely moving.
    assert!(
        of_code(&diags, LINT0002)
            .iter()
            .any(|(_, m, _)| m.contains("v-unused")),
        "the unrelated unused variable must be reported: {diags:?}"
    );
}

#[test]
fn a_deleted_handle_that_is_never_read_is_still_a_dead_store() {
    // Crediting the operand a read must not make every deleted handle look used
    // for the wrong reason: a handle that is assigned and only ever *deleted* is
    // read by the delete, so LINT0006 stays quiet — but one assigned twice with no
    // delete and no read is still reported. The pair is what shows the credit is
    // the delete's, not a blanket exemption.
    let deleted = "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
h = ?.
DELETE OBJECT h.
";
    let never = "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
h = ?.
";
    assert!(
        of_code(&lint_all(deleted, LintSeverityMap::new()), LINT0006).is_empty(),
        "the delete is a read"
    );
    assert!(
        !of_code(&lint_all(never, LintSeverityMap::new()), LINT0006).is_empty(),
        "with no delete, the store is dead"
    );
}

#[test]
fn a_complex_delete_object_operand_credits_the_name_it_reads() {
    // `DELETE OBJECT ttbl:HANDLE.` — the shape that forced the old skip. The
    // attribute access resolves through the buffer, so the temp-table is read.
    let src = "\
DEFINE TEMP-TABLE ttbl NO-UNDO FIELD f AS INTEGER.
DELETE OBJECT ttbl:HANDLE NO-ERROR.
";
    assert!(flagged_variables(src).is_empty());
    let diags = lint_all(src, LintSeverityMap::new());
    assert!(
        !of_code(&diags, LINT0002)
            .iter()
            .any(|(_, m, _)| m.contains("ttbl")),
        "the temp-table is used by the delete: {diags:?}"
    );
}
