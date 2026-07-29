//! End-to-end LINT0002 coverage for the table-parameter false positive.
//!
//! `DEFINE INPUT PARAMETER TABLE FOR tt` declares a symbol in a namespace
//! nothing can reference: every use of `tt` resolves to the `DEFINE TEMP-TABLE`
//! instead, so the parameter's own `read_count` is permanently zero and
//! `unused-variable` fired no matter how heavily the table was used. These tests
//! drive synthetic ABL through the full pipeline (tokenize → parse → analyze →
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

#[test]
fn table_parameter_whose_table_is_used_is_silent() {
    // The reported shape: the table is read in the procedure body, but every
    // one of those reads credits the temp-table symbol, not the parameter.
    let src = "\
DEFINE TEMP-TABLE ttItem NO-UNDO
  FIELD ItemCode AS CHARACTER.

PROCEDURE emit-items:
  DEFINE INPUT PARAMETER TABLE FOR ttItem.
  FOR EACH ttItem:
    MESSAGE ttItem.ItemCode.
  END.
END PROCEDURE.
";
    let diags = lint0002(src);
    assert!(
        diags.is_empty(),
        "used TABLE FOR parameter must not warn: {diags:?}"
    );
}

#[test]
fn table_parameter_used_only_through_a_field_reference_is_silent() {
    // Narrower shape: no FOR EACH / FIND at all, so the only thing crediting
    // the table is the field-access qualifier.
    let src = "\
DEFINE TEMP-TABLE ttItem NO-UNDO
  FIELD ItemCode AS CHARACTER.

PROCEDURE emit-code:
  DEFINE INPUT PARAMETER TABLE FOR ttItem.
  MESSAGE ttItem.ItemCode.
END PROCEDURE.
";
    let diags = lint0002(src);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
}

#[test]
fn unused_variable_beside_a_used_table_parameter_still_warns() {
    // Discrimination: the redirect is per-symbol, so a genuinely unused
    // neighbour is still reported — and it is the only thing reported.
    let src = "\
DEFINE TEMP-TABLE ttItem NO-UNDO
  FIELD ItemCode AS CHARACTER.

PROCEDURE emit-items:
  DEFINE INPUT PARAMETER TABLE FOR ttItem.
  DEFINE VARIABLE v-spare AS INTEGER NO-UNDO.
  FOR EACH ttItem:
    MESSAGE ttItem.ItemCode.
  END.
END PROCEDURE.
";
    let diags = lint0002(src);
    assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
    assert!(diags[0].contains("v-spare"), "{diags:?}");
}

#[test]
fn table_parameter_whose_table_is_never_touched_still_warns() {
    // The preserved true positive: the fix redirects the read-count question to
    // the right symbol, it does not blanket-exempt table parameters. A
    // procedure that declares a table parameter and never touches the table is
    // still worth reporting.
    let src = "\
DEFINE TEMP-TABLE ttItem NO-UNDO
  FIELD ItemCode AS CHARACTER.

PROCEDURE ignore-items:
  DEFINE INPUT PARAMETER TABLE FOR ttItem.
  MESSAGE \"nothing to do\".
END PROCEDURE.
";
    let diags = lint0002(src);
    assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
    assert!(diags[0].contains("ttItem"), "{diags:?}");
}

// ---------------------------------------------------------------------------
// Table used only through a form that names it without reading a field (#130)
// ---------------------------------------------------------------------------
//
// Each of these five forms names its table and nothing else: no FOR EACH, no
// field access, nothing the expression walk can see. Before #130 the backing
// temp-table's `read_count` stayed at zero, so the redirect above asked the
// right symbol and still got the wrong answer.

/// Wrap `body` in a procedure that takes `ttItem` as a table parameter, so the
/// only thing that can credit the table is the statement under test.
fn only_use_of_the_table(body: &str) -> String {
    format!(
        "\
DEFINE TEMP-TABLE ttItem NO-UNDO
  FIELD ItemCode AS CHARACTER.

PROCEDURE handle-items:
  DEFINE INPUT PARAMETER TABLE FOR ttItem.
{body}
END PROCEDURE.
"
    )
}

#[track_caller]
fn assert_silent(body: &str) {
    let src = only_use_of_the_table(body);
    let diags = lint0002(&src);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics for {body:?}: {diags:?}"
    );
}

#[test]
fn table_used_only_by_a_buffer_definition_is_silent() {
    assert_silent("  DEFINE BUFFER bItem FOR ttItem.");
}

/// R2: the explicit `FOR TEMP-TABLE` spelling is a different `BufferTarget`
/// variant and would be an easy one to fix only halfway.
#[test]
fn table_used_only_by_a_temp_table_buffer_definition_is_silent() {
    assert_silent("  DEFINE BUFFER bItem FOR TEMP-TABLE ttItem.");
}

#[test]
fn table_used_only_by_a_buffer_parameter_is_silent() {
    assert_silent("  DEFINE PARAMETER BUFFER bItem FOR ttItem.");
}

#[test]
fn table_used_only_by_empty_temp_table_is_silent() {
    assert_silent("  EMPTY TEMP-TABLE ttItem NO-ERROR.");
}

#[test]
fn table_used_only_by_a_query_definition_is_silent() {
    assert_silent("  DEFINE QUERY q-item FOR ttItem.");
}

#[test]
fn table_used_only_by_an_open_query_is_silent() {
    assert_silent("  OPEN QUERY q-item FOR EACH ttItem.");
}

/// Every backing table a query names is credited, not just the first — a join
/// over two temp-tables reads both.
#[test]
fn every_table_a_query_names_is_credited() {
    let src = "\
DEFINE TEMP-TABLE ttItem NO-UNDO
  FIELD ItemCode AS CHARACTER.
DEFINE TEMP-TABLE ttOrder NO-UNDO
  FIELD OrderNum AS INTEGER.

PROCEDURE join-items:
  DEFINE INPUT PARAMETER TABLE FOR ttItem.
  DEFINE INPUT PARAMETER TABLE FOR ttOrder.
  OPEN QUERY q-join FOR EACH ttItem, EACH ttOrder.
END PROCEDURE.
";
    let diags = lint0002(src);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
}

/// Discrimination, one per implementation shape: crediting the table must not
/// turn into a procedure-wide amnesty. The buffer forms resolve their target
/// directly; the query and empty-table forms go through the marked-skipped
/// harvest, which is the broader of the two paths and the one where an
/// over-eager suppression would hide.
#[test]
fn unused_scalar_beside_a_credited_table_still_warns() {
    for body in [
        "  DEFINE BUFFER bItem FOR ttItem.\n  DEFINE VARIABLE v-spare AS INTEGER NO-UNDO.",
        "  EMPTY TEMP-TABLE ttItem.\n  DEFINE VARIABLE v-spare AS INTEGER NO-UNDO.",
        "  OPEN QUERY q-item FOR EACH ttItem.\n  DEFINE VARIABLE v-spare AS INTEGER NO-UNDO.",
    ] {
        let src = only_use_of_the_table(body);
        let diags = lint0002(&src);
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic for {body:?}: {diags:?}"
        );
        assert!(diags[0].contains("v-spare"), "{diags:?}");
    }
}
