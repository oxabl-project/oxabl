//! Reads and writes credited by the handle-lifetime statements: dynamic-object
//! `CREATE`, widget pools, `DELETE OBJECT | PROCEDURE | WIDGET`, `RELEASE OBJECT`
//! and `APPLY`. Each used to be skipped or mistaken for a record statement, so
//! the variables in them were silently untracked or wrongly reported.

use oxabl_common::{FileId, LintSeverityMap};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0002, LINT0006, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

/// Full pipeline, returning `(code, message)` per diagnostic.
fn lint_all(source: &str) -> Vec<(String, String)> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
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

#[track_caller]
fn assert_clean(source: &str) {
    let diags = lint_all(source);
    assert!(diags.is_empty(), "{source}\n{diags:?}");
}

#[test]
fn delete_procedure_reads_its_handle_and_is_not_a_buffer_reference() {
    assert_clean(
        "\
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hProc.
DELETE PROCEDURE hProc.
",
    );
}

#[test]
fn delete_widget_reads_every_handle() {
    assert_clean(
        "\
DEFINE VARIABLE h1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h2 AS HANDLE NO-UNDO.
CREATE BUTTON h1.
CREATE BUTTON h2.
DELETE WIDGET h1 h2.
",
    );
}

#[test]
fn create_handle_types_write_the_handle() {
    assert_clean(
        "\
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hNode AS HANDLE NO-UNDO.
CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hNode.
hDoc:LOAD(\"file\", \"x.xml\", FALSE).
DELETE OBJECT hNode.
DELETE OBJECT hDoc.
",
    );
}

#[test]
fn create_with_a_subscripted_handle_writes_the_array() {
    assert_clean(
        "\
DEFINE VARIABLE hArr AS HANDLE NO-UNDO EXTENT 3.
CREATE QUERY hArr[1].
DELETE OBJECT hArr[1].
",
    );
}

#[test]
fn create_widget_assign_expressions_are_reads() {
    let source = "\
DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE hBtn AS HANDLE NO-UNDO.
cLabel = \"x\".
CREATE BUTTON hBtn ASSIGN LABEL = cLabel.
DELETE WIDGET hBtn.
";
    assert_clean(source);
}

#[test]
fn a_widget_created_under_a_frame_or_parent_is_owned_and_not_dead() {
    assert_clean(
        "\
DEFINE VARIABLE hFrame AS HANDLE NO-UNDO.
DEFINE VARIABLE hBtn AS HANDLE NO-UNDO.
DEFINE VARIABLE hFld AS HANDLE NO-UNDO.
hFrame = FRAME f1:HANDLE.
CREATE BUTTON hBtn ASSIGN FRAME = hFrame.
CREATE FILL-IN hFld ASSIGN PARENT = hFrame.
",
    );
}

#[test]
fn a_widget_assigned_a_frame_later_is_owned_and_not_dead() {
    assert_clean(
        "\
DEFINE VARIABLE hFrame AS HANDLE NO-UNDO.
DEFINE VARIABLE hBtn AS HANDLE NO-UNDO.
hFrame = FRAME f1:HANDLE.
CREATE BUTTON hBtn.
hBtn:FRAME = hFrame.
",
    );
}

#[test]
fn a_widget_created_without_a_parent_and_never_read_is_dead() {
    let diags = lint_all(
        "\
DEFINE VARIABLE hBtn AS HANDLE NO-UNDO.
CREATE BUTTON hBtn ASSIGN LABEL = \"x\".
",
    );
    assert!(diags.iter().any(|(c, _)| c == LINT0006), "{diags:?}");
}

#[test]
fn create_widget_trigger_bodies_are_analyzed() {
    let source = "\
DEFINE VARIABLE iClicks AS INTEGER NO-UNDO.
DEFINE VARIABLE hBtn AS HANDLE NO-UNDO.
CREATE BUTTON hBtn
    TRIGGERS:
        ON CHOOSE DO:
            iClicks = iClicks + 1.
            missingName = 1.
        END.
    END TRIGGERS.
DELETE WIDGET hBtn.
";
    let diags = lint_all(source);
    let undefined: Vec<_> = diags.iter().filter(|(c, _)| c == LINT0001).collect();
    assert_eq!(undefined.len(), 1, "{diags:?}");
    assert!(undefined[0].1.contains("missingName"), "{diags:?}");
    assert!(
        !diags.iter().any(|(c, _)| c == LINT0002 || c == LINT0006),
        "{diags:?}"
    );
}

#[test]
fn widget_pool_names_are_reads() {
    assert_clean(
        "\
DEFINE VARIABLE cPool AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
cPool = \"p\".
CREATE WIDGET-POOL cPool PERSISTENT.
CREATE QUERY hQuery IN WIDGET-POOL cPool.
DELETE OBJECT hQuery.
DELETE WIDGET-POOL cPool.
",
    );
}

#[test]
fn com_create_and_release_track_the_automation_handle() {
    assert_clean(
        "\
DEFINE VARIABLE chExcel AS COM-HANDLE NO-UNDO.
CREATE \"Excel.Application\" chExcel NO-ERROR.
RELEASE OBJECT chExcel NO-ERROR.
",
    );
}

#[test]
fn apply_reads_its_target_handle_and_event_variable() {
    assert_clean(
        "\
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEFINE VARIABLE cEvent AS CHARACTER NO-UNDO.
cEvent = \"CLOSE\".
RUN lib.p PERSISTENT SET hProc.
APPLY cEvent TO hProc.
",
    );
}

#[test]
fn apply_to_a_static_widget_or_frame_is_not_an_undefined_symbol() {
    assert_clean(
        "\
APPLY \"CHOOSE\" TO btnOk IN FRAME f1.
APPLY \"ENTRY\" TO BROWSE brw.
APPLY \"GO\" TO FRAME fMain.
",
    );
}

#[test]
fn an_undeclared_deleted_handle_is_still_an_undefined_symbol() {
    let diags = lint_all("DELETE PROCEDURE hMissing.\n");
    assert!(
        diags
            .iter()
            .any(|(c, m)| c == LINT0001 && m.contains("hMissing")),
        "{diags:?}"
    );
}

#[test]
fn create_buffer_for_table_reads_the_table_handle_and_accepts_a_table_name() {
    assert_clean(
        "\
DEFINE VARIABLE hTt AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE hCust AS HANDLE NO-UNDO.
CREATE BUFFER hBuf FOR TABLE hTt:DEFAULT-BUFFER-HANDLE.
CREATE BUFFER hCust FOR TABLE Customer.
DELETE OBJECT hBuf.
DELETE OBJECT hCust.
DELETE OBJECT hTt.
",
    );
}

#[test]
fn a_widget_handle_used_only_as_a_trigger_target_is_read() {
    for source in [
        "\
DEFINE VARIABLE hb AS HANDLE NO-UNDO.
CREATE BUTTON hb ASSIGN LABEL = \"x\".
ON CHOOSE OF hb DO:
  MESSAGE \"x\".
END.
",
        "\
DEFINE VARIABLE hb AS HANDLE NO-UNDO.
CREATE BUTTON hb.
ON CHOOSE, ENTRY OF hb IN FRAME f1 DO:
  MESSAGE \"x\".
END.
",
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.
ON CHOOSE OF hq PERSISTENT RUN p IN THIS-PROCEDURE.
",
    ] {
        assert_clean(source);
    }
}

#[test]
fn a_trigger_target_that_is_no_value_is_not_an_undefined_symbol() {
    assert_clean("ON CHOOSE OF btnFromAFrame DO:\n  MESSAGE \"x\".\nEND.\n");
}

#[test]
fn builtin_functions_in_a_create_trigger_body_are_defined() {
    assert_clean(
        "\
DEFINE VARIABLE hb AS HANDLE NO-UNDO.
DEFINE VARIABLE hf AS HANDLE NO-UNDO.
DEFINE VARIABLE s AS CHARACTER NO-UNDO.
CREATE BUTTON hb
  ASSIGN LABEL = \"x\"
  TRIGGERS:
    ON \"choose\":U
    DO:
      ASSIGN s = \"a\" + lower( hf:TABLE ) + \"b\".
      s = LOWER(s).
      MESSAGE s.
    END.
  END TRIGGERS.
DELETE WIDGET hb.
",
    );
}

#[test]
fn memptr_and_case_builtins_are_defined() {
    assert_clean(
        "\
DEFINE VARIABLE mp AS MEMPTR NO-UNDO.
DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE VARIABLE s AS CHARACTER NO-UNDO.
SET-SIZE(mp) = 16.
PUT-BYTE(mp, 1) = 65.
PUT-STRING(mp, 1) = \"x\".
SET-BYTE-ORDER(mp) = 1.
FIX-CODEPAGE(lc) = \"UTF-8\".
h = WIDGET-H(\"1024\").
s = LOWER(UPPER(s)).
MESSAGE h s GET-SIZE(mp) LENGTH(lc).
",
    );
}
