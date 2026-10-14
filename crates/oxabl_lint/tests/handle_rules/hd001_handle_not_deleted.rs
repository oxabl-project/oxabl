//! `handle-not-deleted` (HD001).

use oxabl_lint::handle_not_deleted::run;

use super::{check, check_fragment, help, lines};

#[test]
fn reports_an_object_lost_on_an_early_return() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE lStop AS LOGICAL NO-UNDO.
CREATE QUERY hQuery.
IF lStop THEN RETURN.
DELETE OBJECT hQuery.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(
        diags[0].message.contains("`hQuery`"),
        "{}",
        diags[0].message
    );
    assert!(diags[0].message.contains("query"), "{}", diags[0].message);
    assert_eq!(diags[0].labels.len(), 1);
}

#[test]
fn reports_an_object_that_is_never_deleted() {
    let src = "\
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
CREATE BUFFER hBuffer FOR TABLE \"customer\".
MESSAGE hBuffer:NAME.
";
    assert_eq!(lines(src, &check(run, src)), [2]);
}

#[test]
fn reports_each_object_once() {
    let src = "\
DEFINE VARIABLE hA AS HANDLE NO-UNDO.
DEFINE VARIABLE hB AS HANDLE NO-UNDO.
CREATE QUERY hA.
CREATE TEMP-TABLE hB.
";
    assert_eq!(lines(src, &check(run, src)), [3, 4]);
}

#[test]
fn reports_an_overwrite_at_the_overwriting_statement() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
hQuery = ?.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(
        diags[0].message.contains("assigned"),
        "{}",
        diags[0].message
    );
}

#[test]
fn a_valid_handle_guard_is_a_delete() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_delete_in_finally_is_not_reported() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DO ON ERROR UNDO, THROW:
    CREATE QUERY hQuery.
    hQuery:QUERY-PREPARE(\"for each customer\").
    FINALLY:
        IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
    END FINALLY.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_object_handed_to_an_output_parameter_is_not_reported() {
    let src = "\
DEFINE OUTPUT PARAMETER phQuery AS HANDLE NO-UNDO.
CREATE QUERY phQuery.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_handle_returned_to_the_caller_is_not_reported() {
    let src = "\
FUNCTION make-query RETURNS HANDLE ():
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
    RETURN hQuery.
END FUNCTION.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_object_in_a_pool_the_procedure_deletes_is_not_reported() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE WIDGET-POOL.
CREATE QUERY hQuery.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_delete_that_only_an_error_skips_is_left_to_hd002() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
    hQuery:QUERY-OPEN().
    DELETE OBJECT hQuery.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn persistent_procedures_memptrs_and_class_instances_are_not_reported() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DEFINE VARIABLE o AS Acme.Thing NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
SET-SIZE(m) = 16.
o = NEW Acme.Thing().
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_object_a_skipped_statement_may_free_is_not_reported() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE cProg AS CHARACTER NO-UNDO.
CREATE QUERY hQuery.
RUN VALUE(cProg) (INPUT hQuery).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_include_that_ends_with_the_object_live_names_its_includer() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
";
    let diags = check_fragment(run, src);
    assert_eq!(diags.len(), 1);
    assert!(
        help(&diags[0]).ends_with(oxabl_lint::fragment::FRAGMENT_NOTE),
        "{}",
        help(&diags[0])
    );
    assert!(!oxabl_lint::fragment::autofixable(&diags[0]));
    // The same text as a program has the plain help.
    let plain = check(run, src);
    assert!(!help(&plain[0]).contains("include"));
}

#[test]
fn an_include_overwrite_is_not_lowered() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
hQuery = ?.
";
    let diags = check_fragment(run, src);
    assert_eq!(diags.len(), 1);
    assert!(!help(&diags[0]).contains("include"));
}

#[test]
fn internal_procedure_leaks_are_found() {
    let src = "\
PROCEDURE load:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(
        diags[0].message.contains("procedure `load`"),
        "{}",
        diags[0].message
    );
}

#[test]
fn a_derived_handle_returned_hands_the_object_over() {
    let src = "\
FUNCTION make-table RETURNS HANDLE ():
    DEFINE VARIABLE hTt AS HANDLE NO-UNDO.
    CREATE TEMP-TABLE hTt.
    RETURN hTt:DEFAULT-BUFFER-HANDLE.
END FUNCTION.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_derived_handle_used_locally_is_only_a_use() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hTt AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
    CREATE TEMP-TABLE hTt.
    hBuf = hTt:DEFAULT-BUFFER-HANDLE.
END PROCEDURE.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn a_com_object_is_released_not_deleted() {
    let src = "\
DEFINE VARIABLE chExcel AS COM-HANDLE NO-UNDO.
CREATE \"Excel.Application\" chExcel.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1);
    let h = help(&diags[0]);
    assert!(h.contains("RELEASE OBJECT"), "{h}");
    assert!(!h.contains("DELETE OBJECT"), "{h}");
}

#[test]
fn a_handle_passed_to_a_callee_that_deletes_it_or_that_is_unknown_is_not_reported() {
    let freed = "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.
RUN cleanup (INPUT hq).

PROCEDURE cleanup:
    DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
    DELETE OBJECT ph.
END PROCEDURE.
";
    assert!(check(run, freed).is_empty());
    let external = "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.
RUN cleanup.p (INPUT hq).
";
    assert!(check(run, external).is_empty());
    let kept = "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.
RUN show (INPUT hq).

PROCEDURE show:
    DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
    MESSAGE ph:NAME.
END PROCEDURE.
";
    assert_eq!(lines(kept, &check(run, kept)), [2]);
}
