//! `delete-not-in-finally` (HD002).

use oxabl_lint::delete_not_in_finally::run;

use super::{check, help, lines};

#[test]
fn reports_a_delete_on_the_success_path_only() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
    hQuery:QUERY-PREPARE(\"for each customer\").
    DELETE OBJECT hQuery.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [5]);
    let message = &diags[0].message;
    assert!(message.contains("DELETE OBJECT hQuery"), "{message}");
    assert!(message.contains("hQuery:QUERY-PREPARE"), "{message}");
    assert!(help(&diags[0]).contains("FINALLY"));
    assert_eq!(lines(src, &[diags[0].clone()]).len(), 1);
    assert_eq!(diags[0].labels.len(), 1);
}

#[test]
fn reports_each_delete_site() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    DEFINE VARIABLE lOpen AS LOGICAL NO-UNDO.
    CREATE QUERY hQuery.
    hQuery:QUERY-PREPARE(\"for each customer\").
    IF lOpen THEN DELETE OBJECT hQuery.
    ELSE DELETE OBJECT hQuery.
END PROCEDURE.
";
    assert_eq!(lines(src, &check(run, src)), [6, 7]);
}

#[test]
fn reports_a_persistent_procedure_and_a_memptr_too() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
    DEFINE VARIABLE m AS MEMPTR NO-UNDO.
    RUN lib.p PERSISTENT SET hLib.
    RUN work IN hLib.
    DELETE PROCEDURE hLib.
    SET-SIZE(m) = 16.
    RUN work2 IN hLib.
    SET-SIZE(m) = 0.
END PROCEDURE.
";
    assert_eq!(lines(src, &check(run, src)), [6, 9]);
}

#[test]
fn a_delete_in_finally_is_not_reported() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    DO ON ERROR UNDO, THROW:
        CREATE QUERY hQuery.
        hQuery:QUERY-OPEN().
        FINALLY:
            IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
        END FINALLY.
    END.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_object_that_leaks_on_a_normal_path_is_left_to_hd001() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    DEFINE VARIABLE lStop AS LOGICAL NO-UNDO.
    CREATE QUERY hQuery.
    IF lStop THEN RETURN.
    DELETE OBJECT hQuery.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_object_with_no_delete_is_not_reported() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_object_that_escapes_is_not_reported() {
    let src = "\
PROCEDURE p:
    DEFINE OUTPUT PARAMETER phQuery AS HANDLE NO-UNDO.
    CREATE QUERY phQuery.
    phQuery:QUERY-OPEN().
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_plain_test_is_not_the_cause() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    DEFINE VARIABLE lGo AS LOGICAL NO-UNDO.
    CREATE QUERY hQuery.
    IF lGo THEN lGo = NO.
    hQuery:QUERY-PREPARE(\"for each customer\").
    DELETE OBJECT hQuery.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1);
    let message = &diags[0].message;
    assert!(message.contains("hQuery:QUERY-PREPARE"), "{message}");
}

#[test]
fn a_call_in_a_test_is_a_cause() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
    IF LENGTH(\"x\") > 5 THEN RETURN.
    DELETE OBJECT hQuery.
END PROCEDURE.
";
    let diags = check(run, src);
    assert!(
        diags.iter().all(|d| d.message.contains("LENGTH")),
        "{diags:?}"
    );
}

#[test]
fn a_run_is_named_before_an_earlier_plain_statement() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    DEFINE VARIABLE iA AS INTEGER NO-UNDO.
    CREATE QUERY hQuery.
    iA = iA + 1.
    RUN work.p.
    DELETE OBJECT hQuery.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1);
    assert!(
        diags[0].message.contains("RUN work.p"),
        "{}",
        diags[0].message
    );
}

#[test]
fn a_validity_or_error_status_test_is_not_the_cause() {
    let src = "\
PROCEDURE a:
    DEFINE VARIABLE h AS HANDLE NO-UNDO.
    CREATE X-DOCUMENT h NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.
    DELETE OBJECT h.
END PROCEDURE.
PROCEDURE b:
    DEFINE VARIABLE h AS HANDLE NO-UNDO.
    REPEAT:
        CREATE QUERY h.
        IF VALID-HANDLE(h) THEN DELETE OBJECT h.
        LEAVE.
    END.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}
