//! `persistent-procedure-leak` (HD003).

use oxabl_lint::persistent_procedure_leak::run;

use super::{check, check_fragment, help, lines};

#[test]
fn reports_a_persistent_procedure_that_is_never_deleted() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE hTool AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
RUN tool.p PERSISTENT SET hTool.
DELETE PROCEDURE hTool.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(diags[0].message.contains("RUN lib.p PERSISTENT SET hLib"));
    assert!(help(&diags[0]).contains("DELETE PROCEDURE hLib"));
}

#[test]
fn run_persistent_without_set_is_not_reported() {
    let src = "RUN singleton.p PERSISTENT.\n";
    assert!(check(run, src).is_empty());
}

#[test]
fn apply_close_deletes_the_procedure() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
APPLY \"CLOSE\" TO hLib.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_valid_handle_guard_is_a_delete() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
IF VALID-HANDLE(hLib) THEN DELETE PROCEDURE hLib.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_delete_in_finally_is_not_reported() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DO ON ERROR UNDO, THROW:
    RUN lib.p PERSISTENT SET hLib.
    RUN work IN hLib.
    FINALLY:
        IF VALID-HANDLE(hLib) THEN DELETE PROCEDURE hLib.
    END FINALLY.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_handle_passed_to_the_caller_is_not_reported() {
    let src = "\
DEFINE OUTPUT PARAMETER phLib AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET phLib.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_super_procedure_is_not_reported() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hLib).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn other_dynamic_objects_are_not_reported() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_destroy_call_leaves_the_state_unknown() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
RUN destroy IN hLib.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_include_names_its_includer() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
";
    let diags = check_fragment(run, src);
    assert_eq!(diags.len(), 1);
    assert!(help(&diags[0]).contains("This file is an include"));
}
