//! `run-value` (CL001).

use oxabl_lint::run_value::run;

use super::{check, flagged};

#[test]
fn reports_a_dynamic_target() {
    let src = "RUN VALUE(cProg) (INPUT 1).";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(flagged(src, &diags), ["cProg"]);
}

#[test]
fn reports_every_spelling_of_the_statement() {
    let src = "\
RUN VALUE(c).
RUN VALUE(c) IN h.
RUN VALUE(c) PERSISTENT SET h.
RUN VALUE(c) ON SERVER h.
RUN VALUE(c) ASYNCHRONOUS SET h.
RUN VALUE(c + \".p\") (INPUT 1, OUTPUT x) NO-ERROR.
";
    assert_eq!(check(run, src).len(), 6);
}

#[test]
fn reports_in_nested_blocks_and_class_members() {
    let src = "\
CLASS Acme.Dispatcher:
    METHOD PUBLIC VOID Go(INPUT p AS CHARACTER):
        IF p > \"\" THEN DO:
            RUN VALUE(p).
        END.
    END METHOD.
END CLASS.
PROCEDURE inner:
    FOR EACH Customer:
        RUN VALUE(\"a\" + Customer.Name).
    END.
END PROCEDURE.
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn reports_inside_trigger_blocks() {
    let src = "ON CHOOSE OF btn IN FRAME f DO: RUN VALUE(c). END.";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn static_targets_are_not_reported() {
    let src = "\
RUN prog.p.
RUN \"quoted.p\".
RUN dir/sub/prog.p (INPUT 1).
RUN inner-proc IN h.
RUN inner-proc PERSISTENT SET h.
RUN VALUE(\"literal.p\").
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_concatenation_of_literals_is_still_dynamic() {
    let src = "RUN VALUE(\"a\" + \".p\").";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn other_dynamic_calls_belong_to_another_rule() {
    let src = "x = DYNAMIC-FUNCTION(\"f\").";
    assert!(check(run, src).is_empty());
}
