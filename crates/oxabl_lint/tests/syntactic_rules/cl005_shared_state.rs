//! `shared-state` (CL005).

use oxabl_lint::shared_state::run;

use super::{check, flagged};

fn messages(src: &str) -> Vec<String> {
    check(run, src).into_iter().map(|d| d.message).collect()
}

#[test]
fn reports_the_three_modes_on_a_variable() {
    let src = "\
DEFINE NEW SHARED VARIABLE a AS INTEGER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE b AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE c AS INTEGER NO-UNDO.
DEF NEW GLOBAL SHARED VAR d AS CHAR NO-UNDO.
";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["a", "b", "c", "d"]);
    assert_eq!(
        messages(src),
        [
            "`NEW SHARED` variable `a` is shared state",
            "`NEW GLOBAL SHARED` variable `b` is shared state",
            "`SHARED` variable `c` is shared state",
            "`NEW GLOBAL SHARED` variable `d` is shared state",
        ]
    );
}

#[test]
fn reports_each_modelled_object_kind() {
    let src = "\
DEFINE NEW SHARED TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
DEFINE SHARED BUFFER bCust FOR Customer.
DEFINE NEW SHARED DATASET ds FOR tt.
DEFINE SHARED STREAM sOut.
DEFINE NEW SHARED FRAME fMain WITH 1 DOWN.
";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["tt", "bCust", "ds", "sOut", "fMain"]);
    assert_eq!(
        messages(src),
        [
            "`NEW SHARED` temp-table `tt` is shared state",
            "`SHARED` buffer `bCust` is shared state",
            "`NEW SHARED` dataset `ds` is shared state",
            "`SHARED` stream `sOut` is shared state",
            "`NEW SHARED` frame `fMain` is shared state",
        ]
    );
}

#[test]
fn reports_forms_the_parser_does_not_model() {
    let src = "\
DEFINE NEW SHARED QUERY q1 FOR Customer.
DEFINE SHARED BROWSE b1 QUERY q1 DISPLAY Customer.Name WITH 5 DOWN.
DEFINE NEW GLOBAL SHARED MENU m1 MENUBAR.
DEF NEW SHARED BUTTON btn LABEL \"x\".
";
    let diags = check(run, src);
    assert_eq!(
        flagged(src, &diags),
        [
            "DEFINE NEW SHARED QUERY",
            "DEFINE SHARED BROWSE",
            "DEFINE NEW GLOBAL SHARED MENU",
            "DEF NEW SHARED BUTTON",
        ]
    );
    assert_eq!(
        messages(src),
        [
            "`NEW SHARED` query is shared state",
            "`SHARED` browse is shared state",
            "`NEW GLOBAL SHARED` menu is shared state",
            "`NEW SHARED` button is shared state",
        ]
    );
}

#[test]
fn reports_one_diagnostic_per_definition_in_nested_scopes() {
    let src = "\
PROCEDURE p:
    DEFINE SHARED VARIABLE a AS INTEGER NO-UNDO.
    DEFINE SHARED VARIABLE b AS INTEGER NO-UNDO.
END PROCEDURE.
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn unshared_definitions_are_not_reported() {
    let src = "\
DEFINE VARIABLE a AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
DEFINE BUFFER bCust FOR Customer.
DEFINE DATASET ds FOR tt.
DEFINE STREAM sOut.
DEFINE FRAME fMain WITH 1 DOWN.
DEFINE QUERY q1 FOR Customer.
DEFINE BROWSE b1 QUERY q1 DISPLAY Customer.Name WITH 5 DOWN.
DEFINE BUTTON btn LABEL \"x\".
DEFINE INPUT PARAMETER p AS INTEGER NO-UNDO.
VAR INTEGER v.
";
    assert!(check(run, src).is_empty(), "{:?}", messages(src));
}

#[test]
fn a_statement_that_merely_mentions_shared_is_not_a_definition() {
    let src = "\
MESSAGE \"SHARED\".
PUT UNFORMATTED \"DEFINE SHARED STREAM s\" SKIP.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_legacy_work_tables() {
    let src = "\
DEFINE NEW SHARED WORK-TABLE wt FIELD a AS INTEGER.
DEFINE SHARED WORKFILE wf FIELD a AS INTEGER.
";
    assert_eq!(
        messages(src),
        [
            "`NEW SHARED` work-table `wt` is shared state",
            "`SHARED` workfile `wf` is shared state",
        ]
    );
}
