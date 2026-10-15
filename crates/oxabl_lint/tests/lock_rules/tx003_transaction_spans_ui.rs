//! `transaction-spans-ui` (TX003).

use oxabl_lint::transaction_spans_ui::run;

use super::{check, check_fragment, check_no_schema, lines};

#[test]
fn input_statements_in_a_do_transaction_are_reported() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
DO TRANSACTION:
    UPDATE c.
    SET c.
    PROMPT-FOR c.
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    PAUSE.
    PAUSE MESSAGE \"go on\".
    READKEY.
    MESSAGE \"sure?\" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l.
    MESSAGE \"done\" VIEW-AS ALERT-BOX.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [4, 5, 6, 7, 8, 9, 10, 11, 12]);
    assert!(diags[0].message.contains("waits for the user"));
    assert!(diags[0].message.contains("`DO TRANSACTION`"));
}

#[test]
fn statements_that_do_not_wait_are_not_reported() {
    let src = "\
DO TRANSACTION:
    PAUSE 0.
    PAUSE 1 NO-MESSAGE.
    PAUSE 0 BEFORE-HIDE.
    READKEY PAUSE 2.
    MESSAGE \"working\".
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_implicit_transaction_counts_and_names_the_outermost_block() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FOR EACH Customer EXCLUSIVE-LOCK:
    DO:
        UPDATE c.
    END.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [4]);
    assert!(
        diags[0]
            .message
            .contains("`FOR EACH Customer EXCLUSIVE-LOCK`")
    );
}

#[test]
fn the_classic_interactive_update_is_reported() {
    let src = "\
REPEAT:
    PROMPT-FOR Customer.CustNum.
    FIND FIRST Customer WHERE Customer.CustNum = 1 EXCLUSIVE-LOCK.
    UPDATE Customer.Name.
END.
";
    assert_eq!(lines(src, &check(run, src)), [2, 4]);
}

#[test]
fn outside_a_transaction_nothing_is_reported() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
UPDATE c.
DO TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
END.
ON CHOOSE OF FRAME f DO:
    MESSAGE \"x\" VIEW-AS ALERT-BOX.
END.
FOR EACH Customer NO-LOCK:
    PAUSE.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_implicit_transaction_needs_a_schema_table() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
REPEAT:
    CREATE tt.
    PAUSE.
    LEAVE.
END.
REPEAT:
    CREATE Customer.
    PAUSE.
    LEAVE.
END.
";
    assert_eq!(lines(src, &check(run, src)), [9]);
    assert!(check_no_schema(run, src).is_empty());
}

#[test]
fn an_include_alone_does_not_judge_its_main_block() {
    let src = "\
CREATE Customer.
PAUSE.
DO TRANSACTION:
    PAUSE.
END.
";
    assert_eq!(lines(src, &check(run, src)), [2, 4]);
    assert_eq!(lines(src, &check_fragment(run, src)), [4]);
}

#[test]
fn choose_waits_for_the_user() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DO TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    CHOOSE FIELD c.
END.
";
    assert_eq!(lines(src, &check(run, src)), [4]);
}
