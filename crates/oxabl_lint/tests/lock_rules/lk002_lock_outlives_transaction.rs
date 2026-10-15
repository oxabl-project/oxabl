//! `lock-outlives-transaction` (LK002).

use oxabl_lint::lock_outlives_transaction::run;

use super::{check, help, lines};

#[test]
fn a_do_transaction_reading_into_a_procedure_scope_is_reported() {
    let src = "\
DO TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END.
MESSAGE \"done\".
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(
        diags[0]
            .message
            .contains("EXCLUSIVE-LOCK becomes a SHARE-LOCK")
    );
    assert!(diags[0].message.contains("the main block"));
    assert!(diags[0].message.contains("`DO TRANSACTION`"));
    assert!(help(&diags[0]).contains("DO FOR Customer TRANSACTION"));
}

#[test]
fn a_strong_scope_ends_with_the_transaction() {
    let src = "\
DO FOR Customer TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END.
MESSAGE \"done\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_for_each_that_is_its_own_transaction_is_not_reported() {
    let src = "\
FOR EACH Customer EXCLUSIVE-LOCK:
    Customer.Balance = 0.
END.
MESSAGE \"done\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_weak_scope_raised_by_a_free_reference_is_reported() {
    let src = "\
FIND FIRST Customer NO-LOCK.
FOR FIRST Customer EXCLUSIVE-LOCK:
    Customer.Balance = 0.
END.
MESSAGE \"done\".
";
    assert_eq!(lines(src, &check(run, src)), [2]);
}

#[test]
fn a_raised_for_each_is_not_reported() {
    let src = "\
FIND FIRST Customer NO-LOCK.
FOR EACH Customer EXCLUSIVE-LOCK:
    Customer.Balance = 0.
END.
MESSAGE \"done\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_free_scope_merged_into_an_outer_one_is_not_reported() {
    let src = "\
FIND FIRST Customer NO-LOCK.
REPEAT:
    FIND NEXT Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END.
MESSAGE \"done\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_internal_procedure_using_the_file_buffer_is_reported() {
    let src = "\
RUN upd.
PROCEDURE upd:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(diags[0].message.contains("procedure `upd`"));
    assert!(help(&diags[0]).contains("Define a buffer for `Customer`"));
}

#[test]
fn a_buffer_defined_in_the_procedure_is_not_reported() {
    let src = "\
RUN upd.
PROCEDURE upd:
    DEFINE BUFFER bCust FOR Customer.
    FIND FIRST bCust EXCLUSIVE-LOCK.
    bCust.Balance = 0.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_release_before_the_end_is_not_reported() {
    let src = "\
DO TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
    RELEASE Customer.
END.
MESSAGE \"done\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_last_read_with_no_lock_is_not_reported() {
    let src = "\
DO TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
    FIND FIRST Customer NO-LOCK.
END.
MESSAGE \"done\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn nothing_after_the_transaction_in_the_scope_is_not_reported() {
    let src = "\
REPEAT:
    DO TRANSACTION:
        FIND NEXT Customer EXCLUSIVE-LOCK.
        Customer.Balance = 0.
    END.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn temp_tables_are_not_reported() {
    let src = "\
DEFINE TEMP-TABLE ttX FIELD k AS INTEGER.
DO TRANSACTION:
    FIND FIRST ttX EXCLUSIVE-LOCK.
    ttX.k = 1.
END.
MESSAGE \"done\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_function_with_an_implicit_transaction_is_not_reported() {
    let src = "\
FUNCTION f RETURNS LOGICAL ():
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
    RETURN TRUE.
END FUNCTION.
MESSAGE f().
";
    assert!(check(run, src).is_empty());
}

#[test]
fn the_help_names_scope_widened_only_when_it_reports_the_raise() {
    let alone = "\
PROCEDURE setBal:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END PROCEDURE.
";
    let diags = check(run, alone);
    assert_eq!(lines(alone, &diags), [2]);
    assert!(!help(&diags[0]).contains("TX005"), "{}", help(&diags[0]));

    let raised = "\
FOR EACH Customer NO-LOCK:
END.
DO TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END.
DISPLAY Customer.Name.
";
    let diags = check(run, raised);
    assert_eq!(lines(raised, &diags), [4]);
    assert!(help(&diags[0]).contains("TX005"), "{}", help(&diags[0]));
}
