//! `implicit-transaction` (TX001).

use oxabl_lint::implicit_transaction::run;

use super::{check, check_fragment, check_no_schema, check_recovered, help, lines};

#[test]
fn a_repeat_that_creates_a_record_is_reported_at_the_create() {
    let src = "\
REPEAT:
    CREATE Customer.
    Customer.CustNum = 1.
    LEAVE.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(
        diags[0]
            .message
            .starts_with("`REPEAT` is a transaction block")
    );
    assert!(diags[0].message.contains("creates a record of `Customer`"));
    assert!(help(&diags[0]).contains("TRANSACTION"));
}

#[test]
fn a_for_each_exclusive_lock_is_reported_once() {
    let src = "\
FOR EACH Customer EXCLUSIVE-LOCK:
    Customer.Balance = 0.
    Customer.Name = \"x\".
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [1]);
    assert!(diags[0].message.contains("with EXCLUSIVE-LOCK"));
}

#[test]
fn an_internal_procedure_is_reported_with_its_name() {
    let src = "\
PROCEDURE upd:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(diags[0].message.starts_with("procedure `upd`"));
    assert!(help(&diags[0]).contains("DO TRANSACTION"));
}

#[test]
fn explicit_transactions_are_not_reported() {
    let src = "\
DO TRANSACTION:
    FIND FIRST Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END.
FOR EACH Order EXCLUSIVE-LOCK TRANSACTION:
    DELETE Order.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_sub_transaction_is_not_reported() {
    let src = "\
DO TRANSACTION:
    FOR EACH Order EXCLUSIVE-LOCK:
        DELETE Order.
    END.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn the_main_block_is_left_to_tx002() {
    let src = "\
FIND FIRST Customer EXCLUSIVE-LOCK.
Customer.Balance = 0.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn temp_table_updates_make_no_transaction() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
REPEAT:
    CREATE tt.
    tt.a = 1.
    LEAVE.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn functions_and_triggers_are_not_reported() {
    let src = "\
FUNCTION f RETURNS LOGICAL ():
    FIND FIRST Customer EXCLUSIVE-LOCK.
    RETURN TRUE.
END FUNCTION.
ON WRITE OF Order DO:
    FIND FIRST Item EXCLUSIVE-LOCK.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_block_inside_an_unknown_block_is_not_reported() {
    let src = "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
REPEAT:
    h:BUFFER-CREATE().
    REPEAT:
        CREATE Customer.
        LEAVE.
    END.
    LEAVE.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn silent_without_a_schema_or_after_a_recovered_parse() {
    let src = "\
REPEAT:
    CREATE Customer.
    LEAVE.
END.
";
    assert!(check_no_schema(run, src).is_empty());
    let broken = "\
REPEAT:
    CREATE Customer.
    LEAVE.
END.
DISPLAY (.
";
    assert!(check_recovered(run, broken).is_empty());
}

#[test]
fn an_include_alone_does_not_judge_its_main_block() {
    let src = "\
FOR EACH Customer EXCLUSIVE-LOCK:
    Customer.Balance = 0.
END.
PROCEDURE upd:
    CREATE Order.
END PROCEDURE.
";
    assert_eq!(lines(src, &check_fragment(run, src)), [5]);
}
