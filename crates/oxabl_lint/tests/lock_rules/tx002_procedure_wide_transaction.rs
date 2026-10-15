//! `procedure-wide-transaction` (TX002).

use oxabl_lint::procedure_wide_transaction::run;

use super::{check, check_fragment, check_no_schema, help, lines};

#[test]
fn a_main_block_update_is_reported_once_at_the_first() {
    let src = "\
DEFINE VARIABLE i AS INTEGER NO-UNDO.
FIND FIRST Customer EXCLUSIVE-LOCK.
Customer.Balance = 0.
DO i = 1 TO 2:
    CREATE Order.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(
        diags[0]
            .message
            .contains("reads `Customer` with EXCLUSIVE-LOCK")
    );
    assert!(diags[0].message.contains("spans the whole procedure"));
    assert!(help(&diags[0]).contains("DO TRANSACTION"));
}

#[test]
fn an_update_in_a_plain_do_counts_for_the_main_block() {
    let src = "\
IF TRUE THEN DO:
    CREATE Order.
END.
";
    assert_eq!(lines(src, &check(run, src)), [2]);
}

#[test]
fn updates_in_transaction_blocks_are_not_reported() {
    let src = "\
DO TRANSACTION:
    CREATE Order.
END.
REPEAT:
    CREATE Item.
    LEAVE.
END.
PROCEDURE p:
    CREATE Customer.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_trigger_procedure_is_not_reported() {
    let src = "\
TRIGGER PROCEDURE FOR WRITE OF Customer.
FIND FIRST Item EXCLUSIVE-LOCK.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn silent_without_a_schema_and_in_an_include_alone() {
    let src = "CREATE Order.\n";
    assert!(check_no_schema(run, src).is_empty());
    assert!(check_fragment(run, src).is_empty());
    assert_eq!(lines(src, &check(run, src)), [1]);
}
