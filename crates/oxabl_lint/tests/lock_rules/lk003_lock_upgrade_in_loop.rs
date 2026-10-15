//! `lock-upgrade-in-loop` (LK003).

use oxabl_lint::lock_upgrade_in_loop::run;

use super::{check, lines};

#[test]
fn find_current_on_every_iteration_is_reported() {
    let src = "\
FOR EACH Customer NO-LOCK:
    FIND CURRENT Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(
        diags[0]
            .message
            .contains("FIND CURRENT Customer EXCLUSIVE-LOCK")
    );
    assert!(diags[0].message.contains("`FOR EACH Customer NO-LOCK`"));
    assert!(diags[0].message.contains("the `FOR` phrase"));
}

#[test]
fn an_upgrade_under_a_condition_is_not_reported() {
    let src = "\
FOR EACH Customer NO-LOCK:
    IF Customer.Balance > 0 THEN DO:
        FIND CURRENT Customer EXCLUSIVE-LOCK.
        Customer.Balance = 0.
    END.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_upgrade_after_a_conditional_next_is_not_reported() {
    let src = "\
FOR EACH Customer NO-LOCK:
    IF Customer.Balance = 0 THEN NEXT.
    FIND CURRENT Customer EXCLUSIVE-LOCK.
    Customer.Balance = 0.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_find_no_lock_then_a_refind_by_rowid_is_reported() {
    let src = "\
DEFINE BUFFER bCust FOR Customer.
REPEAT:
    FIND NEXT Customer NO-LOCK.
    FIND bCust WHERE ROWID(bCust) = ROWID(Customer) EXCLUSIVE-LOCK.
    bCust.Balance = 0.
END.
";
    assert_eq!(lines(src, &check(run, src)), [4]);
}

#[test]
fn an_upgrade_from_share_lock_is_not_reported() {
    let src = "\
FOR EACH Customer SHARE-LOCK:
    FIND CURRENT Customer EXCLUSIVE-LOCK.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_upgrade_outside_a_loop_is_not_reported() {
    let src = "\
FIND FIRST Customer NO-LOCK.
FIND CURRENT Customer EXCLUSIVE-LOCK.
FOR FIRST Item NO-LOCK:
    FIND CURRENT Item EXCLUSIVE-LOCK.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_upgrade_of_another_buffer_is_not_reported() {
    let src = "\
FOR EACH Customer NO-LOCK:
    FIND FIRST Item NO-LOCK.
    FIND CURRENT Order EXCLUSIVE-LOCK.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_upgrade_with_no_wait_is_not_reported() {
    let src = "\
FOR EACH Customer NO-LOCK:
    FIND CURRENT Customer EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF LOCKED Customer THEN NEXT.
    Customer.Balance = 0.
END.
";
    assert!(check(run, src).is_empty());
}
