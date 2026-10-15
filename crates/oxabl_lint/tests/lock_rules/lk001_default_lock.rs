//! `default-lock` (LK001).

use oxabl_lint::default_lock::run;

use super::{check, check_no_schema, check_recovered, help, lines};

#[test]
fn reports_each_database_read_with_no_lock_phrase() {
    let src = "\
FIND FIRST Customer.
FIND FIRST Item NO-LOCK.
FOR EACH Order, EACH Item NO-LOCK:
END.
IF CAN-FIND(FIRST Item WHERE Item.ItemNum = 1) THEN MESSAGE \"x\".
DEFINE QUERY q FOR Customer.
OPEN QUERY q FOR EACH Customer.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [1, 3, 5, 7]);
    assert!(diags[0].message.contains("SHARE-LOCK"));
    assert!(diags[0].message.contains("`Customer`"));
    assert!(diags[2].message.contains("`CAN-FIND`"));
}

#[test]
fn temp_tables_and_unknown_tables_are_not_reported() {
    let src = "\
DEFINE TEMP-TABLE ttCust NO-UNDO FIELD k AS INTEGER.
FIND FIRST ttCust.
FIND FIRST Mystery.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn nothing_is_reported_without_a_schema() {
    assert!(check_no_schema(run, "FIND FIRST Customer.\n").is_empty());
}

#[test]
fn get_takes_the_lock_of_its_query() {
    let src = "\
DEFINE QUERY q FOR Customer.
OPEN QUERY q FOR EACH Customer NO-LOCK.
GET FIRST q.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn the_help_suggests_no_lock_only_when_nothing_writes_the_record() {
    let src = "\
FIND FIRST Customer.
DISPLAY Customer.Name.
FIND FIRST Item.
Item.Price = 1.
PROCEDURE p:
    DEFINE BUFFER bOrder FOR Order.
    FIND FIRST bOrder.
    RUN other.p (BUFFER bOrder).
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [1, 3, 7]);
    assert!(help(&diags[0]).contains("`NO-LOCK`"), "{}", help(&diags[0]));
    assert!(help(&diags[1]).contains("`EXCLUSIVE-LOCK`"));
    assert!(!help(&diags[1]).contains("NO-LOCK"));
    assert!(!help(&diags[2]).contains("NO-LOCK"), "{}", help(&diags[2]));
    assert!(!help(&diags[2]).contains("EXCLUSIVE-LOCK"));
}

#[test]
fn a_buffer_parameter_gets_no_lock_suggestion() {
    let src = "\
DEFINE PARAMETER BUFFER bCust FOR Customer.
FIND FIRST bCust.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(!help(&diags[0]).contains("NO-LOCK"));
}

#[test]
fn a_recovered_parse_is_not_judged() {
    let src = "\
FIND FIRST Customer.
DO PRESELECT EACH Order:
  PROCEDURE x:
END.
";
    assert!(check_recovered(run, src).is_empty());
}

#[test]
fn the_screen_form_of_assign_writes_the_record() {
    let src = "\
FIND FIRST Customer WHERE Customer.CustNum = 1.
PROMPT-FOR Customer.Name.
ASSIGN Customer.Name.
FIND FIRST Item.
ASSIGN INPUT Item.Price.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [1, 4]);
    for d in &diags {
        assert!(help(d).contains("`EXCLUSIVE-LOCK`"), "{}", help(d));
    }
}

#[test]
fn a_write_earlier_in_a_loop_writes_the_record_of_the_next_read() {
    let src = "\
REPEAT:
    IF AVAILABLE Customer THEN Customer.Balance = 0.
    FIND NEXT Customer.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(help(&diags[0]).contains("`EXCLUSIVE-LOCK`"));
}

#[test]
fn a_write_in_another_routine_of_the_scope_gets_no_lock_suggestion() {
    let src = "\
PROCEDURE upd:
    Order.CustNum = 1.
END PROCEDURE.
PROCEDURE rd:
    FIND FIRST Order.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [5]);
    assert!(!help(&diags[0]).contains("NO-LOCK"), "{}", help(&diags[0]));
}

#[test]
fn share_abbreviates_share_lock() {
    assert!(check(run, "FIND FIRST Order SHARE.\nFIND FIRST Item EXCLUSIVE.\n").is_empty());
}

#[test]
fn a_can_find_names_the_wait_and_suggests_no_lock() {
    let src = "IF CAN-FIND(FIRST Item WHERE Item.ItemNum = 1) THEN MESSAGE \"x\".\n";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [1]);
    assert!(diags[0].message.contains("waits while another session"));
    assert!(help(&diags[0]).contains("keeps no record"));
    assert!(help(&diags[0]).contains("`NO-LOCK`"));
}
