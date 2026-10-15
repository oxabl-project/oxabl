//! `find-in-loop` (DB001).

use oxabl_lint::find_in_loop::run;

use super::{check, check_no_schema, check_recovered, help, lines};

#[test]
fn a_find_per_row_of_a_for_each_is_reported() {
    let src = "\
FOR EACH Order NO-LOCK:
    FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
    IF CAN-FIND(FIRST Item WHERE Item.ItemNum = 1) THEN.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2, 3]);
    assert!(diags[0].message.contains("`FIND` of `Customer`"));
    assert!(diags[0].message.contains("`FOR EACH Order NO-LOCK`"));
    assert!(diags[1].message.contains("`CAN-FIND` of `Item`"));
    assert!(help(&diags[0]).contains("FIRST b WHERE"));
}

#[test]
fn a_find_per_row_of_a_query_loop_is_reported() {
    let src = "\
DEFINE QUERY q FOR Order.
OPEN QUERY q FOR EACH Order NO-LOCK.
REPEAT:
    GET NEXT q.
    IF NOT AVAILABLE Order THEN LEAVE.
    FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
END.
FIND FIRST Item NO-LOCK NO-ERROR.
DO WHILE AVAILABLE Item:
    FIND FIRST Order WHERE Order.OrderNum = Item.ItemNum NO-LOCK NO-ERROR.
    FIND NEXT Item NO-LOCK NO-ERROR.
END.
";
    assert_eq!(lines(src, &check(run, src)), [6, 10]);
}

#[test]
fn nested_loops_name_the_innermost() {
    let src = "\
FOR EACH Customer NO-LOCK:
    FOR EACH Order WHERE Order.CustNum = Customer.CustNum NO-LOCK:
        FIND Item WHERE Item.ItemNum = Order.OrderNum NO-LOCK NO-ERROR.
    END.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(diags[0].message.contains("`FOR EACH Order WHERE"));
}

#[test]
fn reads_that_are_not_per_row_are_not_reported() {
    let src = "\
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS INTEGER.
FIND FIRST Customer NO-LOCK NO-ERROR.
FOR FIRST Order NO-LOCK:
    FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
END.
DO i = 1 TO 10:
    FIND Item WHERE Item.ItemNum = i NO-LOCK NO-ERROR.
END.
REPEAT:
    FIND NEXT Customer NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Customer THEN LEAVE.
END.
FOR EACH Order NO-LOCK WHERE CAN-FIND(Customer WHERE Customer.CustNum = Order.CustNum):
    FIND CURRENT Order NO-LOCK.
    FIND FIRST tt WHERE tt.k = Order.OrderNum NO-ERROR.
    RUN lookup (Order.CustNum).
END.
PROCEDURE lookup:
    DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.
    FIND Customer WHERE Customer.CustNum = n NO-LOCK NO-ERROR.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_temp_table_loop_with_a_database_find_is_reported() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS INTEGER.
FOR EACH tt:
    FIND Customer WHERE Customer.CustNum = tt.k NO-LOCK NO-ERROR.
END.
";
    assert_eq!(lines(src, &check(run, src)), [3]);
}

#[test]
fn a_joined_phrase_is_not_reported() {
    let src = "\
FOR EACH Order NO-LOCK, FIRST Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK:
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn nothing_is_reported_without_a_schema_or_after_a_recovered_parse() {
    let src = "\
FOR EACH Order NO-LOCK:
    FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
END.
";
    assert!(check_no_schema(run, src).is_empty());
    let broken = "\
FOR EACH Order NO-LOCK:
    FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
    DISPLAY (.
END.
";
    assert!(check_recovered(run, broken).is_empty());
}

#[test]
fn a_find_by_rowid_is_not_reported() {
    let src = "\
DEFINE BUFFER bOrder FOR Order.
FOR EACH Order NO-LOCK:
    DO TRANSACTION:
        FIND bOrder WHERE ROWID(bOrder) = ROWID(Order) EXCLUSIVE-LOCK NO-ERROR.
    END.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_header_that_ends_with_a_period_is_quoted_alone() {
    let src = "\
FOR EACH Order NO-LOCK.
    FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(diags[0].message.contains("of `FOR EACH Order NO-LOCK`:"));
}
