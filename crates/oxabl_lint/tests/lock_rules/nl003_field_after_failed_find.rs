//! `field-after-failed-find` (NL003).

use oxabl_common::LintSeverityMap;
use oxabl_lint::field_after_failed_find::run;
use oxabl_lint::groups;

use super::registry_run::codes_of;
use super::{check, check_fragment, check_no_schema, check_recovered, help, lines};

#[test]
fn a_field_after_a_failed_find_is_reported_once_per_path() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR.
c = Customer.Name.
c = Customer.Name.
FIND Item WHERE Item.ItemNum = 1 EXCLUSIVE-LOCK NO-ERROR.
ASSIGN Item.Price = 0.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3, 6]);
    assert!(diags[0].message.contains("`Customer.Name` is read"));
    assert!(
        diags[0]
            .message
            .contains("`FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR`")
    );
    assert!(diags[1].message.contains("`Item.Price` is assigned"));
    assert!(help(&diags[0]).contains("IF AVAILABLE Customer THEN"));
}

#[test]
fn guards_and_refills_are_not_reported() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FIND FIRST Customer NO-LOCK NO-ERROR.
IF AVAILABLE Customer THEN c = Customer.Name.
IF NOT AVAILABLE Customer THEN RETURN.
c = Customer.Name.
FIND FIRST Order NO-LOCK NO-ERROR.
IF NOT AVAILABLE Order THEN CREATE Order.
Order.CustNum = 1.
REPEAT:
    FIND NEXT Item NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Item THEN LEAVE.
    c = STRING(Item.Price).
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_arm_that_rejoins_is_reported() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FIND FIRST Customer NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN MESSAGE \"none\".
c = Customer.Name.
";
    assert_eq!(lines(src, &check(run, src)), [4]);
}

#[test]
fn several_finds_are_counted_in_the_message() {
    let src = "\
DEFINE VARIABLE n AS INTEGER NO-UNDO.
FIND FIRST Order NO-LOCK NO-ERROR.
DO WHILE AVAILABLE Order:
    FIND NEXT Order NO-LOCK NO-ERROR.
END.
n = Order.OrderNum.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [6]);
    assert!(diags[0].message.contains("(or 1 other)"));
}

#[test]
fn unknown_is_silent() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FIND FIRST Customer NO-LOCK NO-ERROR.
c = Customer.Name.
";
    // No schema: a database table is not judged.
    assert!(check_no_schema(run, src).is_empty());
    let broken = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FIND FIRST Customer NO-LOCK NO-ERROR.
c = Customer.Name.
DISPLAY (.
";
    assert!(check_recovered(run, broken).is_empty());
    let passed = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FIND FIRST Customer NO-LOCK NO-ERROR.
RUN fill (BUFFER Customer).
c = Customer.Name.
";
    assert!(check(run, passed).is_empty());
}

#[test]
fn temp_tables_are_judged_without_a_schema() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS INTEGER.
DEFINE VARIABLE n AS INTEGER NO-UNDO.
FIND FIRST tt NO-ERROR.
n = tt.k.
";
    assert_eq!(lines(src, &check_no_schema(run, src)), [4]);
}

#[test]
fn an_include_checked_alone_is_judged() {
    let src = "\
FIND FIRST Customer NO-LOCK NO-ERROR.
DISPLAY Customer.Name.
";
    assert_eq!(lines(src, &check_fragment(run, src)), [2]);
}

#[test]
fn the_group_enables_the_rule() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FIND FIRST Customer NO-LOCK NO-ERROR.
c = Customer.Name.
";
    assert!(codes_of(src, LintSeverityMap::new(), "NL").is_empty());
    let mut map = LintSeverityMap::new();
    map.base_mut().enable_group(groups::STRICT_NULLS);
    assert_eq!(codes_of(src, map, "NL"), ["NL003"]);
}
