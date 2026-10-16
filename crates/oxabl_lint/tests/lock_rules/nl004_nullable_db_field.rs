//! `nullable-db-field` (NL004) over `oxabl_nulls`.

use oxabl_common::LintSeverityMap;
use oxabl_lint::groups;
use oxabl_lint::nullable_db_field::run;
use oxabl_lint::{unknown_in_comparison, unknown_propagation};

use super::registry_run::codes_of;
use super::{check, check_fragment, check_no_schema, check_recovered, help, lines};

const HEAD: &str = "FIND FIRST Parcel NO-LOCK NO-ERROR.\nIF NOT AVAILABLE Parcel THEN RETURN.\n";

fn parcel(body: &str) -> String {
    format!("{HEAD}{body}")
}

#[test]
fn only_a_nonmandatory_field_that_starts_at_unknown_is_a_source() {
    let src = parcel(
        "IF Parcel.ShipDate < TODAY THEN MESSAGE 1.\n\
IF Parcel.DueDate < TODAY THEN MESSAGE 2.\n\
IF Parcel.Qty < 1 THEN MESSAGE 3.\n\
MESSAGE Parcel.Weight * 2.\n\
MESSAGE Parcel.Qty * 2.\n",
    );
    let diags = check(run, &src);
    assert_eq!(lines(&src, &diags), [3, 6]);
    assert!(diags[0].message.contains("`Parcel.ShipDate` may hold `?`"));
    assert!(diags[0].message.contains("comparison or condition"));
    assert!(diags[1].message.contains("`Parcel.Weight`"));
    assert!(diags[1].message.contains("arithmetic result"));
    assert!(help(&diags[0]).contains("Parcel.ShipDate <> ?"));
    assert!(help(&diags[0]).contains("MANDATORY"));
}

#[test]
fn no_schema_gives_no_finding() {
    let src = parcel("IF Parcel.ShipDate < TODAY THEN MESSAGE 1.\n");
    assert!(check_no_schema(run, &src).is_empty());
}

#[test]
fn a_recovered_parse_gives_no_finding() {
    let src = parcel("IF Parcel.ShipDate < TODAY THEN MESSAGE 1.\nIF THEN.\n");
    assert!(check_recovered(run, &src).is_empty());
}

#[test]
fn an_unqualified_field_and_a_buffer_name_resolve_through_the_scopes() {
    let src = "DEFINE BUFFER p FOR Parcel.\n\
FIND FIRST p NO-LOCK NO-ERROR.\n\
IF NOT AVAILABLE p THEN RETURN.\n\
IF p.ShipDate < TODAY THEN MESSAGE 1.\n\
FOR EACH Parcel NO-LOCK:\n\
 IF Weight > 1 THEN MESSAGE 2.\n\
END.\n";
    assert_eq!(lines(src, &check(run, src)), [4, 6]);
}

#[test]
fn guards_remove_the_source() {
    let src = parcel(
        "IF Parcel.ShipDate <> ? AND Parcel.ShipDate < TODAY THEN MESSAGE 1.\n\
IF Parcel.ShipDate = ? OR Parcel.ShipDate < TODAY THEN MESSAGE 2.\n\
IF Parcel.Weight = ? THEN RETURN.\n\
MESSAGE Parcel.Weight * 2.\n",
    );
    assert!(check(run, &src).is_empty());
    let src = parcel(
        "IF Parcel.Weight = ? THEN MESSAGE Parcel.Weight + 1.\n\
IF Parcel.ShipDate < TODAY THEN MESSAGE 1.\n",
    );
    assert_eq!(lines(&src, &check(run, &src)), [3, 4]);
}

#[test]
fn a_record_that_is_read_again_may_hold_unknown_again() {
    let src = parcel(
        "IF Parcel.Weight = ? THEN RETURN.\n\
IF Parcel.ShipDate = ? THEN RETURN.\n\
MESSAGE Parcel.Weight + 1.\n\
FIND NEXT Parcel NO-LOCK NO-ERROR.\n\
MESSAGE Parcel.Weight + 2.\n\
CREATE Parcel.\n\
MESSAGE Parcel.ShipDate - 3.\n",
    );
    assert_eq!(lines(&src, &check(run, &src)), [7, 9]);
}

#[test]
fn a_write_to_the_record_leaves_its_fields_unknown() {
    let src = parcel(
        "Parcel.ShipDate = TODAY.\n\
MESSAGE Parcel.ShipDate - 1.\n\
Parcel.Weight = ?.\n\
MESSAGE Parcel.Weight * 2.\n\
UPDATE Parcel.Weight.\n\
MESSAGE Parcel.Weight * 3.\n",
    );
    assert_eq!(lines(&src, &check(run, &src)), [6]);
}

#[test]
fn a_buffer_passed_to_a_call_leaves_its_fields_unknown() {
    let src = parcel(
        "IF Parcel.Weight = ? THEN RETURN.\n\
RUN fill (BUFFER Parcel).\n\
MESSAGE Parcel.Weight * 2.\n",
    );
    assert!(check(run, &src).is_empty());
}

#[test]
fn the_where_of_a_read_holds_of_the_record() {
    let src = "FOR EACH Parcel NO-LOCK WHERE Parcel.ShipDate < TODAY:\n\
 MESSAGE Parcel.ShipDate - 1.\n\
END.\n\
FIND FIRST Parcel WHERE Parcel.Weight > 1 NO-LOCK NO-ERROR.\n\
IF AVAILABLE Parcel THEN MESSAGE Parcel.Weight * 2.\n\
FOR EACH Parcel NO-LOCK:\n\
 IF Parcel.Weight < 1 THEN MESSAGE 1.\n\
END.\n";
    assert_eq!(lines(src, &check(run, src)), [7]);
}

#[test]
fn a_loop_loads_a_record_on_each_pass() {
    let src = "FOR EACH Parcel NO-LOCK:\n\
 IF Parcel.ShipDate = ? THEN NEXT.\n\
 MESSAGE Parcel.ShipDate - 1.\n\
END.\n\
FOR EACH Parcel NO-LOCK:\n\
 MESSAGE Parcel.ShipDate - 2.\n\
 IF Parcel.ShipDate = ? THEN LEAVE.\n\
END.\n";
    assert_eq!(lines(src, &check(run, src)), [6]);
}

#[test]
fn a_field_is_reported_once_per_unit_and_kind() {
    let src = parcel(
        "IF Parcel.ShipDate < TODAY THEN MESSAGE 1.\n\
IF Parcel.ShipDate < TODAY - 1 THEN MESSAGE 2.\n\
MESSAGE Parcel.Weight * 2.\n\
MESSAGE Parcel.Weight * 3.\n\
IF Parcel.Weight > 1 THEN MESSAGE 4.\n\
PROCEDURE other:\n\
 FIND FIRST Parcel NO-LOCK NO-ERROR.\n\
 IF AVAILABLE Parcel THEN MESSAGE Parcel.Weight * 4.\n\
END PROCEDURE.\n",
    );
    assert_eq!(lines(&src, &check(run, &src)), [3, 5, 7, 10]);
}

#[test]
fn a_shared_buffer_is_forgotten_by_a_call() {
    let src = "DEFINE SHARED BUFFER Parcel FOR Parcel.\n\
IF Parcel.Weight = ? THEN RETURN.\n\
MESSAGE Parcel.Weight + 1.\n\
RUN other.\n\
MESSAGE Parcel.Weight + 2.\n";
    assert!(check(run, src).is_empty());
}

#[test]
fn locals_and_the_other_value_rules_do_not_change() {
    let src = parcel(
        "DEFINE VARIABLE d AS DATE NO-UNDO.\n\
d = Parcel.ShipDate.\n\
IF d < TODAY THEN MESSAGE 1.\n\
MESSAGE Parcel.Weight * 2.\n",
    );
    assert!(check(unknown_in_comparison::run, &src).is_empty());
    assert_eq!(lines(&src, &check(unknown_propagation::run, &src)), [] as [usize; 0]);
    assert_eq!(lines(&src, &check(run, &src)), [6]);
}

#[test]
fn an_include_checked_alone_is_judged() {
    let src = "FIND FIRST Parcel NO-LOCK NO-ERROR.\nIF Parcel.ShipDate < TODAY THEN MESSAGE 1.\n";
    assert_eq!(lines(src, &check_fragment(run, src)), [2]);
}

#[test]
fn the_group_enables_the_rule() {
    let src = parcel("IF Parcel.ShipDate < TODAY THEN MESSAGE 1.\n");
    assert!(codes_of(&src, LintSeverityMap::new(), "NL").is_empty());
    let mut map = LintSeverityMap::new();
    map.base_mut().enable_group(groups::STRICT_NULLS);
    assert_eq!(codes_of(&src, map, "NL"), ["NL004"]);
}
