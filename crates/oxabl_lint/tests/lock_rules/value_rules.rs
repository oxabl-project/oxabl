use super::registry_run::codes_of;
use super::{check, check_no_schema, lines};
use oxabl_common::LintSeverityMap;
use oxabl_lint::{unknown_in_comparison as comparison, unknown_propagation as propagation};

#[test]
fn comparison_sources_guards_and_low_confidence_results() {
    let src = "DEFINE VARIABLE d AS DATE NO-UNDO.\n\
DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE b AS LOGICAL INITIAL ? NO-UNDO. DEFINE VARIABLE w AS LOGICAL INITIAL ? NO-UNDO.\n\
IF d < TODAY THEN MESSAGE 1.\n\
IF n = ? THEN MESSAGE 1.\n\
IF n <> ? AND n > 0 THEN MESSAGE 1.\n\
IF b THEN MESSAGE 1.\n\
DO WHILE w: LEAVE. END.\n\
b = n > 1.\n\
IF b THEN MESSAGE 1.\n\
b = NOT ?.\n\
IF b THEN MESSAGE 1.\n\
IF n = ? THEN RETURN.\n\
IF n > 1 THEN MESSAGE 1.\n";
    assert_eq!(
        lines(src, &check_no_schema(comparison::run, src)),
        [4, 7, 8, 9]
    );
}

#[test]
fn propagation_sinks_and_absorption() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE c AS CHARACTER INITIAL ? NO-UNDO. DEFINE VARIABLE m AS INTEGER NO-UNDO.\n\
MESSAGE n + 1.\n\
c = \"text\" + c.\n\
MESSAGE STRING(n + 1).\n\
IF n <> ? THEN MESSAGE n + 1.\n\
m = ABSOLUTE(n).\n\
MESSAGE m * 2.\n\
n = INTEGER(STRING(1)).\n\
MESSAGE n + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [3, 4, 8]);
}

#[test]
fn caller_values_and_unseen_returns_are_unknown() {
    let src = "DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.\n\
DEFINE VARIABLE d AS DATE NO-UNDO.\n\
MESSAGE n + 1.\n\
d = unseen().\n\
IF d < TODAY THEN MESSAGE 1.\n\
n = ?.\n\
MESSAGE n + 1.\n";
    assert!(check(comparison::run, src).is_empty());
    assert_eq!(lines(src, &check(propagation::run, src)), [7]);
}

#[test]
fn branches_loops_and_sequential_assignments() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE x AS INTEGER NO-UNDO. DEFINE VARIABLE k AS INTEGER NO-UNDO.\n\
DEFINE INPUT PARAMETER b AS LOGICAL NO-UNDO.\n\
IF b THEN n = 1.\n\
MESSAGE n + 1.\n\
ASSIGN k = ? x = k + 1 k = 1.\n\
MESSAGE k + 1.\n\
MESSAGE x + 1.\n\
REPEAT:\n\
 IF x = ? THEN NEXT.\n\
 MESSAGE x + 1.\n\
 LEAVE.\n\
END.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [5, 6, 8]);
}

#[test]
fn group_is_opt_in_and_defaults_do_not_change() {
    let src =
        "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO. MESSAGE n + 1. IF n > 1 THEN RETURN.";
    assert!(codes_of(src, LintSeverityMap::default(), "NL").is_empty());
    let mut map = LintSeverityMap::default();
    map.base_mut().enable_group("strict.nulls");
    assert_eq!(codes_of(src, map, "NL"), ["NL001", "NL002"]);
}

#[test]
fn writes_from_input_and_output_calls_forget_the_value() {
    let src = "DEFINE VARIABLE d AS DATE NO-UNDO.\n\
UPDATE d.\n\
IF d < TODAY THEN MESSAGE 1.\n\
d = ?.\n\
RUN test (OUTPUT d).\n\
IF d < TODAY THEN MESSAGE 1.\n";
    assert!(check(comparison::run, src).is_empty());
}

#[test]
fn method_parameters_can_acquire_unknown_values() {
    let src = "CLASS Example:\n\
METHOD PUBLIC VOID Test(INPUT n AS INTEGER):\n\
 MESSAGE n + 1.\n\
 n = ?.\n\
 MESSAGE n + 1.\n\
END METHOD.\n\
END CLASS.";
    assert_eq!(lines(src, &check(propagation::run, src)), [5]);
}

#[test]
fn infeasible_guards_do_not_create_sources() {
    let src = "DEFINE VARIABLE n AS INTEGER NO-UNDO.\n\
DEFINE VARIABLE q AS INTEGER INITIAL ? NO-UNDO.\n\
IF n = ? THEN MESSAGE n + 1.\n\
IF n > 0 THEN MESSAGE q + 1.\n\
IF FALSE THEN DO: q = ?. MESSAGE q + 1. END.\n\
IF q = ? THEN RETURN.\n\
q = ?.\n\
MESSAGE q + 1.\n";
    assert!(check(propagation::run, src).is_empty());
}

#[test]
fn conditional_values_and_builtin_chains() {
    let src = "DEFINE VARIABLE q AS INTEGER INITIAL ? NO-UNDO. DEFINE VARIABLE b AS LOGICAL INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
x = IF q <> ? THEN q ELSE 0.\n\
MESSAGE x + 1.\n\
x = IF b THEN ? ELSE 0.\n\
MESSAGE x + 1.\n\
MESSAGE LENGTH(q + 1).\n\
IF ABSOLUTE(q) > 1 THEN RETURN.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [7]);
    assert_eq!(lines(src, &check(comparison::run, src)), [8]);
}

#[test]
fn a_counted_loop_can_end_before_its_while_condition_is_false() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
DO i = 1 TO 2 WHILE n <> ?: MESSAGE n + 1. END.\n\
MESSAGE n + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [4]);
}

#[test]
fn a_guard_can_follow_the_ordering_comparison() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO. DEFINE VARIABLE m AS INTEGER INITIAL ? NO-UNDO.\n\
IF n > 1 OR n = ? THEN MESSAGE 1.\n\
IF n > 1 AND n <> ? THEN MESSAGE 1.\n\
IF n > 1 OR n <> ? THEN MESSAGE 1.\n\
IF m > 1 AND m = ? THEN MESSAGE 1.\n";
    assert_eq!(lines(src, &check(comparison::run, src)), [4, 5]);
}

#[test]
fn unseen_expression_calls_can_write_their_arguments() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
x = unseen(OUTPUT n).\n\
MESSAGE n + 1.\n";
    assert!(check(propagation::run, src).is_empty());
}

#[test]
fn a_call_forgets_locals_that_another_unit_writes() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
RUN resetValue.\n\
MESSAGE n + 1.\n\
PROCEDURE resetValue: n = 1. END PROCEDURE.\n";
    assert!(check(propagation::run, src).is_empty());
}

#[test]
fn a_proven_source_survives_an_unknown_path_at_a_join() {
    let src = "DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.\n\
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.\n\
IF choice THEN n = ?.\n\
MESSAGE n + 1.\n\
IF n > 1 THEN MESSAGE 1.\n\
n = unseen().\n\
MESSAGE n + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [4]);
    assert_eq!(lines(src, &check(comparison::run, src)), [5]);
}

#[test]
fn a_true_ordering_test_proves_its_operands_known() {
    let src = "DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.\n\
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.\n\
IF choice THEN n = ?.\n\
IF n > 0 THEN MESSAGE n + 1.\n\
MESSAGE n + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [5]);
    assert_eq!(lines(src, &check(comparison::run, src)), [4]);
}

#[test]
fn builtin_abbreviations_keep_the_same_propagation() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO. DEFINE VARIABLE m AS INTEGER NO-UNDO.\n\
n = INT(n).\n\
n = ABS(n).\n\
MESSAGE n + 1.\n\
m = MIN(n, 1).\n\
MESSAGE m + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [4, 6]);
}

#[test]
fn paired_initial_values_do_not_escape_their_initialization_arm() {
    let src = "DEFINE VARIABLE a AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE b AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.\n\
REPEAT:\n\
 IF a = ? AND b = ? THEN ASSIGN a = unseen() b = unseen().\n\
 ELSE IF b > a THEN MESSAGE 1.\n\
 IF choice THEN LEAVE.\n\
END.\n";
    assert!(check(comparison::run, src).is_empty());
}

#[test]
fn replacing_one_value_breaks_a_source_dependency() {
    let src = "DEFINE VARIABLE a AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE b AS INTEGER INITIAL ? NO-UNDO.\n\
a = unseen().\n\
IF a <> ? THEN MESSAGE b + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [4]);
}

#[test]
fn dependencies_must_hold_on_every_path_that_carries_the_source() {
    let src = "DEFINE VARIABLE a AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE b AS INTEGER INITIAL ? NO-UNDO.\n\
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.\n\
IF choice THEN a = 1.\n\
IF a <> ? THEN MESSAGE b + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [5]);
}

#[test]
fn a_guard_also_removes_a_copied_source() {
    let src = "DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.\n\
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.\n\
DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
IF choice THEN n = ?.\n\
x = n.\n\
IF n <> ? THEN MESSAGE x + 1.\n";
    assert!(check(propagation::run, src).is_empty());
}

#[test]
fn equal_lengths_keep_the_same_unknown_status() {
    let src = "DEFINE INPUT PARAMETER a AS CHARACTER NO-UNDO.\n\
DEFINE INPUT PARAMETER b AS CHARACTER NO-UNDO.\n\
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.\n\
IF choice THEN b = ?.\n\
IF LENGTH(a) = LENGTH(b) THEN DO:\n\
 IF a <> ? THEN MESSAGE b + \"text\".\n\
END.\n";
    assert!(check(propagation::run, src).is_empty());
}

#[test]
fn a_variable_is_reported_once_per_unit_and_rule() {
    let src = "DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
MESSAGE n + 1.\n\
MESSAGE n + 2.\n\
IF n > 1 THEN RETURN.\n\
IF n > 2 THEN RETURN.\n\
PROCEDURE p:\n\
 DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
 MESSAGE n + 1.\n\
END PROCEDURE.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [2, 8]);
    assert_eq!(lines(src, &check(comparison::run, src)), [4]);
}

#[test]
fn a_test_of_the_same_record_correlates_branches() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD d AS DATE.\n\
DEFINE VARIABLE d AS DATE NO-UNDO. DEFINE VARIABLE e AS DATE NO-UNDO.\n\
DEFINE VARIABLE g AS DATE NO-UNDO.\n\
FIND FIRST tt NO-ERROR.\n\
IF AVAILABLE tt THEN d = TODAY.\n\
IF AVAILABLE tt AND d > TODAY THEN MESSAGE 1.\n\
IF AVAILABLE tt THEN MESSAGE d + 1.\n\
IF AVAILABLE tt THEN e = TODAY.\n\
FIND NEXT tt NO-ERROR.\n\
IF AVAILABLE tt THEN MESSAGE e + 1.\n\
IF AVAILABLE tt THEN g = TODAY.\n\
IF NOT AVAILABLE tt THEN RETURN.\n\
MESSAGE g + 1.\n";
    assert!(check(comparison::run, src).is_empty());
    assert_eq!(lines(src, &check(propagation::run, src)), [10]);
}

#[test]
fn a_logical_flag_correlates_branches() {
    let src = "DEFINE VARIABLE d AS DATE NO-UNDO.\n\
DEFINE VARIABLE e AS DATE NO-UNDO.\n\
DEFINE VARIABLE found AS LOGICAL NO-UNDO.\n\
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.\n\
found = FALSE.\n\
IF choice THEN ASSIGN found = TRUE d = TODAY.\n\
IF found THEN MESSAGE d + 1.\n\
IF choice THEN ASSIGN found = TRUE e = TODAY.\n\
found = choice.\n\
IF found THEN MESSAGE e + 1.\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [10]);
}

#[test]
fn a_counted_loop_with_an_unknown_limit_runs_no_iteration() {
    let src = "DEFINE VARIABLE c AS CHARACTER INITIAL ? NO-UNDO.\n\
DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
DO i = 1 TO NUM-ENTRIES(c):\n\
 MESSAGE c + \"x\".\n\
END.\n\
MESSAGE c + \"x\".\n";
    assert_eq!(lines(src, &check(propagation::run, src)), [6]);
}
