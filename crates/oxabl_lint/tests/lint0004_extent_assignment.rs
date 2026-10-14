//! A scalar assigned to an `EXTENT` variable assigns every element, so
//! `type-mismatch-assignment` judges it against the element type.

mod support;

use support::lint0004_without_index;

#[test]
fn scalar_to_extent_variable_is_clean() {
    let src = "DEFINE VARIABLE a AS INTEGER NO-UNDO EXTENT 5.\na = 0.\n";
    assert!(lint0004_without_index(src).is_empty());
}

#[test]
fn scalar_to_dynamic_extent_variable_is_clean() {
    let src = "DEFINE VARIABLE a AS INTEGER NO-UNDO EXTENT.\na = 0.\n";
    assert!(lint0004_without_index(src).is_empty());
}

#[test]
fn scalar_to_extent_variable_in_assign_is_clean() {
    let src = "DEFINE VARIABLE a AS INTEGER NO-UNDO EXTENT 5.\nASSIGN a = 0.\n";
    assert!(lint0004_without_index(src).is_empty());
}

#[test]
fn widening_scalar_to_extent_variable_is_clean() {
    let src = "DEFINE VARIABLE a AS DECIMAL NO-UNDO EXTENT 5.\na = 1.\n";
    assert!(lint0004_without_index(src).is_empty());
}

#[test]
fn wrong_typed_scalar_to_extent_variable_still_mismatches() {
    let src = "DEFINE VARIABLE a AS INTEGER NO-UNDO EXTENT 5.\na = \"abc\".\n";
    let diags = lint0004_without_index(src);
    assert_eq!(diags.len(), 1, "{diags:?}");
}

#[test]
fn scalar_initial_value_on_extent_variable_is_clean() {
    let src = "DEFINE VARIABLE a AS INTEGER NO-UNDO EXTENT 5 INITIAL 0.\n";
    assert!(lint0004_without_index(src).is_empty());
}

#[test]
fn extent_to_scalar_still_mismatches() {
    let src = "DEFINE VARIABLE a AS INTEGER NO-UNDO EXTENT 5.\nDEFINE VARIABLE s AS INTEGER NO-UNDO.\ns = a.\n";
    let diags = lint0004_without_index(src);
    assert_eq!(diags.len(), 1, "{diags:?}");
}
