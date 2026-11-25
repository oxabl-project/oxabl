//! `builtin_may_declare` reads the catalog of built-in types: a complete type
//! answers from its members and those of its supertypes, any other type may
//! declare any method.

use oxabl_index::hierarchy::builtin_may_declare;
use oxabl_semantic::IndexName;

fn may(class: &str, method: &str) -> bool {
    builtin_may_declare(class, &IndexName::new(method))
}

#[test]
fn a_complete_type_declares_only_the_methods_of_its_levels() {
    // Own, inherited from `ProError`, and from `Progress.Lang.Object`.
    assert!(may("Progress.Lang.AppError", "AddMessage"));
    assert!(may("Progress.Lang.AppError", "getmessage"));
    assert!(may("Progress.Lang.AppError", "ToString"));
    assert!(!may("Progress.Lang.AppError", "NoSuchMethod"));
    // A property is no method.
    assert!(!may("Progress.Lang.AppError", "NumMessages"));
    // An interface has the methods of `Progress.Lang.Object`.
    assert!(may("Progress.Lang.Error", "ToString"));
    assert!(may("Progress.Lang.Object", "Clone"));
    assert!(!may("Progress.Lang.Object", "Describe"));
}

#[test]
fn a_type_the_catalog_does_not_have_whole_may_declare_any_method() {
    assert!(may("Progress.Lang.Class", "NoSuchMethod"));
    assert!(may("Progress.Collections.List", "NoSuchMethod"));
    assert!(may("No.Such.Type", "Anything"));
}
