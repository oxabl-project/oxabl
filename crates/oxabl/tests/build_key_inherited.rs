//! A subclass sees the members it inherits from workspace superclasses, so
//! `name:Method()` on an inherited variable is no class reference and the unit
//! gets a key. The tests drive `oxabl build --plan --probe` with the fake AVM.

mod common;

use common::keyed::{Keyed, Plan};

const BASE: &str = "CLASS pkg.Base:\n\
    \x20   DEFINE PROTECTED VARIABLE req_ AS Progress.Lang.Object NO-UNDO.\n\
    \x20   DEFINE PUBLIC PROPERTY prop_ AS Progress.Lang.Object NO-UNDO GET. SET.\n\
    \x20   DEFINE PRIVATE VARIABLE secret_ AS Progress.Lang.Object NO-UNDO.\n\
    END CLASS.\n";

/// A subclass in its own package that imports the superclass's package, the
/// shape in which a miss on a bare name is read as a class name.
fn sub(parent: &str, body: &str) -> String {
    format!(
        "USING pkg.*.\n\
         CLASS app.Sub INHERITS {parent}:\n\
         \x20   METHOD PUBLIC VOID Run():\n\
         \x20       {body}\n\
         \x20   END METHOD.\n\
         END CLASS.\n"
    )
}

const SUB: &str = "src/app/Sub.cls";

fn plan_of(files: &[(&str, &str)]) -> Plan {
    let workspace = Keyed::simple();
    for (path, text) in files {
        workspace.write(path, text);
    }
    workspace.plan()
}

fn assert_keyed(plan: &Plan) {
    assert!(
        plan.key(SUB).is_some(),
        "{SUB} has no key: {}",
        plan.unit(SUB)
    );
    assert!(plan.unkeyable(SUB).is_empty(), "{:?}", plan.unkeyable(SUB));
}

#[test]
fn an_inherited_protected_variable_used_as_an_object_gives_a_key() {
    let plan = plan_of(&[
        ("src/pkg/Base.cls", BASE),
        (
            SUB,
            &sub("pkg.Base", "IF req_:ToString() EQ \"x\" THEN RETURN."),
        ),
    ]);

    assert_keyed(&plan);
}

#[test]
fn an_inherited_public_property_used_as_an_object_gives_a_key() {
    let plan = plan_of(&[
        ("src/pkg/Base.cls", BASE),
        (
            SUB,
            &sub("pkg.Base", "IF prop_:ToString() EQ \"x\" THEN RETURN."),
        ),
    ]);

    assert_keyed(&plan);
}

#[test]
fn a_member_inherited_through_several_levels_gives_a_key() {
    let plan = plan_of(&[
        ("src/pkg/Base.cls", BASE),
        (
            "src/pkg/Mid.cls",
            "CLASS pkg.Mid INHERITS pkg.Base:\nEND CLASS.\n",
        ),
        (
            SUB,
            &sub("pkg.Mid", "IF req_:ToString() EQ \"x\" THEN RETURN."),
        ),
    ]);

    assert_keyed(&plan);
}

#[test]
fn a_name_that_is_a_class_still_resolves_as_one() {
    let plan = plan_of(&[
        ("src/pkg/Base.cls", BASE),
        ("src/pkg/Util.cls", "CLASS pkg.Util:\nEND CLASS.\n"),
        (
            SUB,
            &sub("pkg.Base", "IF pkg.Util:ToString() EQ \"x\" THEN RETURN."),
        ),
    ]);

    assert_keyed(&plan);
}

#[test]
fn a_name_no_ancestor_declares_stays_an_unresolved_class() {
    let plan = plan_of(&[
        ("src/pkg/Base.cls", BASE),
        (
            SUB,
            &sub("pkg.Base", "IF nothing_:ToString() EQ \"x\" THEN RETURN."),
        ),
    ]);

    assert_eq!(plan.key(SUB), None, "{}", plan.unit(SUB));
    assert!(
        plan.unkeyable(SUB)
            .contains(&"unresolved-class: nothing_ in src/app/Sub.cls".to_string()),
        "{:?}",
        plan.unkeyable(SUB)
    );
}

#[test]
fn a_superclass_oxabl_cannot_read_keeps_the_unit_unkeyable() {
    let plan = plan_of(&[(
        SUB,
        &sub("pkg.Base", "IF req_:ToString() EQ \"x\" THEN RETURN."),
    )]);

    assert_eq!(plan.key(SUB), None, "{}", plan.unit(SUB));
    assert!(
        plan.unkeyable(SUB)
            .contains(&"unresolved-class: pkg.base in src/app/Sub.cls".to_string()),
        "{:?}",
        plan.unkeyable(SUB)
    );
}
