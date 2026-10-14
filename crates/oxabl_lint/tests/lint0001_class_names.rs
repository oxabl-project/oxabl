//! `undefined-symbol` on class and type names: the type operand of `CAST`,
//! `TYPE-OF` and `GET-CLASS`, and unqualified names in a class whose ancestors
//! were not all read.

mod support;

use oxabl_semantic::{Resolution, SymbolKind, UnresolvedReason};

use support::*;

const CALC_BASE_PATH: &str = "/src/orders/calc-base.cls";
const CALC_BASE: &str = "CLASS orders.calc-base:
    METHOD PUBLIC INTEGER calc-total():
        RETURN 0.
    END METHOD.
END CLASS.";

// ---------------------------------------------------------------------------
// Type operands
// ---------------------------------------------------------------------------

/// The three type operands, each naming `JsonObject` through a shipped wildcard.
const SHIPPED_OPERANDS: &str = "USING Progress.Json.ObjectModel.*.
CLASS repro:
    METHOD PUBLIC VOID go (INPUT x AS Progress.Lang.Object):
        DEFINE VARIABLE r AS Progress.Lang.Object NO-UNDO.
        DEFINE VARIABLE l AS LOGICAL NO-UNDO.
        r = CAST(x, JsonObject).
        l = TYPE-OF(x, JsonObject).
        r = GET-CLASS(JsonObject).
        MESSAGE VALID-OBJECT(r) l.
    END METHOD.
END CLASS.";

#[test]
fn a_type_operand_is_not_a_value() {
    assert!(lint0001_with_index(SHIPPED_OPERANDS, &[]).is_empty());
    assert!(lint0001_without_index(SHIPPED_OPERANDS).is_empty());
    let (_stmts, sem) = with_index(SHIPPED_OPERANDS, &[]);
    assert_eq!(
        unresolved_reasons(&sem, "jsonobject"),
        vec![UnresolvedReason::External; 3]
    );
}

#[test]
fn a_type_operand_resolves_to_the_workspace_class() {
    let source = "USING orders.*.
DEFINE VARIABLE x AS Progress.Lang.Object NO-UNDO.
x = CAST(x, calc-base).
MESSAGE TYPE-OF(x, orders.calc-base) VALID-OBJECT(GET-CLASS(calc-base)).
";
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];
    let (_stmts, sem) = with_index(source, &workspace);
    let class = sole_symbol(&sem, "orders.calc-base");
    assert_eq!(sem.symbols.get(class).kind, SymbolKind::Class);
    let resolutions = sem
        .references
        .iter()
        .filter(|(_, r)| matches!(r, Resolution::Resolved(s) if *s == class))
        .count();
    assert_eq!(resolutions, 3, "each operand names the class");
    assert!(lint0001_with_index(source, &workspace).is_empty());
}

#[test]
fn a_type_operand_nothing_supplies_is_still_reported() {
    let source = "DEFINE VARIABLE x AS Progress.Lang.Object NO-UNDO.
x = CAST(x, no-such-class).
";
    let found = lint0001_with_index(source, &[]);
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].message.contains("no-such-class"));
}

#[test]
fn the_value_operand_of_a_cast_is_still_a_value() {
    let source = "USING Progress.Json.ObjectModel.*.
DEFINE VARIABLE x AS Progress.Lang.Object NO-UNDO.
x = CAST(no-such-var, JsonObject).
";
    let found = lint0001_with_index(source, &[]);
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].message.contains("no-such-var"));
}

#[test]
fn a_user_function_named_like_a_type_function_takes_values() {
    let source = "FUNCTION type-of RETURNS LOGICAL (INPUT a AS INTEGER, INPUT b AS INTEGER):
    RETURN TRUE.
END FUNCTION.
MESSAGE type-of(1, no-such-var).
";
    let found = lint0001_with_index(source, &[]);
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].message.contains("no-such-var"));
}

// ---------------------------------------------------------------------------
// Unqualified names under an unread ancestor
// ---------------------------------------------------------------------------

/// A class whose parent no file supplies.
const ORPHAN: &str = "CLASS Foo.Bar INHERITS Foo.MissingParent:
    METHOD PUBLIC VOID DoWork():
        DoInheritedThing().
        myInheritedProp = 1.
    END METHOD.
END CLASS.";

#[test]
fn a_name_under_a_parent_no_file_supplies_is_unknowable() {
    assert!(lint0001_with_index(ORPHAN, &[]).is_empty());
    assert!(lint0001_without_index(ORPHAN).is_empty());
    let (_stmts, sem) = with_index(ORPHAN, &[]);
    for name in ["doinheritedthing", "myinheritedprop"] {
        assert_eq!(
            unresolved_reasons(&sem, name),
            vec![UnresolvedReason::External],
            "{name}"
        );
    }
}

#[test]
fn a_name_under_a_grandparent_no_file_supplies_is_unknowable() {
    let source = "CLASS orders.child INHERITS orders.calc-mid:
    METHOD PUBLIC VOID run-it():
        base-total().
    END METHOD.
END CLASS.";
    let workspace = [(
        "/src/orders/calc-mid.cls",
        "CLASS orders.calc-mid INHERITS orders.gone: END CLASS.",
    )];
    assert!(lint0001_with_index(source, &workspace).is_empty());
}

#[test]
fn a_name_under_a_shipped_parent_is_unknowable() {
    let source = "CLASS orders.my-error INHERITS Progress.Lang.AppError:
    METHOD PUBLIC VOID raise-it():
        AddMessage(\"x\", 1).
    END METHOD.
END CLASS.";
    assert!(lint0001_with_index(source, &[]).is_empty());
    assert!(lint0001_without_index(source).is_empty());
}

#[test]
fn a_name_under_a_parent_the_index_reads_is_still_reported() {
    let source = "CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        calc-totl().
    END METHOD.
END CLASS.";
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];
    assert_eq!(lint0001_with_index(source, &workspace).len(), 1);
}

#[test]
fn a_missing_interface_or_the_root_class_hides_no_member() {
    // A class declares its interfaces' members itself, and every class has
    // `Progress.Lang.Object` as its root whether it names it or not.
    for header in [
        "CLASS orders.impl IMPLEMENTS orders.i-gone:",
        "CLASS orders.impl INHERITS Progress.Lang.Object:",
        "CLASS orders.impl:",
    ] {
        let source = format!(
            "{header}
    METHOD PUBLIC VOID run-it():
        no-such-method().
    END METHOD.
END CLASS."
        );
        assert_eq!(lint0001_with_index(&source, &[]).len(), 1, "{header}");
        assert_eq!(lint0001_without_index(&source).len(), 1, "{header}");
    }
}

#[test]
fn a_receiver_under_an_unread_parent_still_resolves_as_a_class() {
    let source = "USING orders.*.
CLASS orders.child INHERITS orders.gone:
    METHOD PUBLIC VOID run-it():
        MESSAGE calc-base:calc-total().
    END METHOD.
END CLASS.";
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];
    let (_stmts, sem) = with_index(source, &workspace);
    assert_eq!(resolved_to(&sem, "orders.calc-base").len(), 1);
    assert!(lint0001_with_index(source, &workspace).is_empty());
}
