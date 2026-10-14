//! `undefined-symbol` on class and type names: the type operand of `CAST`,
//! `TYPE-OF` and `GET-CLASS`.

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
