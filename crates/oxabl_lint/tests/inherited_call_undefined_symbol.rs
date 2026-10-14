//! An unqualified call to a method a *resolvable* supertype declares is not an
//! undefined symbol, however deep in the chain it is declared and wherever the
//! call sits: a statement, an argument, or an argument of a call chain.

mod support;

use support::*;

const BASE: &str = r#"CLASS orders.base:
    METHOD PROTECTED CHARACTER Body():
        RETURN "".
    END METHOD.
    METHOD PROTECTED CHARACTER Flag(INPUT pc AS CHARACTER):
        RETURN pc.
    END METHOD.
    DEFINE PROTECTED PROPERTY Level AS INTEGER NO-UNDO GET. SET.
END CLASS."#;

const MIDDLE: &str = "CLASS orders.middle INHERITS orders.base: END CLASS.";

const BASE_PATH: &str = "/src/orders/base.cls";
const MIDDLE_PATH: &str = "/src/orders/middle.cls";

const CHILD: &str = r#"CLASS orders.child INHERITS orders.middle:
    METHOD OVERRIDE PROTECTED CHARACTER Body():
        DEFINE VARIABLE c AS CHARACTER NO-UNDO.
        DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.
        c = SUPER:Body().
        c = SUPER:Flag(c).
        c = THIS-OBJECT:Flag(c).
        o = NEW orders.base():Create(Body()):Get(Flag("x")).
        Level = Level + 1.
        RETURN Flag(Flag(c)).
    END METHOD.
END CLASS."#;

#[test]
fn inherited_calls_resolve_through_a_two_level_chain() {
    let workspace = [(BASE_PATH, BASE), (MIDDLE_PATH, MIDDLE)];
    let found = lint0001_with_index(CHILD, &workspace);
    assert!(found.is_empty(), "unexpected LINT0001: {found:?}");
}

#[test]
fn a_name_the_chain_does_not_declare_is_still_undefined() {
    let workspace = [(BASE_PATH, BASE), (MIDDLE_PATH, MIDDLE)];
    let child = r#"CLASS orders.child INHERITS orders.middle:
    METHOD PUBLIC VOID go():
        MESSAGE Body() Nope().
    END METHOD.
END CLASS."#;
    let found = lint0001_with_index(child, &workspace);
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].message.contains("Nope"), "{found:?}");
}

#[test]
fn an_override_property_does_not_hide_the_members_declared_after_it() {
    let workspace = [(BASE_PATH, BASE), (MIDDLE_PATH, MIDDLE)];
    let child = r#"CLASS orders.child INHERITS orders.middle:
    DEFINE PROTECTED OVERRIDE PROPERTY Level AS INTEGER NO-UNDO
        GET():
            RETURN 1.
        END GET.
    CONSTRUCTOR PUBLIC child():
        MESSAGE Flag("x").
    END CONSTRUCTOR.
    METHOD PUBLIC VOID go():
        MESSAGE Body() Flag("x").
    END METHOD.
END CLASS."#;
    let found = lint0001_with_index(child, &workspace);
    assert!(found.is_empty(), "unexpected LINT0001: {found:?}");
}
