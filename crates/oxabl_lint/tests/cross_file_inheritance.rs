//! Cross-file inheritance: what a class in another file contributes to the file
//! under analysis, and what it must not.
//!
//! These run the real [`BatchIndex`] over an in-memory filesystem rather than a
//! hand-written stub, so what they pin is the resolution every client gets rather
//! than a shape only this test can produce.
//!
//! Two properties are asserted over and over, because they are the ones a later
//! change is most likely to break:
//!
//! 1. **With no index attached nothing changes.** Every scenario is run twice —
//!    once with an index, once without — and the no-index run must produce
//!    today's answer. That is the R11 firewall, and it is why the shared helpers
//!    in `support` take the workspace as an argument rather than hard-coding one.
//! 2. **A synthesized cross-file symbol is not in the scope tree.** It is
//!    reachable only through the reference entry that deliberately points at it,
//!    so a bare mention of an inherited member's name outside the subclass stays
//!    unresolved.

mod support;

use oxabl_ast::NodeId;
use oxabl_lint::LINT0004;
use oxabl_semantic::{NamespaceId, PrimitiveTy, ResolvedType, SymbolKind, UnresolvedReason};

use support::*;

// ---------------------------------------------------------------------------
// Fixtures — synthetic ABL only
// ---------------------------------------------------------------------------

/// The base class every scenario inherits from: one public property, one public
/// method with a declared return type, one protected method, one private method.
const CALC_BASE: &str = r#"CLASS orders.calc-base:
    DEFINE PUBLIC PROPERTY base-label AS CHARACTER NO-UNDO GET. SET.
    METHOD PUBLIC INTEGER calc-total():
        RETURN 0.
    END METHOD.
    METHOD PROTECTED DECIMAL calc-rate():
        RETURN 0.0.
    END METHOD.
    METHOD PRIVATE INTEGER calc-secret():
        RETURN 0.
    END METHOD.
END CLASS."#;

/// Path the batch index searches for [`CALC_BASE`] under, given `/src` on the
/// paths: a qualified name maps to a relative path by replacing dots with
/// separators.
const CALC_BASE_PATH: &str = "/src/orders/calc-base.cls";

/// A subclass assigning an inherited `INTEGER` method's result into a `LOGICAL`
/// variable — a **deliberate type mismatch**.
///
/// Every other fixture in this file type-matches its assignment
/// (`v-total AS INTEGER = calc-total()`), which is exactly why they could not
/// catch an inherited member's declared type reaching the type lattice: a
/// correctly typed assignment produces no LINT0004 finding whether the type is
/// known or `Unknown`. This one produces one the moment it is known, and none
/// today, so the firewall sweep below has real work to do.
const MISMATCHED_CHILD: &str = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.
        v-flag = calc-total().
    END METHOD.
END CLASS."#;

// ---------------------------------------------------------------------------
// Accessible members resolve, with their declared types
// ---------------------------------------------------------------------------

#[test]
fn a_public_parent_method_resolves_for_the_child_with_its_return_type() {
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];

    let (_stmts, sem) = with_index(child, &workspace);
    let sym = sole_resolved(&sem, "calc-total");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Function);
    // The declared return type is on `data_type`, the same field a local
    // declaration populates and the one both type-lattice readers consult
    // (`check.rs::type_from_reference`'s fallback arm and
    // `type_mismatch_assignment::target_type`). It used to be parked in a side
    // map so it could not reach them; judging the cross-file population is what
    // reaching them is for.
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer)),
        "the resolved symbol carries the parent's declared return type"
    );
    assert_synthesized(&sem, sym, NamespaceId::Functions);

    // The firewall: without an index the same call is unresolved, exactly as it
    // is for every client today.
    let (_stmts, plain) = without_index(child);
    assert!(resolved_to(&plain, "calc-total").is_empty());
    assert_eq!(
        unresolved_reasons(&plain, "calc-total"),
        vec![UnresolvedReason::NotInScope],
        "today's answer for a name only the parent file declares"
    );
    assert!(
        symbols_named(&plain, "calc-total").is_empty(),
        "nothing is synthesized without an index"
    );
}

#[test]
fn a_public_parent_property_resolves_for_the_child_with_its_type() {
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.
        v-label = base-label.
    END METHOD.
END CLASS."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];

    let (_stmts, sem) = with_index(child, &workspace);
    let sym = sole_resolved(&sem, "base-label");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Property);
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Character))
    );
    assert_synthesized(&sem, sym, NamespaceId::Values);

    let (_stmts, plain) = without_index(child);
    assert!(resolved_to(&plain, "base-label").is_empty());
    assert!(symbols_named(&plain, "base-label").is_empty());
}

#[test]
fn a_protected_parent_method_resolves_for_the_child() {
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-rate AS DECIMAL NO-UNDO.
        v-rate = calc-rate().
    END METHOD.
END CLASS."#;
    let (_stmts, sem) = with_index(child, &[(CALC_BASE_PATH, CALC_BASE)]);
    let sym = sole_resolved(&sem, "calc-rate");
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Decimal))
    );
}

#[test]
fn a_private_parent_method_does_not_resolve_for_the_child() {
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-secret().
    END METHOD.
END CLASS."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];

    let (_stmts, sem) = with_index(child, &workspace);
    assert!(
        resolved_to(&sem, "calc-secret").is_empty(),
        "a private member is not inherited, so nothing may resolve to it"
    );
    assert!(
        symbols_named(&sem, "calc-secret").is_empty(),
        "and no symbol is synthesized for it either — a later access-check rule \
         must see a violation, not a resolved reference"
    );
    // `NotFoundInWorkspace`, not `NotInScope`: the chain *does* declare the name,
    // it just does not pass it on, and claiming there is no such symbol would be
    // false. Every rule already skips the cross-file reasons, so the reference is
    // silent rather than reported as undefined.
    assert_eq!(
        unresolved_reasons(&sem, "calc-secret"),
        vec![UnresolvedReason::NotFoundInWorkspace]
    );
    assert!(
        lint0001_with_index(child, &workspace).is_empty(),
        "an inaccessible member is not an undefined symbol"
    );

    let (_stmts, plain) = without_index(child);
    assert_eq!(
        unresolved_reasons(&plain, "calc-secret"),
        vec![UnresolvedReason::NotInScope],
        "with no index nothing was looked at, so today's answer stands"
    );
}

#[test]
fn an_inaccessible_ancestor_member_only_softens_its_own_namespace() {
    // `calc-secret` is a private ancestor *method*, so it lives in the functions
    // namespace. A bare `calc-secret` — no parentheses — is a **value**-namespace
    // reference, naming no method at all, and nothing in the chain declares a
    // value by that name. It is a genuine undefined symbol and must still be
    // reported.
    //
    // The inaccessible-member set is what would silence it: softening a reference
    // to "a member an ancestor declares privately" suppresses the finding, and a
    // key without the namespace in it suppresses across namespaces. That is a lost
    // true positive, so the key carries the namespace.
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-n AS INTEGER NO-UNDO.
        v-n = calc-secret.
    END METHOD.
END CLASS."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];

    let (_stmts, sem) = with_index(child, &workspace);
    assert_eq!(
        unresolved_reasons(&sem, "calc-secret"),
        vec![UnresolvedReason::NotInScope],
        "a value-namespace reference is not answered by a function-namespace member"
    );
    assert_eq!(
        lint0001_with_index(child, &workspace).len(),
        1,
        "and the finding survives — LINT0001 renders `NotInScope` only"
    );
    // Exactly today's answer, which is the other half of the property: the fix
    // restores a finding rather than adding one.
    assert_eq!(
        lint0001_without_index(child).len(),
        1,
        "the same finding the single-file run produces"
    );

    // The sibling shape that proves the softening still works where it should: the
    // *same* private name **called**, which is a functions-namespace reference, is
    // an inaccessible member and stays silent.
    let called = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-n AS INTEGER NO-UNDO.
        v-n = calc-secret().
    END METHOD.
END CLASS."#;
    let (_stmts, sem) = with_index(called, &workspace);
    assert_eq!(
        unresolved_reasons(&sem, "calc-secret"),
        vec![UnresolvedReason::NotFoundInWorkspace]
    );
    assert!(lint0001_with_index(called, &workspace).is_empty());
}

#[test]
fn a_name_no_ancestor_declares_stays_undefined_under_an_index() {
    // The other half of the reason split, and the one that matters most: a
    // misspelling inside a subclass must keep firing LINT0001 when an index is
    // attached. Softening every miss inside a class body would have silenced it.
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-totl().
    END METHOD.
END CLASS."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];

    let (_stmts, sem) = with_index(child, &workspace);
    assert_eq!(
        unresolved_reasons(&sem, "calc-totl"),
        vec![UnresolvedReason::NotInScope]
    );
    assert_eq!(
        lint0001_with_index(child, &workspace).len(),
        1,
        "a typo is still an undefined symbol with an index attached"
    );
}

// ---------------------------------------------------------------------------
// Chain shape
// ---------------------------------------------------------------------------

#[test]
fn a_three_level_chain_resolves_a_member_declared_on_the_grandparent() {
    let child = r#"CLASS orders.child INHERITS orders.middle:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS."#;
    let workspace = [
        (CALC_BASE_PATH, CALC_BASE),
        (
            "/src/orders/middle.cls",
            "CLASS orders.middle INHERITS orders.calc-base: END CLASS.",
        ),
    ];

    let (_stmts, sem) = with_index(child, &workspace);
    let sym = sole_resolved(&sem, "calc-total");
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
    );
}

#[test]
fn a_class_inheriting_itself_terminates_with_not_found() {
    // A real shape in broken code. The visited set is seeded with the class's own
    // name, so this neither recurses nor folds the class's own members in as
    // inherited ones.
    let child = r#"CLASS orders.child INHERITS orders.child:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS."#;
    let workspace = [("/src/orders/child.cls", child)];

    let (_stmts, sem) = with_index(child, &workspace);
    assert!(resolved_to(&sem, "calc-total").is_empty());
    assert_eq!(
        unresolved_reasons(&sem, "calc-total"),
        vec![UnresolvedReason::NotInScope]
    );
}

#[test]
fn a_two_class_cycle_terminates() {
    // `orders.a` inherits `orders.b`, which inherits `orders.a` back. The walk
    // must finish, contribute what `orders.b` really declares, and answer nothing
    // for a name neither declares.
    let a = r#"CLASS orders.a INHERITS orders.b:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = from-b().
        v-total = ghost-member().
    END METHOD.
END CLASS."#;
    let b = r#"CLASS orders.b INHERITS orders.a:
    METHOD PUBLIC INTEGER from-b():
        RETURN 0.
    END METHOD.
END CLASS."#;
    let workspace = [("/src/orders/a.cls", a), ("/src/orders/b.cls", b)];

    let (_stmts, sem) = with_index(a, &workspace);
    let sym = sole_resolved(&sem, "from-b");
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
    );
    assert_eq!(
        unresolved_reasons(&sem, "ghost-member"),
        vec![UnresolvedReason::NotInScope]
    );
}

#[test]
fn an_interface_method_contributes_to_an_implementing_class() {
    let child = r#"CLASS orders.child IMPLEMENTS orders.i-calc:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = required-total().
    END METHOD.
END CLASS."#;
    let workspace = [(
        "/src/orders/i-calc.cls",
        r#"INTERFACE orders.i-calc:
    METHOD PUBLIC INTEGER required-total().
END INTERFACE."#,
    )];

    let (_stmts, sem) = with_index(child, &workspace);
    let sym = sole_resolved(&sem, "required-total");
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
    );
    assert_synthesized(&sem, sym, NamespaceId::Functions);
}

// ---------------------------------------------------------------------------
// Shadowing, memoization, and scope-tree isolation
// ---------------------------------------------------------------------------

#[test]
fn a_member_declared_on_both_child_and_parent_resolves_to_the_childs() {
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC CHARACTER calc-total():
        RETURN "mine".
    END METHOD.
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.
        v-label = calc-total().
    END METHOD.
END CLASS."#;

    let (_stmts, sem) = with_index(child, &[(CALC_BASE_PATH, CALC_BASE)]);
    let sym = sole_resolved(&sem, "calc-total");
    let symbol = sem.symbols.get(sym);
    assert_ne!(
        symbol.declaration,
        NodeId::DUMMY,
        "the local scope tree wins over the index, always"
    );
    assert_eq!(
        symbol.data_type,
        Some(ResolvedType::Primitive(PrimitiveTy::Character)),
        "the child's own return type, not the parent's INTEGER"
    );
    assert_eq!(
        symbols_named(&sem, "calc-total").len(),
        1,
        "the shadowed parent member is never synthesized — nothing can reach it"
    );
}

#[test]
fn twenty_references_to_one_inherited_member_synthesize_it_once() {
    let mut body = String::from(
        "CLASS orders.child INHERITS orders.calc-base:\n    \
         METHOD PUBLIC VOID run-it():\n        \
         DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n",
    );
    for _ in 0..20 {
        body.push_str("        v-total = calc-total().\n");
    }
    body.push_str("    END METHOD.\nEND CLASS.");

    let (_stmts, sem) = with_index(&body, &[(CALC_BASE_PATH, CALC_BASE)]);
    let sym = sole_resolved(&sem, "calc-total");
    assert_eq!(
        symbols_named(&sem, "calc-total"),
        vec![sym],
        "one symbol for twenty references"
    );
    assert_eq!(
        sem.symbols.get(sym).read_count,
        20,
        "every reference is still counted against the one symbol"
    );

    // And the members nobody named cost nothing at all: the parent declares a
    // property and two more methods, none of which appear.
    assert!(symbols_named(&sem, "base-label").is_empty());
    assert!(symbols_named(&sem, "calc-rate").is_empty());
}

#[test]
fn a_synthesized_inherited_member_is_not_reachable_from_an_unrelated_scope() {
    // The KTD6 property, asserted behaviorally: the same name mentioned outside
    // the subclass must not find the symbol the subclass's reference synthesized.
    let source = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS.

PROCEDURE elsewhere:
    DEFINE VARIABLE v-other AS INTEGER NO-UNDO.
    v-other = calc-total().
END PROCEDURE."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];

    let (_stmts, sem) = with_index(source, &workspace);
    // One reference resolved (inside the class), one did not (outside it).
    assert_eq!(resolved_to(&sem, "calc-total").len(), 1);
    assert_eq!(
        unresolved_reasons(&sem, "calc-total"),
        vec![UnresolvedReason::NotInScope],
        "the procedure's call is undefined — the synthesized symbol is not in the \
         scope tree, so nothing outside the subclass can reach it"
    );
    assert_eq!(
        lint0001_with_index(source, &workspace).len(),
        1,
        "and the outside reference is still reported"
    );
}

// ---------------------------------------------------------------------------
// The parent link on the class symbol
// ---------------------------------------------------------------------------

#[test]
fn an_unresolvable_parent_is_recorded_on_the_class_symbol_with_its_span() {
    let child = "CLASS orders.child INHERITS orders.no-such-base: END CLASS.";
    let (_stmts, sem) = with_index(child, &[]);

    let class = sem
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Class)
        .map(|(id, _)| id)
        .expect("the class is declared");
    let supers = sem
        .symbols
        .supertypes(class)
        .expect("a class naming a parent records it");
    let parent = supers
        .inherits
        .as_ref()
        .expect("the INHERITS name is recorded even though nothing declares it");
    assert_eq!(parent.name.as_str(), "orders.no-such-base");
    assert_eq!(parent.name.as_written(), "orders.no-such-base");
    // The span covers the name in the header, which is what a diagnostic about an
    // unresolvable parent would point at.
    assert_eq!(
        &child[parent.name_span.start as usize..parent.name_span.end as usize],
        "orders.no-such-base"
    );
    assert!(supers.implements.is_empty());
}

#[test]
fn a_class_with_no_parent_is_distinguishable_from_one_whose_parent_is_missing() {
    let (_stmts, sem) = without_index("CLASS orders.child: END CLASS.");
    let class = sem
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Class)
        .map(|(id, _)| id)
        .expect("the class is declared");
    assert!(
        sem.symbols.supertypes(class).is_none(),
        "a header naming no supertype records none — not an empty one"
    );
}

#[test]
fn implemented_interfaces_are_recorded_in_declaration_order() {
    let source = "CLASS orders.child INHERITS orders.calc-base IMPLEMENTS orders.i-calc, orders.i-audit: \
         END CLASS.";
    let (_stmts, sem) = without_index(source);
    let class = sem
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Class)
        .map(|(id, _)| id)
        .expect("the class is declared");
    let supers = sem.symbols.supertypes(class).unwrap();
    assert_eq!(
        supers
            .inherits
            .as_ref()
            .map(|s| s.name.as_str().to_string()),
        Some("orders.calc-base".to_string())
    );
    let implements: Vec<&str> = supers.implements.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(implements, vec!["orders.i-calc", "orders.i-audit"]);
}

// ---------------------------------------------------------------------------
// What an index adds, enumerated
// ---------------------------------------------------------------------------

#[test]
fn attaching_an_index_adds_exactly_the_enumerated_diagnostics() {
    // This swept the phase's old central property — that attaching an index added
    // no finding to any fixture in this file — and it is now the enumeration of
    // what an index *does* add, one expected code list per scenario. Inverted
    // rather than deleted: the sweep's value was never the zero, it was covering
    // every fixture at once across all six rules plus the semantic pass, and a
    // reader comparing a scenario against its list can see which shapes are judged
    // and which are still silent.
    //
    // Spans are deliberately out of the expectation. A fixture reflowed by one
    // character would shift every span in it, which would make the table expensive
    // to maintain and would say nothing about behavior; the byte-span comparison
    // still happens inside the helper, which is what decides whether a finding is
    // "the same" one.
    //
    // One combined workspace rather than each scenario's own: the property is
    // unconditional, so a broader workspace can only make the sweep stricter.
    let workspace = [
        (CALC_BASE_PATH, CALC_BASE),
        (
            "/src/orders/middle.cls",
            "CLASS orders.middle INHERITS orders.calc-base: END CLASS.",
        ),
        (
            "/src/orders/i-calc.cls",
            "INTERFACE orders.i-calc:\n    METHOD PUBLIC INTEGER required-total().\nEND INTERFACE.",
        ),
        (
            "/src/orders/child.cls",
            "CLASS orders.child INHERITS orders.child: END CLASS.",
        ),
        (
            "/src/orders/a.cls",
            "CLASS orders.a INHERITS orders.b: END CLASS.",
        ),
        (
            "/src/orders/b.cls",
            "CLASS orders.b INHERITS orders.a:\n    METHOD PUBLIC INTEGER from-b():\n        RETURN 0.\n    END METHOD.\nEND CLASS.",
        ),
    ];

    let mut many_references = String::from(
        "CLASS orders.child INHERITS orders.calc-base:\n    \
         METHOD PUBLIC VOID run-it():\n        \
         DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n",
    );
    for _ in 0..20 {
        many_references.push_str("        v-total = calc-total().\n");
    }
    many_references.push_str("    END METHOD.\nEND CLASS.");

    // The five deliberate mismatches: an inherited member's declared type now
    // reaches the lattice, so each of these gains exactly one LINT0004 — the
    // finding the valve was holding back. Five spellings rather than one, because
    // a type can enter the rule through a superclass method, an interface method,
    // a property in the *target* position, `ASSIGN`, or `INITIAL`.
    let judged = [
        // The mismatch: an inherited INTEGER method into a LOGICAL variable.
        MISMATCHED_CHILD.to_string(),
        // The same shape through an *interface* rather than a superclass, and the
        // same shape with the mismatch in the target direction — both routes an
        // inherited type could take into the lattice.
        r#"CLASS orders.child IMPLEMENTS orders.i-calc:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.
        v-flag = required-total().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        base-label = 5.
    END METHOD.
END CLASS."#
            .to_string(),
        // `ASSIGN` and `INITIAL`, the two other forms the same mismatch reaches
        // the rule through.
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.
        ASSIGN v-flag = calc-total().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO INITIAL ?.
        v-flag = calc-total().
    END METHOD.
END CLASS."#
            .to_string(),
    ];
    // Everything else: type-matched assignments, a private ancestor member, a
    // typo, the two inheritance cycles, an unresolvable parent, and the
    // twenty-reference fixture. Each stays exactly as silent as it was — resolving
    // a name across files is not, on its own, a reason to say anything.
    let silent = [
        // Every type-matched fixture the tests above use, so a future change that
        // shifts one of them shows up here too.
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.
        v-label = base-label.
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-rate AS DECIMAL NO-UNDO.
        v-rate = calc-rate().
    END METHOD.
END CLASS."#
            .to_string(),
        // A private ancestor member, and a typo — the two shapes that must stay
        // exactly as undefined (or as silent) as they are today.
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-secret().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-totl().
    END METHOD.
END CLASS."#
            .to_string(),
        // A three-level chain, a self-inheriting class, and a two-class cycle.
        r#"CLASS orders.child INHERITS orders.middle:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.child:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.a INHERITS orders.b:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = from-b().
        v-total = ghost-member().
    END METHOD.
END CLASS."#
            .to_string(),
        // An interface member, a child member shadowing the parent's, and a
        // reference from a scope the synthesized symbol must not reach.
        r#"CLASS orders.child IMPLEMENTS orders.i-calc:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = required-total().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC CHARACTER calc-total():
        RETURN "mine".
    END METHOD.
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.
        v-label = calc-total().
    END METHOD.
END CLASS."#
            .to_string(),
        r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
    END METHOD.
END CLASS.

PROCEDURE elsewhere:
    DEFINE VARIABLE v-other AS INTEGER NO-UNDO.
    v-other = calc-total().
END PROCEDURE."#
            .to_string(),
        // An unresolvable parent.
        "CLASS orders.child INHERITS orders.no-such-base: END CLASS.".to_string(),
        many_references,
    ];

    for (i, source) in judged.iter().enumerate() {
        assert_eq!(
            codes_added_by_index(source, &workspace),
            vec![LINT0004],
            "judged scenario {i} must gain exactly one type-mismatch finding:\n{source}"
        );
    }
    for (i, source) in silent.iter().enumerate() {
        assert_eq!(
            codes_added_by_index(source, &workspace),
            Vec::<&str>::new(),
            "silent scenario {i} must gain nothing:\n{source}"
        );
    }
}

#[test]
fn a_mismatched_assignment_from_an_inherited_method_is_reported() {
    // The enumeration above says this scenario gains a LINT0004; this says what
    // that finding is made of. Every ingredient the rule needs is present and
    // cross-file: the member resolves through the index, its declared `INTEGER`
    // is on the symbol, and the target is a genuinely incompatible `LOGICAL`.
    //
    // The test it replaces asserted the opposite — that the ingredients were all
    // present and the rule still said nothing, because the type was parked in a
    // side map where neither `check.rs` nor the rule could read it. Both halves
    // were true then. Promoting the type onto `Symbol::data_type` is what turns
    // the silence into the finding, and this is where that shows.
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];
    let (_stmts, sem) = with_index(MISMATCHED_CHILD, &workspace);

    let member = sole_resolved(&sem, "calc-total");
    assert_eq!(
        sem.symbols.get(member).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer)),
        "the inherited member resolved, carrying the parent's declared return type"
    );
    let target = sole_symbol(&sem, "v-flag");
    assert_eq!(
        sem.symbols.get(target).data_type,
        Some(ResolvedType::Primitive(PrimitiveTy::Logical)),
        "and the assignment target is a genuinely incompatible LOGICAL"
    );

    let found = lint0004_with_index(MISMATCHED_CHILD, &workspace);
    assert_eq!(found.len(), 1, "one type mismatch: {found:?}");
    assert!(
        found[0].message.contains("INTEGER") && found[0].message.contains("LOGICAL"),
        "and it names both ABL types: {}",
        found[0].message
    );
    // With no index the same call resolves to nothing, so there is no type to
    // judge and no finding — the direction of the effect, which is the property
    // the whole cross-file suite is built to state.
    assert!(lint0004_without_index(MISMATCHED_CHILD).is_empty());

    // The in-file baseline: the identical assignment from a method declared right
    // here has always fired, which is what makes the cross-file finding the same
    // verdict rather than a new kind of one.
    let local = r#"CLASS orders.child:
    METHOD PUBLIC INTEGER calc-total():
        RETURN 0.
    END METHOD.
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.
        v-flag = calc-total().
    END METHOD.
END CLASS."#;
    assert_eq!(
        lint0004_without_index(local).len(),
        1,
        "an in-file INTEGER-into-LOGICAL assignment is reported"
    );
    assert_eq!(lint0004_with_index(local, &workspace).len(), 1);
}

#[test]
fn an_interface_records_its_supertypes_as_implements() {
    // An interface may extend several interfaces, so its list cannot fit
    // `inherits: Option<_>` — it is recorded the way `oxabl_index` records it.
    let (_stmts, sem) =
        without_index("INTERFACE orders.i-calc INHERITS orders.i-audit: END INTERFACE.");
    let iface = sem
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Interface)
        .map(|(id, _)| id)
        .expect("the interface is declared");
    let supers = sem.symbols.supertypes(iface).unwrap();
    assert!(supers.inherits.is_none());
    let implements: Vec<&str> = supers.implements.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(implements, vec!["orders.i-audit"]);
}

// ---------------------------------------------------------------------------
// The promotion's edges
// ---------------------------------------------------------------------------

#[test]
fn a_void_parent_method_leaves_the_member_untyped_and_fires_nothing() {
    // `VOID` names no type, so the index contributes none and the symbol stays
    // `data_type: None`. That matters to the rule: `None` is what
    // `type_mismatch_assignment::target_type` reads as "no verdict possible",
    // so assigning a void call's result is silent rather than wrong-in-a-new-way.
    let base = r#"CLASS orders.void-base:
    METHOD PUBLIC VOID do-it():
    END METHOD.
END CLASS."#;
    let child = r#"CLASS orders.child INHERITS orders.void-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.
        v-flag = do-it().
    END METHOD.
END CLASS."#;
    let workspace = [("/src/orders/void-base.cls", base)];

    let (_stmts, sem) = with_index(child, &workspace);
    let member = sole_resolved(&sem, "do-it");
    assert_eq!(
        sem.symbols.get(member).data_type,
        None,
        "a VOID method contributes no type to promote"
    );
    assert!(lint0004_with_index(child, &workspace).is_empty());
}

#[test]
fn a_parent_property_type_reaches_the_lattice_like_a_return_type_does() {
    // A property and a method return type travel different paths through the
    // index — different namespaces, different declaration forms — and both must
    // land on `data_type`. The mismatch is in the *target* position here, which is
    // the read `type_mismatch_assignment::target_type` performs directly.
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        base-label = 5.
    END METHOD.
END CLASS."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];

    let (_stmts, sem) = with_index(child, &workspace);
    let member = sole_resolved(&sem, "base-label");
    assert_eq!(
        sem.symbols.get(member).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Character))
    );
    let found = lint0004_with_index(child, &workspace);
    assert_eq!(
        found.len(),
        1,
        "INTEGER into a CHARACTER property: {found:?}"
    );
    assert!(found[0].message.contains("CHARACTER"));
    assert!(lint0004_without_index(child).is_empty());
}

#[test]
fn a_member_reached_through_a_three_level_chain_carries_the_grandparents_type() {
    let child = r#"CLASS orders.child INHERITS orders.middle:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.
        v-flag = calc-total().
    END METHOD.
END CLASS."#;
    let workspace = [
        (CALC_BASE_PATH, CALC_BASE),
        (
            "/src/orders/middle.cls",
            "CLASS orders.middle INHERITS orders.calc-base: END CLASS.",
        ),
    ];

    let (_stmts, sem) = with_index(child, &workspace);
    let member = sole_resolved(&sem, "calc-total");
    assert_eq!(
        sem.symbols.get(member).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer)),
        "the type declared two levels up"
    );
    assert_eq!(
        lint0004_with_index(child, &workspace).len(),
        1,
        "and it is judged from there"
    );
}

#[test]
fn nothing_is_synthesized_and_nothing_is_typed_without_an_index() {
    // The promotion cannot fire where nothing was looked up. Stated on the
    // mismatch fixture, so a regression shows as a *new* finding rather than as a
    // missing symbol.
    let (_stmts, sem) = without_index(MISMATCHED_CHILD);
    assert!(symbols_named(&sem, "calc-total").is_empty());
    assert!(lint0004_without_index(MISMATCHED_CHILD).is_empty());
}

#[test]
fn a_colon_qualified_call_on_this_object_stays_unjudged() {
    // The deferred boundary, pinned rather than assumed. `check.rs` types every
    // `MethodCall` and `MemberAccess` as `Unknown` without consulting the symbol
    // the receiver resolved to, so an inherited type reaches the lattice only
    // through an *unqualified* reference. `THIS-OBJECT:calc-total()` is the
    // ordinary OO-ABL spelling of the same call and is silent — the larger half of
    // the population, and its own unit of work.
    let child = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.
        v-flag = THIS-OBJECT:calc-total().
    END METHOD.
END CLASS."#;
    let workspace = [(CALC_BASE_PATH, CALC_BASE)];
    assert!(
        lint0004_with_index(child, &workspace).is_empty(),
        "a `:`-qualified call is not typed yet, deliberately"
    );
    assert_eq!(codes_added_by_index(child, &workspace), Vec::<&str>::new());
}
