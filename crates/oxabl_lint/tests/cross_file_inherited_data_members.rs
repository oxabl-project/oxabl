//! Data members a class inherits from a workspace superclass (variables,
//! buffers, temp-tables, datasets) are in scope in the subclass, by the same
//! rules as inherited methods: `PROTECTED` and `PUBLIC` reach it, `PRIVATE`
//! (the default for a data member) does not. These run the real `BatchIndex`.

mod support;

use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_semantic::{
    ClassLookup, NamespaceId, PrimitiveTy, ResolvedType, SymbolKind, UnresolvedReason,
};

use support::*;

const BASE_PATH: &str = "/src/pkg/base.cls";
const BASE: &str = r#"CLASS pkg.base:
    DEFINE PROTECTED VARIABLE req_ AS Progress.Lang.Object NO-UNDO.
    DEFINE PROTECTED VARIABLE count_ AS INTEGER NO-UNDO.
    DEFINE PUBLIC VARIABLE shown_ AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE secret_ AS Progress.Lang.Object NO-UNDO.
    DEFINE VARIABLE unmarked_ AS Progress.Lang.Object NO-UNDO.
    DEFINE PROTECTED TEMP-TABLE tt_ NO-UNDO FIELD f AS INTEGER.
    DEFINE PROTECTED BUFFER buf_ FOR TEMP-TABLE tt_.
    DEFINE PROTECTED DATASET ds_ FOR tt_.
END CLASS."#;

fn child(body: &str) -> String {
    format!(
        "USING pkg.*.\nCLASS app.child INHERITS pkg.base:\n    METHOD PUBLIC VOID run-it():\n        {body}\n    END METHOD.\nEND CLASS."
    )
}

#[test]
fn a_protected_variable_resolves_for_the_child_with_its_type() {
    let source = child("DEFINE VARIABLE v AS INTEGER NO-UNDO. v = count_.");
    let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

    let sym = sole_resolved(&sem, "count_");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Variable);
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
    );
    assert_synthesized(&sem, sym, NamespaceId::Values);
}

#[test]
fn a_public_variable_resolves_for_the_child() {
    let source = child("MESSAGE shown_.");
    let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

    sole_resolved(&sem, "shown_");
}

#[test]
fn a_protected_object_variable_is_no_class_reference() {
    let source = child("IF req_:ToString() EQ \"x\" THEN RETURN.");
    let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

    assert_eq!(
        sem.symbols.get(sole_resolved(&sem, "req_")).kind,
        SymbolKind::Variable
    );
    assert_eq!(
        sem.symbols.class_lookup(&OxablAtom::from("req_")),
        None,
        "no lookup for the variable's name as a class"
    );
}

#[test]
fn a_private_variable_does_not_resolve_for_the_child() {
    for name in ["secret_", "unmarked_"] {
        let source = child(&format!("IF {name}:ToString() EQ \"x\" THEN RETURN."));
        let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

        assert!(
            resolved_to(&sem, name).is_empty(),
            "{name}: a private member is not inherited"
        );
        assert!(
            symbols_named(&sem, name).is_empty(),
            "{name}: no symbol is synthesized for it"
        );
        assert_eq!(
            unresolved_reasons(&sem, name),
            vec![UnresolvedReason::PresentButUnusable],
            "{name}"
        );
        assert_eq!(
            sem.symbols.class_lookup(&OxablAtom::from(name)),
            None,
            "{name}"
        );
    }
}

#[test]
fn a_protected_temp_table_and_buffer_resolve_in_the_buffers_namespace() {
    let source = child("DEFINE VARIABLE h AS HANDLE NO-UNDO. h = buf_:HANDLE. h = tt_:HANDLE.");
    let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

    let buf = sole_resolved(&sem, "buf_");
    assert_eq!(sem.symbols.get(buf).kind, SymbolKind::Buffer);
    assert_synthesized(&sem, buf, NamespaceId::Buffers);
    let tt = sole_resolved(&sem, "tt_");
    assert_eq!(sem.symbols.get(tt).kind, SymbolKind::TempTable);
    assert_synthesized(&sem, tt, NamespaceId::Buffers);
}

#[test]
fn a_protected_dataset_resolves_for_the_child() {
    let source = child("ds_:EMPTY-DATASET().");
    let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

    assert_eq!(
        sem.symbols.get(sole_resolved(&sem, "ds_")).kind,
        SymbolKind::Dataset
    );
}

#[test]
fn a_variable_resolves_through_a_three_level_chain() {
    let workspace = [
        (BASE_PATH, BASE),
        (
            "/src/pkg/mid.cls",
            "CLASS pkg.mid INHERITS pkg.base: END CLASS.",
        ),
    ];
    let source = "USING pkg.*.\nCLASS app.child INHERITS pkg.mid:\n    METHOD PUBLIC VOID run-it():\n        req_:ToString().\n    END METHOD.\nEND CLASS.";
    let (_stmts, sem) = with_index(source, &workspace);

    sole_resolved(&sem, "req_");
}

#[test]
fn a_name_that_is_a_class_still_resolves_as_one() {
    let workspace = [
        (BASE_PATH, BASE),
        ("/src/pkg/util.cls", "CLASS pkg.util: END CLASS."),
    ];
    let source = child("pkg.util:ToString().");
    let (_stmts, sem) = with_index(&source, &workspace);

    assert!(matches!(
        sem.symbols.class_lookup(&OxablAtom::from("pkg.util")),
        Some(ClassLookup::Linked(_))
    ));
}

#[test]
fn a_name_no_ancestor_declares_is_still_a_class_miss() {
    let source = child("nothing_:ToString().");
    let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

    assert!(resolved_to(&sem, "nothing_").is_empty());
    assert_eq!(
        sem.symbols.class_lookup(&OxablAtom::from("nothing_")),
        Some(ClassLookup::Absent)
    );
}

#[test]
fn the_child_own_variable_shadows_the_inherited_one() {
    let source = child("DEFINE VARIABLE count_ AS CHARACTER NO-UNDO. count_ = \"x\".");
    let (_stmts, sem) = with_index(&source, &[(BASE_PATH, BASE)]);

    let sym = sole_resolved(&sem, "count_");
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Character))
    );
}
