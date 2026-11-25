//! Which class a name binds when a workspace file and the built-in catalog
//! both have it. The order is the compiler's (OpenEdge 12.8, `COMPILE`):
//! a `Progress.*` type is always the built-in one; an `OpenEdge.*` copy in the
//! workspace wins; the unnamed package comes before the implicit
//! `USING Progress.Lang.*`, and an explicit wildcard `USING` before both.

mod support;

use oxabl_semantic::{ResolvedType, Semantic, SymbolKind};

use support::*;

/// What the declared type of the variable `name` is: a built-in type (its
/// documented name), a workspace class (its folded name), or neither.
fn bound(sem: &Semantic, name: &str) -> String {
    let ty = sem
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Variable && s.name.as_ref() == name)
        .and_then(|(_, s)| s.data_type.clone())
        .expect("the variable is declared");
    match ty {
        ResolvedType::Class(sym) => match sem.symbols.builtin_class(sym) {
            Some(class) => format!("built-in {}", class.name),
            None => format!("workspace {}", sem.symbols.get(sym).name),
        },
        other => format!("{other:?}"),
    }
}

#[test]
fn a_progress_type_is_the_built_in_one_whatever_the_workspace_holds() {
    let workspace = [(
        "/src/Progress/Lang/AppError.cls",
        "CLASS Progress.Lang.AppError:\n    METHOD PUBLIC VOID Foo():\n    END METHOD.\nEND CLASS.\n",
    )];
    let (_s, sem) = with_index(
        "DEFINE VARIABLE e AS Progress.Lang.AppError NO-UNDO.\n",
        &workspace,
    );
    assert_eq!(bound(&sem, "e"), "built-in Progress.Lang.AppError");
}

#[test]
fn a_workspace_copy_of_an_openedge_type_wins() {
    let workspace = [(
        "/src/OpenEdge/Core/String.cls",
        "CLASS OpenEdge.Core.String:\nEND CLASS.\n",
    )];
    let (_s, sem) = with_index(
        "DEFINE VARIABLE s AS OpenEdge.Core.String NO-UNDO.\n",
        &workspace,
    );
    assert_eq!(bound(&sem, "s"), "workspace openedge.core.string");
}

#[test]
fn the_unnamed_package_comes_before_the_implicit_import_and_after_a_wildcard() {
    let workspace = [(
        "/src/AppError.cls",
        "CLASS AppError:\n    METHOD PUBLIC VOID Bar():\n    END METHOD.\nEND CLASS.\n",
    )];
    let (_s, bare) = with_index("DEFINE VARIABLE e AS AppError NO-UNDO.\n", &workspace);
    assert_eq!(bound(&bare, "e"), "workspace apperror");
    let (_s, wildcard) = with_index(
        "USING Progress.Lang.*.\nDEFINE VARIABLE e AS AppError NO-UNDO.\n",
        &workspace,
    );
    assert_eq!(bound(&wildcard, "e"), "built-in Progress.Lang.AppError");
    // With no unnamed-package class, the implicit import gives the built-in type.
    let (_s, implicit) = with_index("DEFINE VARIABLE e AS AppError NO-UNDO.\n", &[]);
    assert_eq!(bound(&implicit, "e"), "built-in Progress.Lang.AppError");
}
