//! Built-in types in the resolve pass: which answers are sure, the symbols of
//! built-in types and their supertypes, and the class lattice over them.

use std::sync::Arc;

use oxabl_common::FileId;
use oxabl_schema::Schema;

use crate::coercion::{ClassLattice, Reach};
use crate::{
    AnalysisContext, ClassDescriptor, IndexAnswer, IndexName, IndexRevision, IndexedFileId,
    MemberDescriptor, ResolvedType, Semantic, SymbolId, SymbolKind, WorkspaceIndex, analyze_file,
};

/// An index that knows no class and searches the paths it was given:
/// every candidate spelling was looked up and missed.
struct SearchedEmpty;

impl WorkspaceIndex for SearchedEmpty {
    fn class(&self, _name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        IndexAnswer::NotFound
    }
    fn class_members(&self, _class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        IndexAnswer::NotFound
    }
    fn program(&self, _target: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }
    fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }
    fn revision(&self) -> IndexRevision {
        IndexRevision::new(1)
    }
}

/// The same index with no search path: nothing was looked up.
struct PathlessEmpty;

impl WorkspaceIndex for PathlessEmpty {
    fn class(&self, _name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        IndexAnswer::NotFound
    }
    fn class_members(&self, _class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        IndexAnswer::NotFound
    }
    fn program(&self, _target: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }
    fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }
    fn revision(&self) -> IndexRevision {
        IndexRevision::new(1)
    }
    fn searches_any_path(&self) -> bool {
        false
    }
}

fn analyze_with(source: &str, index: Option<&dyn WorkspaceIndex>) -> Semantic {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let ctx = match index {
        Some(index) => ctx.with_index(index),
        None => ctx,
    };
    analyze_file(&program.statements, &ctx)
}

fn analyze(source: &str) -> Semantic {
    analyze_with(source, None)
}

/// The declared type of the variable `name`.
fn var_type(sem: &Semantic, name: &str) -> ResolvedType {
    sem.symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Variable && s.name.as_ref() == name)
        .and_then(|(_, s)| s.data_type.clone())
        .expect("the variable is declared")
}

/// The documented name of the built-in type of the variable `name`, if its
/// type is one.
fn builtin_of(sem: &Semantic, name: &str) -> Option<&'static str> {
    match var_type(sem, name) {
        ResolvedType::Class(sym) => sem.symbols.builtin_class(sym).map(|c| c.name),
        _ => None,
    }
}

fn class_of(sem: &Semantic, name: &str) -> SymbolId {
    match var_type(sem, name) {
        ResolvedType::Class(sym) => sym,
        other => panic!("`{name}` has the type {other:?}"),
    }
}

#[test]
fn a_qualified_name_is_sure_with_no_index() {
    let sem = analyze(
        "DEFINE VARIABLE e AS Progress.Lang.AppError NO-UNDO.\n\
         DEFINE VARIABLE s AS OpenEdge.Core.String NO-UNDO.\n\
         e = NEW Progress.Lang.AppError().\n",
    );
    assert_eq!(builtin_of(&sem, "e"), Some("Progress.Lang.AppError"));
    assert_eq!(builtin_of(&sem, "s"), Some("OpenEdge.Core.String"));
}

#[test]
fn the_first_wildcard_is_sure() {
    let sem = analyze("USING Progress.Lang.*.\nDEFINE VARIABLE e AS AppError NO-UNDO.\n");
    assert_eq!(builtin_of(&sem, "e"), Some("Progress.Lang.AppError"));
}

#[test]
fn an_earlier_import_that_was_not_looked_up_is_not_sure() {
    let source = "USING acme.*.\nUSING Progress.Lang.*.\nDEFINE VARIABLE e AS AppError NO-UNDO.\n";
    assert_eq!(var_type(&analyze(source), "e"), ResolvedType::Unknown);
    assert_eq!(
        var_type(&analyze_with(source, Some(&PathlessEmpty)), "e"),
        ResolvedType::Unknown
    );
    // An index that searched every candidate makes it sure.
    let searched = analyze_with(source, Some(&SearchedEmpty));
    assert_eq!(builtin_of(&searched, "e"), Some("Progress.Lang.AppError"));

    let explicit = "USING acme.AppError.\nDEFINE VARIABLE e AS AppError NO-UNDO.\n";
    assert_eq!(var_type(&analyze(explicit), "e"), ResolvedType::Unknown);
}

#[test]
fn a_bare_name_is_not_sure_before_the_unnamed_package_was_looked_up() {
    let source = "DEFINE VARIABLE e AS AppError NO-UNDO.\n";
    assert_eq!(var_type(&analyze(source), "e"), ResolvedType::Unknown);
    let searched = analyze_with(source, Some(&SearchedEmpty));
    assert_eq!(builtin_of(&searched, "e"), Some("Progress.Lang.AppError"));
}

#[test]
fn a_class_of_the_file_named_like_a_built_in_type_wins() {
    let sem = analyze(
        "CLASS AppError:\n\
         METHOD PUBLIC VOID Go():\n\
         DEFINE VARIABLE e AS AppError NO-UNDO.\n\
         END METHOD.\n\
         END CLASS.\n",
    );
    let sym = class_of(&sem, "e");
    assert!(sem.symbols.builtin_class(sym).is_none());
}

#[test]
fn a_generic_type_has_no_type() {
    let sem =
        analyze("DEFINE VARIABLE l AS Progress.Collections.List<OpenEdge.Core.String> NO-UNDO.\n");
    assert_eq!(var_type(&sem, "l"), ResolvedType::Unknown);
}

#[test]
fn the_chain_of_app_error_is_recorded() {
    let sem = analyze("DEFINE VARIABLE e AS Progress.Lang.AppError NO-UNDO.\n");
    let app = class_of(&sem, "e");
    let name = |sym: SymbolId| sem.symbols.builtin_class(sym).unwrap().name;
    let pro: Vec<_> = sem.symbols.resolved_supertypes(app).to_vec();
    assert_eq!(
        pro.iter().map(|s| name(*s)).collect::<Vec<_>>(),
        ["Progress.Lang.ProError"]
    );
    let up: Vec<_> = sem
        .symbols
        .resolved_supertypes(pro[0])
        .iter()
        .map(|s| name(*s))
        .collect();
    assert_eq!(up, ["Progress.Lang.Object", "Progress.Lang.Error"]);
    assert!(!sem.symbols.has_incomplete_chain(app));
}

#[test]
fn an_unknown_header_records_an_incomplete_chain() {
    let sem = analyze("DEFINE VARIABLE b AS Progress.IO.ByteOrder NO-UNDO.\n");
    assert!(sem.symbols.has_incomplete_chain(class_of(&sem, "b")));
}

#[test]
fn the_lattice_climbs_built_in_chains() {
    let sem = analyze(
        "CLASS Thing:\n\
         METHOD PUBLIC VOID Go():\n\
         DEFINE VARIABLE t AS Thing NO-UNDO.\n\
         DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.\n\
         DEFINE VARIABLE a AS Progress.Lang.AppError NO-UNDO.\n\
         DEFINE VARIABLE e AS Progress.Lang.Error NO-UNDO.\n\
         DEFINE VARIABLE s AS Progress.Lang.SysError NO-UNDO.\n\
         END METHOD.\n\
         END CLASS.\n",
    );
    let lattice = ClassLattice::new(&sem.symbols);
    let [t, o, a, e, s] = ["t", "o", "a", "e", "s"].map(|n| class_of(&sem, n));
    assert_eq!(lattice.reaches(t, o), Reach::Widens);
    assert_eq!(lattice.reaches(a, e), Reach::Widens);
    assert_eq!(lattice.reaches(a, o), Reach::Widens);
    assert_eq!(lattice.reaches(s, a), Reach::DoesNot);
    assert_eq!(lattice.reaches(o, a), Reach::DoesNot);
}

#[test]
fn a_catch_variable_has_its_class_type() {
    let sem = analyze(
        "DO ON ERROR UNDO, LEAVE:\n\
         CATCH err AS Progress.Lang.Error:\n\
         END CATCH.\n\
         END.\n",
    );
    assert_eq!(builtin_of(&sem, "err"), Some("Progress.Lang.Error"));
}
