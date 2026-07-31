//! Inheritance-aware assignability, end to end through `type-mismatch-assignment`.
//!
//! The subject is one rule (`LINT0004`) over one predicate
//! (`oxabl_semantic::assignable`), which now consults the class lattice the
//! symbol table records instead of comparing class symbols by identity.
//!
//! **Read the split before adding a scenario here.** Only the *in-file* cases
//! exercise the lattice end to end. A cross-file class is still typed
//! `ResolvedType::Unknown` by the firewall in `oxabl_semantic::check` — kept on
//! purpose, because lifting it would turn cross-file types into new (correct)
//! findings and this phase's contract is zero drift — so a cross-file scenario
//! goes silent whether the walk works or not. Those scenarios below therefore
//! assert the silence *and* the reason for it, so that the day the firewall lifts
//! the reason assertion fails loudly instead of the test quietly becoming inert.
//! The walk itself is asserted directly, against a hand-built lattice, in
//! `oxabl_semantic::coercion`'s unit tests.

use std::path::PathBuf;

use oxabl_ast::{NodeId, Statement, StatementKind};
use oxabl_common::{Diagnostic, FileId};
use oxabl_index::BatchIndex;
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_lint::{LINT0004, type_mismatch_assignment};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, ResolvedType, Semantic, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

fn parse(source: &str) -> Vec<Statement> {
    let tokens = oxabl_lexer::tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}",
        program.errors
    );
    program.statements
}

/// `LINT0004` findings for `source`, analyzed with no index — the single-file
/// world every client is in today.
fn lint(source: &str) -> (Vec<Statement>, Semantic, Vec<Diagnostic>) {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    let diags = type_mismatch_assignment::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0004)
        .collect();
    (stmts, sem, diags)
}

/// The same, against a real [`BatchIndex`] over an in-memory workspace rooted at
/// `/src` — the harness shape `cross_file_inheritance.rs` established.
fn lint_with_index(
    source: &str,
    workspace: &[(&str, &str)],
) -> (Vec<Statement>, Semantic, Vec<Diagnostic>) {
    let mut fs = InMemoryFileSystem::new();
    for (path, contents) in workspace {
        fs.insert(PathBuf::from(path), *contents);
    }
    let paths = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &paths);
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    let sem = analyze_file(&stmts, &ctx);
    let diags = type_mismatch_assignment::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0004)
        .collect();
    (stmts, sem, diags)
}

/// NodeId of the value expression of the last `target = value` in the file.
///
/// The *last*, because every fixture puts its subject assignment at the end
/// after whatever declarations it needs.
fn last_assignment_value_id(stmts: &[Statement]) -> NodeId {
    fn find(stmts: &[Statement], out: &mut Option<NodeId>) {
        for s in stmts {
            match &s.kind {
                StatementKind::Assignment { value, .. } => *out = Some(value.id),
                StatementKind::Procedure { body, .. }
                | StatementKind::Block(body)
                | StatementKind::Method { body, .. }
                | StatementKind::Class { body, .. } => find(body, out),
                _ => {}
            }
        }
    }
    let mut out = None;
    find(stmts, &mut out);
    out.expect("fixture must contain an assignment statement")
}

/// Synthesized type of the value side of the subject assignment.
fn value_type(stmts: &[Statement], sem: &Semantic) -> ResolvedType {
    let id = last_assignment_value_id(stmts);
    sem.types
        .get(id)
        .cloned()
        .expect("the check pass types every expression")
}

/// Declared type recorded for the variable named `name`.
fn declared_type(sem: &Semantic, name: &str) -> Option<ResolvedType> {
    let atom = OxablAtom::from(name);
    sem.symbols
        .iter()
        .find(|(_, s)| s.name == atom)
        .and_then(|(_, s)| s.data_type.clone())
}

// ---------------------------------------------------------------------------
// In-file: the lattice really runs here
// ---------------------------------------------------------------------------

/// Parent and child in one file, a parent-typed variable, and a child instance
/// assigned into it. Both classes are locally declared, so both sides type as a
/// real `ResolvedType::Class` and the walk is what decides the verdict.
const CHILD_TO_PARENT: &str = r#"CLASS parent-cls:
END CLASS.

CLASS child-cls INHERITS parent-cls:
END CLASS.

DEFINE VARIABLE v-parent AS CLASS parent-cls NO-UNDO.
v-parent = NEW child-cls().
"#;

#[test]
fn a_child_assigned_to_a_parent_typed_variable_is_silent_in_file() {
    // The characterization case. Before the lattice existed this fired a
    // `type-mismatch-assignment` error — a live false positive, and the one
    // admissible finding delta of this whole phase. Both sides being real class
    // types is asserted first, so a future regression that silences this by
    // dropping a type back to `Unknown` cannot pass as a fix.
    let (stmts, sem, diags) = lint(CHILD_TO_PARENT);
    assert!(
        matches!(value_type(&stmts, &sem), ResolvedType::Class(_)),
        "the child instance must type as a real class, or this proves nothing"
    );
    assert!(
        matches!(
            declared_type(&sem, "v-parent"),
            Some(ResolvedType::Class(_))
        ),
        "the parent-typed variable must type as a real class too"
    );
    assert!(
        diags.is_empty(),
        "a subclass assigned where its parent is expected is legal ABL: {diags:?}"
    );
}

#[test]
fn a_grandchild_assigned_to_a_grandparent_typed_variable_is_silent_in_file() {
    let source = r#"CLASS grand-cls:
END CLASS.

CLASS mid-cls INHERITS grand-cls:
END CLASS.

CLASS child-cls INHERITS mid-cls:
END CLASS.

DEFINE VARIABLE v-grand AS CLASS grand-cls NO-UNDO.
v-grand = NEW child-cls().
"#;
    let (stmts, sem, diags) = lint(source);
    assert!(matches!(value_type(&stmts, &sem), ResolvedType::Class(_)));
    assert!(diags.is_empty(), "unexpected diags: {diags:?}");
}

#[test]
fn a_class_assigned_to_an_interface_typed_variable_it_implements_is_silent_in_file() {
    let source = r#"INTERFACE i-calc:
END INTERFACE.

CLASS calc-cls IMPLEMENTS i-calc:
END CLASS.

DEFINE VARIABLE v-calc AS CLASS i-calc NO-UNDO.
v-calc = NEW calc-cls().
"#;
    let (stmts, sem, diags) = lint(source);
    assert!(matches!(value_type(&stmts, &sem), ResolvedType::Class(_)));
    assert!(
        matches!(declared_type(&sem, "v-calc"), Some(ResolvedType::Class(_))),
        "an interface name in `AS CLASS` resolves to the interface symbol"
    );
    assert!(diags.is_empty(), "unexpected diags: {diags:?}");
}

#[test]
fn a_parent_assigned_to_a_child_typed_variable_still_fires() {
    // Widening is one-directional. Losing this would be a silent correctness
    // regression, so it is asserted down to the code.
    let source = r#"CLASS parent-cls:
END CLASS.

CLASS child-cls INHERITS parent-cls:
END CLASS.

DEFINE VARIABLE v-child AS CLASS child-cls NO-UNDO.
v-child = NEW parent-cls().
"#;
    let (_stmts, _sem, diags) = lint(source);
    assert_eq!(diags.len(), 1, "expected exactly one finding: {diags:?}");
    assert_eq!(diags[0].code.0, LINT0004);
}

#[test]
fn two_unrelated_classes_are_not_assignable_in_either_direction() {
    let source = r#"CLASS alpha-cls:
END CLASS.

CLASS beta-cls:
END CLASS.

DEFINE VARIABLE v-alpha AS CLASS alpha-cls NO-UNDO.
DEFINE VARIABLE v-beta AS CLASS beta-cls NO-UNDO.
v-alpha = NEW beta-cls().
v-beta = NEW alpha-cls().
"#;
    let (_stmts, _sem, diags) = lint(source);
    assert_eq!(diags.len(), 2, "both directions are mismatches: {diags:?}");
}

#[test]
fn a_self_inheriting_class_terminates_rather_than_hanging() {
    // A real shape in broken code. Reaching the assertion at all is the point;
    // the verdict is incidental (nothing widens, so the mismatch stands).
    let source = r#"CLASS loop-cls INHERITS loop-cls:
END CLASS.

CLASS other-cls:
END CLASS.

DEFINE VARIABLE v-other AS CLASS other-cls NO-UNDO.
v-other = NEW loop-cls().
"#;
    let (_stmts, _sem, diags) = lint(source);
    assert_eq!(diags.len(), 1, "unexpected diags: {diags:?}");
}

#[test]
fn a_class_naming_a_parent_no_file_declares_is_not_assignable_to_an_unrelated_class() {
    // An unresolvable supertype contributes nothing to the walk, and must not be
    // mistaken for "matches anything".
    let source = r#"CLASS child-cls INHERITS nowhere-cls:
END CLASS.

CLASS other-cls:
END CLASS.

DEFINE VARIABLE v-other AS CLASS other-cls NO-UNDO.
v-other = NEW child-cls().
"#;
    let (_stmts, _sem, diags) = lint(source);
    assert_eq!(diags.len(), 1, "unexpected diags: {diags:?}");
}

// ---------------------------------------------------------------------------
// Cross-file: silent today, and the reason is asserted
// ---------------------------------------------------------------------------

#[test]
fn a_child_assigned_to_a_cross_file_parent_typed_variable_is_silent() {
    // The child is declared here; the parent lives in another file. Silent —
    // but *not* because the walk ran: the `AS CLASS orders.base` declaration is
    // never upgraded past `ResolvedType::Unknown` (resolve's
    // `indexed_receiver_class` keeps the cross-file link out of the type
    // lattice), so the rule has nothing to compare. The lattice is what will
    // carry this case once that firewall lifts; until then the assertion below
    // pins the real cause, so this test cannot go quietly inert.
    let source = r#"CLASS orders.child INHERITS orders.base:
END CLASS.

DEFINE VARIABLE v-base AS CLASS orders.base NO-UNDO.
v-base = NEW orders.child().
"#;
    let workspace = [("/src/orders/base.cls", "CLASS orders.base: END CLASS.")];

    let (stmts, sem, diags) = lint_with_index(source, &workspace);
    assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    assert!(
        matches!(value_type(&stmts, &sem), ResolvedType::Class(_)),
        "the locally declared child does type as a real class"
    );
    assert_eq!(
        declared_type(&sem, "v-base"),
        Some(ResolvedType::Unknown),
        "the cross-file parent-typed declaration stays at the lattice bottom — \
         that, not the chain walk, is why this is silent today"
    );

    // And with no index attached, exactly the same answer: the R11 firewall.
    let (_stmts, plain_sem, plain_diags) = lint(source);
    assert!(plain_diags.is_empty(), "unexpected diags: {plain_diags:?}");
    assert_eq!(
        declared_type(&plain_sem, "v-base"),
        Some(ResolvedType::Unknown)
    );
}

#[test]
fn a_cross_file_child_assigned_to_a_local_parent_typed_variable_is_silent() {
    // The mirror image: the parent is local, the child lives elsewhere. Silent
    // because `check`'s firewall types an index-synthesized class as `Unknown`,
    // which is asserted rather than assumed. When that arm is removed this
    // assertion fails, and the chain walk must be what keeps the case silent.
    let source = r#"CLASS parent-cls:
END CLASS.

DEFINE VARIABLE v-parent AS CLASS parent-cls NO-UNDO.
v-parent = NEW orders.child().
"#;
    let workspace = [(
        "/src/orders/child.cls",
        "CLASS orders.child INHERITS parent-cls: END CLASS.",
    )];

    let (stmts, sem, diags) = lint_with_index(source, &workspace);
    assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    assert_eq!(
        value_type(&stmts, &sem),
        ResolvedType::Unknown,
        "an index-synthesized class is held at the lattice bottom by the \
         firewall in `oxabl_semantic::check` — that is why this is silent"
    );
    assert!(
        matches!(
            declared_type(&sem, "v-parent"),
            Some(ResolvedType::Class(_))
        ),
        "the local parent-typed declaration is a real class type, so the only \
         thing keeping the rule quiet is the value side"
    );
}

#[test]
fn an_unresolved_cross_file_class_stays_unknown_and_silent() {
    // No file anywhere declares `orders.ghost`. Today's answer must stand: the
    // declaration types as `Unknown` and nothing is reported, with or without an
    // index attached.
    let source = r#"DEFINE VARIABLE v-ghost AS CLASS orders.ghost NO-UNDO.
DEFINE VARIABLE v-n AS INTEGER NO-UNDO.
v-ghost = v-n.
"#;
    for (label, (_stmts, sem, diags)) in [
        ("with an index", lint_with_index(source, &[])),
        ("without an index", lint(source)),
    ] {
        assert!(diags.is_empty(), "unexpected diags {label}: {diags:?}");
        assert_eq!(
            declared_type(&sem, "v-ghost"),
            Some(ResolvedType::Unknown),
            "a class nothing declares stays at the lattice bottom {label}"
        );
    }
}
