//! The cross-file unresolved reasons, who produces each one, and which rules stay
//! silent for them.
//!
//! Two halves. The skip-list tests build a real `Semantic` from synthetic ABL and
//! then rewrite the reference entries in question, which is how a rule's treatment
//! of a reason is exercised independently of whether any input can currently
//! produce it — the trap being `type-mismatch-assignment`, whose early return
//! names each reason by hand and would otherwise fall through for anything new.
//! The producer tests at the bottom go the other way: real ABL through a real
//! index, asserting which situation mints which reason, so "searched and absent",
//! "not statically knowable", and "we did not look" cannot quietly collapse into
//! each other.

use oxabl_ast::{NodeId, Statement, StatementKind};
use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{
    LINT0001, LINT0003, LINT0004, type_mismatch_assignment, undefined_symbol,
    unknown_table_or_field,
};
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::customer_schema};
use oxabl_semantic::{
    AnalysisContext, Resolution, Semantic, UnresolvedReason, analyze_file, resolve_pass,
};

/// The reasons under test, so every scenario runs against both.
const NEW_REASONS: [UnresolvedReason; 2] = [
    UnresolvedReason::NotFoundInWorkspace,
    UnresolvedReason::Unknowable,
];

fn parse(source: &str) -> Vec<Statement> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(
        program.errors.is_empty(),
        "parse errors for {source:?}: {:?}",
        program.errors
    );
    program.statements
}

/// Rewrite every `NotInScope` resolution to `reason`, returning how many
/// entries were rewritten so a test can assert it actually had a subject.
fn retag_not_in_scope(sem: &mut Semantic, reason: UnresolvedReason) -> usize {
    let targets: Vec<(NodeId, Resolution)> = sem
        .references
        .iter()
        .filter_map(|(id, res)| match res {
            Resolution::Unresolved {
                name,
                reason: UnresolvedReason::NotInScope,
            } => Some((
                id,
                Resolution::Unresolved {
                    name: name.clone(),
                    reason,
                },
            )),
            _ => None,
        })
        .collect();
    let n = targets.len();
    for (id, res) in targets {
        sem.references.insert(id, res);
    }
    n
}

/// NodeId of the value expression of the first `target = value` statement.
fn first_assignment_value_id(stmts: &[Statement]) -> NodeId {
    fn find(stmts: &[Statement]) -> Option<NodeId> {
        for s in stmts {
            match &s.kind {
                StatementKind::Assignment { value, .. } => return Some(value.id),
                StatementKind::Procedure { body, .. } | StatementKind::Block(body) => {
                    if let Some(id) = find(body) {
                        return Some(id);
                    }
                }
                _ => {}
            }
        }
        None
    }
    find(stmts).expect("fixture should contain an assignment statement")
}

// ---------------------------------------------------------------------------
// LINT0001 — undefined-symbol
// ---------------------------------------------------------------------------

#[test]
fn undefined_symbol_stays_silent_for_the_new_reasons() {
    // `x-missing` is genuinely not declared, so the baseline resolution is
    // `NotInScope` and the rule fires. Retagged as a cross-file reason, the
    // same reference must produce nothing.
    let src = "\
PROCEDURE p:
  DEFINE VARIABLE v-n AS INTEGER NO-UNDO.
  v-n = x-missing.
END PROCEDURE.
";
    let stmts = parse(src);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, src, &schema);

    let baseline = undefined_symbol::run(&stmts, &analyze_file(&stmts, &ctx), &ctx);
    assert_eq!(
        baseline.iter().filter(|d| d.code.0 == LINT0001).count(),
        1,
        "fixture must fire LINT0001 before retagging: {baseline:?}"
    );

    for reason in NEW_REASONS {
        let mut sem = analyze_file(&stmts, &ctx);
        assert!(retag_not_in_scope(&mut sem, reason) > 0);
        let diags = undefined_symbol::run(&stmts, &sem, &ctx);
        assert!(diags.is_empty(), "LINT0001 fired for {reason:?}: {diags:?}");
    }
}

// ---------------------------------------------------------------------------
// LINT0004 — type-mismatch-assignment
// ---------------------------------------------------------------------------

/// Baseline + retagged runs for `v-n = v-c`, where only the *value*
/// expression's reference is rewritten (the target must keep resolving, or
/// the rule bails for an unrelated reason).
fn lint0004_for(reason: Option<UnresolvedReason>) -> Vec<Diagnostic> {
    let src = "\
PROCEDURE p:
  DEFINE VARIABLE v-n AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-c AS CHARACTER NO-UNDO.
  v-n = v-c.
END PROCEDURE.
";
    let stmts = parse(src);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, src, &schema);
    let mut sem = analyze_file(&stmts, &ctx);
    if let Some(reason) = reason {
        let value_id = first_assignment_value_id(&stmts);
        sem.references.insert(
            value_id,
            Resolution::Unresolved {
                name: oxabl_lexer::oxabl_atom::OxablAtom::from("v-c"),
                reason,
            },
        );
    }
    type_mismatch_assignment::run(&stmts, &sem, &ctx)
}

#[test]
fn type_mismatch_stays_silent_for_the_new_reasons() {
    let baseline = lint0004_for(None);
    assert_eq!(
        baseline.iter().filter(|d| d.code.0 == LINT0004).count(),
        1,
        "fixture must fire LINT0004 before retagging: {baseline:?}"
    );
    // The pre-existing suppression, asserted alongside so the widening is
    // visibly the same behavior rather than a new one.
    assert!(lint0004_for(Some(UnresolvedReason::External)).is_empty());

    for reason in NEW_REASONS {
        let diags = lint0004_for(Some(reason));
        assert!(diags.is_empty(), "LINT0004 fired for {reason:?}: {diags:?}");
    }
}

// ---------------------------------------------------------------------------
// LINT0003 — unknown-table-or-field
// ---------------------------------------------------------------------------

#[test]
fn unknown_field_stays_silent_for_the_new_reasons() {
    // With the Customer schema loaded, `Customer.NoSuchField` resolves to
    // `NotInScope` and LINT0003 fires. Retagged, the rule must not.
    let src = "\
PROCEDURE p:
  DISPLAY Customer.NoSuchField.
END PROCEDURE.
";
    let stmts = parse(src);
    let schema = customer_schema();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, src, &schema);

    let baseline = unknown_table_or_field::run(&stmts, &analyze_file(&stmts, &ctx), &ctx);
    assert_eq!(
        baseline.iter().filter(|d| d.code.0 == LINT0003).count(),
        1,
        "fixture must fire LINT0003 before retagging: {baseline:?}"
    );

    for reason in NEW_REASONS {
        let mut sem = analyze_file(&stmts, &ctx);
        assert!(retag_not_in_scope(&mut sem, reason) > 0);
        let diags = unknown_table_or_field::run(&stmts, &sem, &ctx);
        assert!(diags.is_empty(), "LINT0003 fired for {reason:?}: {diags:?}");
    }
}

// ---------------------------------------------------------------------------
// The reasons themselves
// ---------------------------------------------------------------------------

#[test]
fn no_index_produces_no_cross_file_reason() {
    // Named for what it checks rather than for when it was written: this run
    // attaches **no index**, and a resolve pass with nowhere to look must never
    // mint a cross-file reason. That is the invariant the `External` case rests
    // on — "we did not look" has to be reachable only when nothing looked — and it
    // outlives the index landing. The per-reason producer tests below are what the
    // old name promised would replace it; they are additions, not replacements.
    let src = "\
DEFINE VARIABLE v-n AS INTEGER NO-UNDO.
v-n = x-missing.
DISPLAY Customer.CustNum.
RUN some-external.p.
";
    let stmts = parse(src);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, src, &schema);
    let (scope_tree, mut symbols, _diags, rev) = oxabl_semantic::declare_pass(&stmts, &ctx);
    let (references, _types, _rd) = resolve_pass(&stmts, &ctx, &scope_tree, &mut symbols, rev);
    for (_, res) in references.iter() {
        if let Resolution::Unresolved { reason, .. } = res {
            assert!(
                !NEW_REASONS.contains(reason),
                "resolve pass produced {reason:?}, which has no producer yet"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Who produces each reason
// ---------------------------------------------------------------------------

/// Analyze `source` against a batch index over `workspace`, searching `/src`.
fn with_index(source: &str, workspace: &[(&str, &str)]) -> (Vec<Statement>, Semantic) {
    use std::path::PathBuf;
    let mut fs = oxabl_workspace::InMemoryFileSystem::new();
    for (path, contents) in workspace {
        fs.insert(PathBuf::from(path), *contents);
    }
    let dirs = vec![PathBuf::from("/src")];
    let index = oxabl_index::BatchIndex::new(&fs, &dirs);
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    let sem = analyze_file(&stmts, &ctx);
    (stmts, sem)
}

/// Every reason recorded under `name`, in reference order.
fn reasons_for(sem: &Semantic, name: &str) -> Vec<UnresolvedReason> {
    let atom = oxabl_lexer::oxabl_atom::OxablAtom::from(name);
    sem.references
        .iter()
        .filter_map(|(_, res)| match res {
            Resolution::Unresolved { name, reason } if *name == atom => Some(*reason),
            _ => None,
        })
        .collect()
}

#[test]
fn a_name_no_configured_path_provides_is_not_found_in_the_workspace() {
    // The genuine path-search miss: an index is attached, every configured path
    // was searched, and no file spells the name.
    let (_stmts, sem) = with_index("RUN never-shipped.p.\n", &[]);
    assert_eq!(
        reasons_for(&sem, "never-shipped.p"),
        vec![UnresolvedReason::NotFoundInWorkspace]
    );
}

#[test]
fn a_runtime_computed_target_is_unknowable() {
    // Not statically knowable, so no amount of indexing helps — which is the whole
    // distinction this reason carries.
    let source = "DEFINE VARIABLE c-name AS CHARACTER NO-UNDO.\nRUN VALUE(c-name).\n";
    let (_stmts, sem) = with_index(source, &[]);
    let unknowable = sem.references.iter().any(|(_, res)| {
        matches!(
            res,
            Resolution::Unresolved {
                reason: UnresolvedReason::Unknowable,
                ..
            }
        )
    });
    assert!(unknowable, "a computed RUN target records Unknowable");
}

#[test]
fn the_same_name_records_no_reason_at_all_when_no_index_is_attached() {
    // The pair that makes the reasons a real distinction rather than several
    // spellings of one: identical source, and the answer differs by whether
    // anything looked. With no index, a literal `RUN` target gets no reference
    // entry at all — which is a stronger silence than `External`, and is why the
    // rule cannot see it either way.
    let source = "RUN never-shipped.p.\n";
    let stmts = parse(source);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    assert_eq!(
        reasons_for(&sem, "never-shipped.p"),
        Vec::new(),
        "nothing looked, so nothing was recorded about the name"
    );
}

#[test]
fn a_class_name_only_the_workspace_could_provide_is_external_without_an_index() {
    // `External` proper: the reference *is* recorded, and the reason says the
    // lookup never happened. This is the pre-existing suppression state every rule
    // skip-lists, and the one the other reasons have to stay distinguishable from.
    let source = "DEFINE VARIABLE v-cache AS CLASS myapp.cache NO-UNDO.\n\
                  v-cache = NEW myapp.cache().\n";
    let stmts = parse(source);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    assert_eq!(
        reasons_for(&sem, "myapp.cache"),
        vec![UnresolvedReason::External],
        "with nothing attached, the honest answer is that we did not look"
    );
}
