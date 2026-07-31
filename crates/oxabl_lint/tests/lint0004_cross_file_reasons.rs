//! The two cross-file unresolved reasons must be as silent as `External`.
//!
//! `NotFoundInWorkspace` and `Unknowable` land before anything can produce
//! them, so these tests build a real `Semantic` from synthetic ABL and then
//! rewrite the reference entries in question — the only way to exercise the
//! skip-lists ahead of the workspace index. What they pin is the firewall: no
//! rule may treat a new reason as a finding, and the trap is
//! `type-mismatch-assignment`, whose early return names one reason by hand and
//! would otherwise fall through for anything new.

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
fn nothing_produces_the_new_reasons_yet() {
    // The firewall's other half: this unit adds the reasons but no producer,
    // so a resolve pass over ordinary ABL must never mint one. When the
    // workspace index lands, this test is the one that should be replaced.
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
