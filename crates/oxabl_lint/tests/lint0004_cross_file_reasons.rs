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

/// The cross-file reasons under test, so every scenario runs against all of
/// them. `AbsentFromWorkspace` is deliberately **not** here: it is the one
/// cross-file reason `undefined-symbol` reports, and the tests below assert
/// silence.
const NEW_REASONS: [UnresolvedReason; 2] = [
    UnresolvedReason::PresentButUnusable,
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
fn a_name_no_configured_path_provides_is_absent_from_the_workspace() {
    // The genuine path-search miss: an index is attached, every configured path
    // was searched, and no file spells the name.
    let (_stmts, sem) = with_index("RUN never-shipped.p.\n", &[]);
    assert_eq!(
        reasons_for(&sem, "never-shipped.p"),
        vec![UnresolvedReason::AbsentFromWorkspace]
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

/// Analyze `source` against a batch index over `workspace` that searches
/// **nowhere** — an index with no configured path.
fn with_index_searching_nothing(source: &str, workspace: &[(&str, &str)]) -> Semantic {
    use std::path::PathBuf;
    let mut fs = oxabl_workspace::InMemoryFileSystem::new();
    for (path, contents) in workspace {
        fs.insert(PathBuf::from(path), *contents);
    }
    let index = oxabl_index::BatchIndex::new(&fs, &[]);
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    analyze_file(&stmts, &ctx)
}

#[test]
fn an_inaccessible_inherited_member_is_present_but_unusable() {
    // The member is declared, `PRIVATE`, and therefore not passed on. Reporting it
    // as absent would say "no such symbol" about a symbol in the parent's own
    // source — which is why this reason exists.
    let base = "CLASS orders.calc-base:\n\
                    METHOD PRIVATE INTEGER calc-secret():\n\
                        RETURN 0.\n\
                    END METHOD.\n\
                END CLASS.\n";
    let child = "CLASS orders.child INHERITS orders.calc-base:\n\
                     METHOD PUBLIC VOID run-it():\n\
                         DEFINE VARIABLE v-n AS INTEGER NO-UNDO.\n\
                         v-n = calc-secret().\n\
                     END METHOD.\n\
                 END CLASS.\n";
    let (_stmts, sem) = with_index(child, &[("/src/orders/calc-base.cls", base)]);
    assert_eq!(
        reasons_for(&sem, "calc-secret"),
        vec![UnresolvedReason::PresentButUnusable]
    );
}

#[test]
fn a_protected_member_reached_from_an_unrelated_class_is_present_but_unusable() {
    // Not an inheritance question at all: the caller holds an instance, and
    // `PROTECTED` is not part of the public surface. Same situation, same answer.
    let base = "CLASS orders.calc-base:\n\
                    METHOD PROTECTED INTEGER calc-rate():\n\
                        RETURN 0.\n\
                    END METHOD.\n\
                END CLASS.\n";
    let caller = "USING orders.calc-base.\n\
                  DEFINE VARIABLE v-calc AS CLASS orders.calc-base NO-UNDO.\n\
                  DEFINE VARIABLE v-n AS INTEGER NO-UNDO.\n\
                  v-n = v-calc:calc-rate().\n";
    let (_stmts, sem) = with_index(caller, &[("/src/orders/calc-base.cls", base)]);
    assert_eq!(
        reasons_for(&sem, "calc-rate"),
        vec![UnresolvedReason::PresentButUnusable]
    );
}

#[test]
fn a_located_but_unparseable_dependency_is_present_but_unusable() {
    // The file is on the search path and oxabl cannot parse it. Blaming the user
    // for that would be worse than silence, so it is not `AbsentFromWorkspace` —
    // the one reason `undefined-symbol` reports.
    let broken = "DEFINE VARIABLE .\n";
    let source = "RUN post-order.p.\n";
    let (_stmts, sem) = with_index(source, &[("/src/post-order.p", broken)]);
    assert_eq!(
        reasons_for(&sem, "post-order.p"),
        vec![UnresolvedReason::PresentButUnusable],
        "located and unreadable is a fact about oxabl, not about the workspace"
    );
}

#[test]
fn an_index_with_no_search_path_reports_external_not_absent() {
    // R17. With nowhere to look, "we did not look" is the truthful answer — and it
    // is what keeps the browser, which has no filesystem, from disagreeing with the
    // CLI about a diagnostic.
    let sem = with_index_searching_nothing("RUN never-shipped.p.\n", &[]);
    assert_eq!(
        reasons_for(&sem, "never-shipped.p"),
        vec![UnresolvedReason::External]
    );

    let using = "USING myapp.cache.\nMESSAGE \"hi\".\n";
    let sem = with_index_searching_nothing(using, &[]);
    assert_eq!(
        reasons_for(&sem, "myapp.cache"),
        vec![UnresolvedReason::External]
    );

    // The contrast, on the same source: give the index a path to search and the
    // same miss becomes a claim about the workspace.
    let (_stmts, searched) = with_index(using, &[]);
    assert_eq!(
        reasons_for(&searched, "myapp.cache"),
        vec![UnresolvedReason::AbsentFromWorkspace]
    );
}

#[test]
fn every_rule_is_silent_for_both_new_reasons() {
    // This unit adds no diagnostic: it splits a reason and leaves both halves
    // skip-listed. `undefined-symbol` reporting `AbsentFromWorkspace` is the next
    // unit's job, which is why that reason is not in this list.
    let sources = [
        // An inaccessible inherited member.
        (
            "CLASS orders.child INHERITS orders.calc-base:\n\
                 METHOD PUBLIC VOID run-it():\n\
                     DEFINE VARIABLE v-n AS INTEGER NO-UNDO.\n\
                     v-n = calc-secret().\n\
                     MESSAGE v-n.\n\
                 END METHOD.\n\
             END CLASS.\n",
            "/src/orders/calc-base.cls",
            "CLASS orders.calc-base:\n\
                 METHOD PRIVATE INTEGER calc-secret():\n\
                     RETURN 0.\n\
                 END METHOD.\n\
             END CLASS.\n",
        ),
        // A located-but-unparseable dependency.
        (
            "RUN post-order.p.\n",
            "/src/post-order.p",
            "DEFINE VARIABLE .\n",
        ),
    ];
    for (source, path, sibling) in sources {
        let stmts = parse(source);
        let schema = customer_schema();
        let mut fs = oxabl_workspace::InMemoryFileSystem::new();
        fs.insert(std::path::PathBuf::from(path), sibling);
        let dirs = vec![std::path::PathBuf::from("/src")];
        let index = oxabl_index::BatchIndex::new(&fs, &dirs);
        let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
        let sem = analyze_file(&stmts, &ctx);
        for (rule, code) in [
            (undefined_symbol::run as fn(_, _, _) -> _, LINT0001),
            (unknown_table_or_field::run, LINT0003),
            (type_mismatch_assignment::run, LINT0004),
        ] {
            let diags: Vec<Diagnostic> = rule(&stmts, &sem, &ctx)
                .into_iter()
                .filter(|d| d.code.0 == code)
                .collect();
            assert!(
                diags.is_empty(),
                "{code} fired for a `PresentButUnusable` name: {diags:?}\nin:\n{source}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// What `undefined-symbol` reports, and what it will not
// ---------------------------------------------------------------------------

fn lint0001_for(source: &str, workspace: &[(&str, &str)]) -> Vec<Diagnostic> {
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
    undefined_symbol::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0001)
        .collect()
}

fn lint0001_without_any_index(source: &str) -> Vec<Diagnostic> {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    undefined_symbol::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0001)
        .collect()
}

/// The three spellings AE2 names. The `AS CLASS pkg.Missing` declaration is
/// deliberately absent: `DataType::Class` is a bare `String` with no span, so
/// there is nothing to underline and nothing for the rule to see.
const ABSENT_SPELLINGS: [&str; 3] = [
    "USING pkg.Missing.\nMESSAGE \"hi\".\n",
    "DEFINE VARIABLE v-thing AS CLASS pkg.Missing NO-UNDO.\nv-thing = NEW pkg.Missing().\n",
    "RUN missing.p.\n",
];

#[test]
fn each_absent_spelling_is_reported_with_the_search_path_help() {
    // Covers AE2. An index over a path that contains no `pkg.Missing` and no
    // `missing.p`: every one of these names is genuinely unreachable, the way ABL
    // itself would find them unreachable at run time.
    for source in ABSENT_SPELLINGS {
        let found = lint0001_for(
            source,
            &[("/src/pkg/Other.cls", "CLASS pkg.Other: END CLASS.")],
        );
        assert_eq!(found.len(), 1, "one finding for:\n{source}\ngot {found:?}");
        assert!(
            found[0]
                .help
                .as_deref()
                .is_some_and(|h| h.contains("include_paths") && h.contains("-I")),
            "R18: the finding names the search-path configuration: {:?}",
            found[0].help
        );
        // The span lands on real text rather than on a default.
        let (start, end) = (
            found[0].span.span.start as usize,
            found[0].span.span.end as usize,
        );
        assert!(start < end && end <= source.len());
        assert!(!source[start..end].trim().is_empty());
    }
}

#[test]
fn the_same_spellings_are_silent_with_no_index_attached() {
    // Covers AE3. Nothing looked, so nothing is claimed — the pre-existing
    // behavior every client had before an index could be attached.
    for source in ABSENT_SPELLINGS {
        assert!(
            lint0001_without_any_index(source).is_empty(),
            "no index attached, so nothing may be reported for:\n{source}"
        );
    }
}

#[test]
fn a_runtime_computed_run_target_is_never_reported() {
    // Covers AE4. `Unknowable` is not a fact about the workspace, and no amount of
    // indexing would change it.
    let source = "DEFINE VARIABLE c-target AS CHARACTER NO-UNDO.\n\
                  c-target = \"missing.p\".\n\
                  RUN VALUE(c-target).\n";
    assert!(lint0001_for(source, &[]).is_empty());
}

#[test]
fn a_shipped_system_namespace_is_never_reported() {
    // Covers AE6, and R15. These libraries come with the AVM and have no source on
    // any search path, so their absence says nothing about the user's workspace —
    // reporting them would make the rule fire on ordinary correct code.
    let shipped = [
        "USING Progress.Json.ObjectModel.JsonObject.\nMESSAGE \"hi\".\n",
        "USING OpenEdge.Net.HTTP.IHttpRequest.\nMESSAGE \"hi\".\n",
        "v-x = NEW System.Text.StringBuilder().\nMESSAGE v-x.\n",
        "v-x = NEW Microsoft.Win32.Registry().\nMESSAGE v-x.\n",
    ];
    for source in shipped {
        let with_schema = {
            let stmts = parse(source);
            let schema = customer_schema();
            let mut fs = oxabl_workspace::InMemoryFileSystem::new();
            fs.insert(
                std::path::PathBuf::from("/src/pkg/Other.cls"),
                "CLASS pkg.Other: END CLASS.",
            );
            let dirs = vec![std::path::PathBuf::from("/src")];
            let index = oxabl_index::BatchIndex::new(&fs, &dirs);
            let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
            let sem = analyze_file(&stmts, &ctx);
            undefined_symbol::run(&stmts, &sem, &ctx)
                .into_iter()
                .filter(|d| d.code.0 == LINT0001 && !d.message.contains("v-x"))
                .collect::<Vec<_>>()
        };
        assert!(
            with_schema.is_empty(),
            "a shipped system class must never be reported: {with_schema:?}\nin:\n{source}"
        );
        let without: Vec<Diagnostic> = lint0001_for(source, &[])
            .into_iter()
            .filter(|d| !d.message.contains("v-x"))
            .collect();
        assert!(without.is_empty(), "{without:?}");
    }

    // The carve-out is a prefix on the first segment, not a substring: a user class
    // whose package merely starts with the same letters is still reported.
    let user = "USING Systems.Cache.\nMESSAGE \"hi\".\n";
    assert_eq!(
        lint0001_for(user, &[]).len(),
        1,
        "`Systems.Cache` is nobody's shipped library"
    );
}

#[test]
fn an_inaccessible_inherited_member_is_never_reported() {
    let base = "CLASS orders.calc-base:\n\
                    METHOD PRIVATE INTEGER calc-secret():\n\
                        RETURN 0.\n\
                    END METHOD.\n\
                END CLASS.\n";
    let child = "CLASS orders.child INHERITS orders.calc-base:\n\
                     METHOD PUBLIC VOID run-it():\n\
                         DEFINE VARIABLE v-n AS INTEGER NO-UNDO.\n\
                         v-n = calc-secret().\n\
                         MESSAGE v-n.\n\
                     END METHOD.\n\
                 END CLASS.\n";
    assert!(
        lint0001_for(child, &[("/src/orders/calc-base.cls", base)]).is_empty(),
        "the member exists — `undefined symbol` would be a false claim"
    );
}

#[test]
fn a_member_behind_an_unexpanded_include_gains_no_finding_from_the_index() {
    // The index does not expand includes, so a member spliced in from one is
    // invisible to it. Two shapes, and they differ — which is worth stating
    // precisely, because the difference is deliberate.
    //
    // Qualified, the name can only be a member, so a lookup that comes up empty
    // against a class the index *did* answer for is `PresentButUnusable` and
    // silent. Unqualified, the same name could just as easily be a misspelled local
    // variable — which is the case `undefined-symbol` exists for — so it stays
    // `NotInScope` and fires, exactly as it does with no index attached. That
    // finding is pre-existing, not something widening the rule introduced, and the
    // assertion is therefore about the *delta*: attaching an index adds nothing
    // here either way.
    let base = "CLASS orders.calc-base:\n{members.i}\nEND CLASS.\n";
    let workspace = [("/src/orders/calc-base.cls", base)];

    let qualified = "USING orders.calc-base.\n\
                     DEFINE VARIABLE v-base AS CLASS orders.calc-base NO-UNDO.\n\
                     DEFINE VARIABLE v-n AS INTEGER NO-UNDO.\n\
                     v-n = v-base:spliced-in().\n";
    assert!(
        lint0001_for(qualified, &workspace).is_empty(),
        "a member the index cannot see is not a name it may call absent"
    );

    let unqualified = "CLASS orders.child INHERITS orders.calc-base:\n\
                           METHOD PUBLIC VOID run-it():\n\
                               DEFINE VARIABLE v-n AS INTEGER NO-UNDO.\n\
                               v-n = spliced-in().\n\
                               MESSAGE v-n.\n\
                           END METHOD.\n\
                       END CLASS.\n";
    let with_index = lint0001_for(unqualified, &workspace);
    let without = lint0001_without_any_index(unqualified);
    assert_eq!(
        with_index.len(),
        without.len(),
        "the unqualified finding is pre-existing; the index must not add to it"
    );
    assert!(
        with_index.iter().all(|d| d.help.is_none()),
        "and it is the plain not-in-scope finding, not a workspace-absence claim"
    );
}

#[test]
fn the_other_two_rules_stay_silent_for_every_cross_file_reason() {
    // R5's other half: widening `undefined-symbol` is the whole behavior change,
    // and the two rules with their own skip-lists are untouched.
    for source in ABSENT_SPELLINGS {
        let stmts = parse(source);
        let schema = customer_schema();
        let mut fs = oxabl_workspace::InMemoryFileSystem::new();
        fs.insert(
            std::path::PathBuf::from("/src/pkg/Other.cls"),
            "CLASS pkg.Other: END CLASS.",
        );
        let dirs = vec![std::path::PathBuf::from("/src")];
        let index = oxabl_index::BatchIndex::new(&fs, &dirs);
        let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
        let sem = analyze_file(&stmts, &ctx);
        for (rule, code) in [
            (unknown_table_or_field::run as fn(_, _, _) -> _, LINT0003),
            (type_mismatch_assignment::run, LINT0004),
        ] {
            let diags: Vec<Diagnostic> = rule(&stmts, &sem, &ctx)
                .into_iter()
                .filter(|d| d.code.0 == code)
                .collect();
            assert!(diags.is_empty(), "{code} fired: {diags:?}\nin:\n{source}");
        }
    }
}
