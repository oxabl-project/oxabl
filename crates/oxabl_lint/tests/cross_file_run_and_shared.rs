//! Literal and dynamic `RUN` targets, and the `SHARED` producer link: what a
//! program name reaches once the workspace index can answer for it, what it must
//! decline to guess at, and where the deliberately-deferred boundaries sit.
//!
//! Same harness shape as `cross_file_inheritance.rs` and `cross_file_using.rs`,
//! and for the same reason: these drive the real [`BatchIndex`] over an in-memory
//! filesystem rather than a hand-written stub, so what they pin is the resolution
//! every client gets.
//!
//! Four properties recur:
//!
//! 1. **With no index attached nothing changes.** Every scenario is run twice and
//!    the no-index run must produce today's answer, which for a `RUN` target is
//!    *no reference entry at all*. That is the R11 firewall.
//! 2. **The local scope tree wins, and the index is not even asked.** An internal
//!    `PROCEDURE` in the file under analysis is resolved without a single index
//!    query — asserted with an index that panics if consulted, not by inspecting
//!    a counter.
//! 3. **A wrong link is poison.** One name matching two path entries declines
//!    rather than taking the first match.
//! 4. **No new finding, ever.** Where a test says "produces no finding" it also
//!    asserts a sibling shape where the finding genuinely does fire, so the
//!    assertion cannot pass by the rule being inert.
//!
//! # The `shared_producer` precondition, stated once
//!
//! A `SHARED` variable name maps onto no file — there is nothing to derive a
//! candidate path from — and the `FileSystem` trait exposes no directory listing,
//! so `BatchIndex::shared_producer` answers from the files the run has **already
//! indexed**. In these tests that means a consumer links to its producer only
//! when something in the same file pulled the producer in, in practice a `RUN` of
//! it. Every scenario below is written that way on purpose; the link is *not*
//! unconditional today, and seeding the batch with the run's file set is separate,
//! already-tracked work.

use std::path::PathBuf;

use oxabl_ast::{RunTarget, Statement, StatementKind};
use oxabl_common::{Diagnostic, FileId};
use oxabl_index::BatchIndex;
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0004, type_mismatch_assignment, undefined_symbol};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{
    AnalysisContext, ClassDescriptor, IndexAnswer, IndexName, IndexRevision, IndexedFileId,
    MemberDescriptor, PrimitiveTy, Resolution, ResolvedType, Semantic, SymbolFlags, SymbolId,
    SymbolKind, UnresolvedReason, WorkspaceIndex, analyze_file,
};
use oxabl_workspace::InMemoryFileSystem;

// ---------------------------------------------------------------------------
// Fixtures — synthetic ABL only
// ---------------------------------------------------------------------------

/// The program every literal-`RUN` scenario targets. Takes exactly one INPUT
/// parameter, which is what makes the deferred-signature boundary assertable.
const POST_ORDER: &str = "DEFINE INPUT PARAMETER p-order-id AS INTEGER NO-UNDO.\n\
                          MESSAGE p-order-id.\n";

/// A producer of a plain `NEW SHARED` name.
const INIT_GLOBALS: &str = "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n";

/// A producer of a `NEW GLOBAL SHARED` name — a different declaration form, the
/// same producer fact.
const INIT_REGION: &str = "DEFINE NEW GLOBAL SHARED VARIABLE v-region-code AS CHARACTER NO-UNDO.\n";

/// The workspace as most scenarios see it, rooted at `/src`.
const WORKSPACE: [(&str, &str); 4] = [
    ("/src/post-order.p", POST_ORDER),
    ("/src/orders/recalc-total.p", POST_ORDER),
    ("/src/init-globals.p", INIT_GLOBALS),
    ("/src/init-region.p", INIT_REGION),
];

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

fn parse(source: &str) -> Vec<Statement> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}",
        program.errors
    );
    program.statements
}

/// Analyze `source` against a batch index over `workspace`, searching `paths`.
fn with_paths(
    source: &str,
    workspace: &[(&str, &str)],
    paths: &[&str],
) -> (Vec<Statement>, Semantic) {
    let mut fs = InMemoryFileSystem::new();
    for (path, contents) in workspace {
        fs.insert(PathBuf::from(path), *contents);
    }
    let dirs: Vec<PathBuf> = paths.iter().map(PathBuf::from).collect();
    let index = BatchIndex::new(&fs, &dirs);
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    let sem = analyze_file(&stmts, &ctx);
    (stmts, sem)
}

/// The common case: one path entry, `/src`.
fn with_index(source: &str, workspace: &[(&str, &str)]) -> (Vec<Statement>, Semantic) {
    with_paths(source, workspace, &["/src"])
}

/// Analyze `source` the way every client does today: no index at all.
fn without_index(source: &str) -> (Vec<Statement>, Semantic) {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    (stmts, sem)
}

/// `undefined-symbol` findings for `source`, with an index attached.
fn lint0001_with_index(source: &str, workspace: &[(&str, &str)]) -> Vec<Diagnostic> {
    let mut fs = InMemoryFileSystem::new();
    for (path, contents) in workspace {
        fs.insert(PathBuf::from(path), *contents);
    }
    let dirs = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &dirs);
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    let sem = analyze_file(&stmts, &ctx);
    undefined_symbol::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0001)
        .collect()
}

/// `undefined-symbol` findings for `source`, with no index at all.
fn lint0001_without_index(source: &str) -> Vec<Diagnostic> {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    undefined_symbol::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0001)
        .collect()
}

/// `type-mismatch-assignment` findings for `source`, with an index attached.
fn lint0004_with_index(source: &str, workspace: &[(&str, &str)]) -> Vec<Diagnostic> {
    let mut fs = InMemoryFileSystem::new();
    for (path, contents) in workspace {
        fs.insert(PathBuf::from(path), *contents);
    }
    let dirs = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &dirs);
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    let sem = analyze_file(&stmts, &ctx);
    type_mismatch_assignment::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0004)
        .collect()
}

/// `type-mismatch-assignment` findings for `source`, with no index at all.
fn lint0004_without_index(source: &str) -> Vec<Diagnostic> {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    type_mismatch_assignment::run(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0004)
        .collect()
}

/// The resolution recorded for the file's one `RUN` statement.
///
/// The two arms are keyed differently on purpose and the test suite must not
/// paper over which: a literal target has its own node id (that is what U2's AST
/// promotion bought), while a dynamic target has none — the expression inside it
/// already owns its id and carries its *own* resolution — so the statement's id
/// is the anchor there.
fn run_resolution(stmts: &[Statement], sem: &Semantic) -> Option<Resolution> {
    let mut found = None;
    for stmt in stmts {
        let StatementKind::Run { target, .. } = &stmt.kind else {
            continue;
        };
        let key = match target {
            RunTarget::Literal { id, .. } => *id,
            RunTarget::Dynamic(_) => stmt.id,
        };
        assert!(found.is_none(), "fixtures carry exactly one RUN statement");
        found = Some(sem.references.get(key).cloned());
    }
    found.expect("fixture must contain a RUN statement")
}

/// The symbol a name is declared as, insisting on exactly one.
fn sole_symbol(sem: &Semantic, name: &str) -> SymbolId {
    let atom = OxablAtom::from(name);
    let hits: Vec<SymbolId> = sem
        .symbols
        .iter()
        .filter(|(_, s)| s.name == atom)
        .map(|(id, _)| id)
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "expected one symbol named `{name}`: {hits:?}"
    );
    hits[0]
}

/// An index that reports a real revision and panics on the one query a locally
/// declared internal procedure must never trigger.
///
/// A counter would prove the same thing only if the test remembered to read it;
/// a panic makes "the index is not consulted" unfalsifiable-by-omission.
struct RunHostileIndex;

impl WorkspaceIndex for RunHostileIndex {
    fn class(&self, _name: &IndexName) -> IndexAnswer<std::sync::Arc<ClassDescriptor>> {
        IndexAnswer::NotFound
    }

    fn class_members(&self, _class: &IndexName) -> IndexAnswer<std::sync::Arc<[MemberDescriptor]>> {
        IndexAnswer::NotFound
    }

    fn program(&self, target: &IndexName) -> IndexAnswer<IndexedFileId> {
        panic!(
            "the index must not be asked about `{}`",
            target.as_written()
        );
    }

    fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }

    fn revision(&self) -> IndexRevision {
        IndexRevision::new(11)
    }
}

// ---------------------------------------------------------------------------
// R5 — the dynamic arm
// ---------------------------------------------------------------------------

#[test]
fn run_value_records_unknowable_and_produces_no_undefined_symbol_finding() {
    // AE2. `RUN VALUE(<expr>)` is the canonical statically-undecidable target:
    // no amount of indexing resolves it, so it is `Unknowable` rather than
    // `NotFoundInWorkspace`, and no rule may report it.
    let source = "DEFINE VARIABLE c-name AS CHARACTER NO-UNDO.\n\
                  c-name = \"post-order.p\".\n\
                  RUN VALUE(c-name).\n";
    let (stmts, sem) = with_index(source, &WORKSPACE);

    assert_eq!(
        run_resolution(&stmts, &sem),
        Some(Resolution::Unresolved {
            name: OxablAtom::from(""),
            reason: UnresolvedReason::Unknowable,
        }),
        "a computed target names no identifier, so the atom is empty and the \
         reason is the one no rule reports"
    );
    assert!(
        lint0001_with_index(source, &WORKSPACE).is_empty(),
        "an unknowable target is not an undefined symbol"
    );

    // Not inert: the same statement shape over an *undeclared* variable does
    // fire, so the emptiness above is the reason's doing rather than the rule
    // being asleep on `RUN` statements.
    let baseline = "RUN VALUE(c-never-declared).\n";
    assert_eq!(
        lint0001_with_index(baseline, &WORKSPACE).len(),
        1,
        "the expression inside VALUE() is still resolved normally"
    );
}

#[test]
fn the_dynamic_arm_records_nothing_without_an_index() {
    // Unknowability does not depend on an index — but *recording* it does. With
    // no index attached nothing was looked at, so a run that asked no cross-file
    // question must not grow its `references` table, which is what keeps a
    // single-file analysis byte-for-byte what it is today.
    let source = "DEFINE VARIABLE c-name AS CHARACTER NO-UNDO.\nRUN VALUE(c-name).\n";
    let (stmts, sem) = without_index(source);
    assert_eq!(run_resolution(&stmts, &sem), None);
}

// ---------------------------------------------------------------------------
// R3 — literal targets
// ---------------------------------------------------------------------------

#[test]
fn a_literal_target_with_exactly_one_matching_file_resolves() {
    let source = "RUN post-order.p (INPUT 1).\n";
    let (stmts, sem) = with_index(source, &WORKSPACE);

    let Some(Resolution::Resolved(sym)) = run_resolution(&stmts, &sem) else {
        panic!("exactly one file on the paths matches");
    };
    let symbol = sem.symbols.get(sym);
    assert_eq!(symbol.kind, SymbolKind::Procedure);
    assert_eq!(symbol.name, OxablAtom::from("post-order.p"));
    assert_eq!(
        symbol.declaration,
        oxabl_ast::NodeId::DUMMY,
        "the target is declared in another file, not by a node in this one"
    );
    // The link's payload: which indexed file supplies the program. This is what
    // the analyze envelope reads out.
    let file = sem
        .symbols
        .program_file(sym)
        .expect("a resolved target records the file that supplies it");
    assert_ne!(file.raw(), 0, "index file ids are minted from 1");

    // Reachable only through the reference entry, never by name: nothing may
    // stumble onto a workspace program through ordinary scope resolution.
    assert!(
        sem.scope_tree
            .resolve(
                oxabl_semantic::ScopeId::ROOT,
                oxabl_semantic::NamespaceId::Procedures,
                &OxablAtom::from("post-order.p"),
            )
            .is_none(),
        "a synthesized program symbol stays out of the scope tree"
    );
}

#[test]
fn a_quoted_relative_literal_target_resolves() {
    // `RUN "sub/thing.p"` — the quoted form, whose name the lexer hands over
    // without the quotes, and whose path is relative to a search-path entry.
    let source = "RUN \"orders/recalc-total.p\" (INPUT 1).\n";
    let (stmts, sem) = with_index(source, &WORKSPACE);

    let Some(Resolution::Resolved(sym)) = run_resolution(&stmts, &sem) else {
        panic!("the quoted relative literal has exactly one match");
    };
    assert_eq!(
        sem.symbols.get(sym).name,
        OxablAtom::from("orders/recalc-total.p")
    );
    assert!(sem.symbols.program_file(sym).is_some());
}

#[test]
fn one_name_on_two_path_entries_is_unknowable_not_the_first_match() {
    // The wrong-link-is-poison case. Both entries carry `post-order.p`; the
    // workspace genuinely cannot say which is meant, so the link is declined
    // rather than guessed. Taking the first match would mis-attribute every
    // symbol the target declares.
    let workspace = [
        ("/first/post-order.p", POST_ORDER),
        ("/second/post-order.p", POST_ORDER),
    ];
    let source = "RUN post-order.p (INPUT 1).\n";
    let (stmts, sem) = with_paths(source, &workspace, &["/first", "/second"]);

    assert_eq!(
        run_resolution(&stmts, &sem),
        Some(Resolution::Unresolved {
            name: OxablAtom::from("post-order.p"),
            reason: UnresolvedReason::Unknowable,
        })
    );
    // And the one-entry search over the same tree does resolve, so the decline
    // above is the ambiguity's doing and not a broken search.
    let (stmts, sem) = with_paths(source, &workspace, &["/first"]);
    assert!(matches!(
        run_resolution(&stmts, &sem),
        Some(Resolution::Resolved(_))
    ));
}

#[test]
fn a_name_no_file_on_the_paths_matches_is_not_found_in_the_workspace() {
    let source = "RUN never-shipped.p.\n";
    let (stmts, sem) = with_index(source, &WORKSPACE);

    assert_eq!(
        run_resolution(&stmts, &sem),
        Some(Resolution::Unresolved {
            name: OxablAtom::from("never-shipped.p"),
            reason: UnresolvedReason::NotFoundInWorkspace,
        }),
        "an index was attached and it looked: that is a fact about the \
         workspace, not a missing capability"
    );
    assert!(
        lint0001_with_index(source, &WORKSPACE).is_empty(),
        "the new reason stays in the suppressed position `External` occupies"
    );
}

#[test]
fn an_internal_procedure_resolves_locally_and_the_index_is_not_consulted() {
    // The local answer wins, and it wins *before* the index is asked — a
    // same-named program elsewhere on the paths must not be able to hijack an
    // internal call. Asserted with an index that panics if queried.
    let source = "PROCEDURE post-order:\n\
                  MESSAGE \"local\".\n\
                  END PROCEDURE.\n\
                  RUN post-order.\n";
    let schema = Schema::empty();
    let index = RunHostileIndex;
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    let sem = analyze_file(&stmts, &ctx);

    let Some(Resolution::Resolved(sym)) = run_resolution(&stmts, &sem) else {
        panic!("an internal procedure declared in this file resolves locally");
    };
    let symbol = sem.symbols.get(sym);
    assert_eq!(symbol.kind, SymbolKind::Procedure);
    assert_ne!(
        symbol.declaration,
        oxabl_ast::NodeId::DUMMY,
        "the local declaration is a real node in this file"
    );
    assert!(
        sem.symbols.program_file(sym).is_none(),
        "a local procedure is supplied by no indexed file"
    );
}

#[test]
fn a_literal_target_records_nothing_without_an_index() {
    for source in [
        "RUN post-order.p (INPUT 1).\n",
        "RUN \"orders/recalc-total.p\" (INPUT 1).\n",
        "RUN never-shipped.p.\n",
        "PROCEDURE post-order:\nMESSAGE \"local\".\nEND PROCEDURE.\nRUN post-order.\n",
    ] {
        let (stmts, sem) = without_index(source);
        assert_eq!(
            run_resolution(&stmts, &sem),
            None,
            "no index means no cross-file question was asked: {source}"
        );
    }
}

// ---------------------------------------------------------------------------
// R4 — the SHARED producer link
// ---------------------------------------------------------------------------

/// A consumer of `v-site-code` that pulls its producer in with a `RUN`. See the
/// module docs: the `RUN` is what makes the producer visible at all.
const SHARED_CONSUMER: &str = "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n\
                               RUN init-globals.p.\n\
                               MESSAGE v-site-code.\n";

#[test]
fn a_shared_consumer_links_to_the_new_shared_producer_the_run_pulled_in() {
    let (_, sem) = with_index(SHARED_CONSUMER, &WORKSPACE);
    let consumer = sole_symbol(&sem, "v-site-code");
    assert!(
        sem.symbols
            .get(consumer)
            .flags
            .contains(SymbolFlags::SHARED),
        "the fixture's consumer is a plain DEFINE SHARED"
    );
    let producer = sem
        .symbols
        .shared_producer(consumer)
        .expect("the RUN pulled the producing file in, so the link resolves");

    // The link names the same indexed file the `RUN` resolved to — one file, one
    // id, which is what makes the two facts composable downstream.
    let run_sym = match run_resolution(&parse(SHARED_CONSUMER), &sem) {
        Some(Resolution::Resolved(sym)) => sym,
        other => panic!("the RUN target resolves: {other:?}"),
    };
    assert_eq!(
        Some(producer),
        sem.symbols.program_file(run_sym),
        "the producer is the file the RUN linked to"
    );
}

#[test]
fn a_new_global_shared_producer_links_the_same_way() {
    let source = "DEFINE SHARED VARIABLE v-region-code AS CHARACTER NO-UNDO.\n\
                  RUN init-region.p.\n\
                  MESSAGE v-region-code.\n";
    let (_, sem) = with_index(source, &WORKSPACE);
    let consumer = sole_symbol(&sem, "v-region-code");
    assert!(
        sem.symbols.shared_producer(consumer).is_some(),
        "NEW GLOBAL SHARED is a producer form too"
    );
}

#[test]
fn a_consumer_whose_type_differs_from_the_producer_keeps_its_own_type() {
    // KTD10's second clause: the link records *which file*, and nothing more.
    // The producer declares CHARACTER and the consumer INTEGER; retyping from
    // the producer, or diagnosing the disagreement, is deferred follow-up work,
    // so the consumer's own declaration stays the type of record and the
    // integer assignment below is silent.
    let source = "DEFINE SHARED VARIABLE v-site-code AS INTEGER NO-UNDO.\n\
                  RUN init-globals.p.\n\
                  v-site-code = 5.\n";
    let (_, sem) = with_index(source, &WORKSPACE);
    let consumer = sole_symbol(&sem, "v-site-code");
    assert!(
        sem.symbols.shared_producer(consumer).is_some(),
        "the link is recorded even though the two declarations disagree"
    );
    assert_eq!(
        sem.symbols.get(consumer).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer)),
        "the consumer's own declaration is the type of record"
    );
    assert!(
        lint0004_with_index(source, &WORKSPACE).is_empty(),
        "assigning an integer to the consumer's INTEGER declaration is correct"
    );

    // Not inert: the rule does fire on this declaration when the assignment
    // genuinely mismatches, with and without an index. If retyping from the
    // producer ever leaked in, the *first* assertion above would break — and
    // this one proves the rule is watching this exact declaration.
    let mismatch = "DEFINE SHARED VARIABLE v-site-code AS INTEGER NO-UNDO.\n\
                    RUN init-globals.p.\n\
                    v-site-code = TRUE.\n";
    assert_eq!(lint0004_without_index(mismatch).len(), 1);
    assert_eq!(lint0004_with_index(mismatch, &WORKSPACE).len(), 1);
}

#[test]
fn a_shared_consumer_with_no_producer_anywhere_links_to_nothing() {
    let source = "DEFINE SHARED VARIABLE v-orphan-code AS CHARACTER NO-UNDO.\n\
                  MESSAGE v-orphan-code.\n";
    let (_, sem) = with_index(source, &WORKSPACE);
    let consumer = sole_symbol(&sem, "v-orphan-code");
    assert!(sem.symbols.shared_producer(consumer).is_none());
    assert!(
        lint0001_with_index(source, &WORKSPACE).is_empty(),
        "an unlinked consumer is still a perfectly good local declaration"
    );
    assert!(
        sem.diagnostics.is_empty(),
        "and the semantic passes say nothing about it: {:?}",
        sem.diagnostics
    );
}

#[test]
fn a_producer_is_never_linked_as_its_own_consumer() {
    // A file that declares `DEFINE NEW SHARED` is a producer. Linking it to
    // itself would invent a cross-file relationship out of one declaration.
    let source = "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n\
                  RUN init-globals.p.\n";
    let (_, sem) = with_index(source, &WORKSPACE);
    let producer = sole_symbol(&sem, "v-site-code");
    assert!(sem.symbols.shared_producer(producer).is_none());
}

#[test]
fn the_shared_link_is_absent_without_an_index() {
    for source in [
        SHARED_CONSUMER,
        "DEFINE SHARED VARIABLE v-x AS INTEGER NO-UNDO.\n",
    ] {
        let (_, sem) = without_index(source);
        for (sid, _) in sem.symbols.iter() {
            assert!(
                sem.symbols.shared_producer(sid).is_none(),
                "no index means no producer question was asked: {source}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// The deferred boundaries, asserted so a later change cannot cross them quietly
// ---------------------------------------------------------------------------

#[test]
fn a_resolved_target_called_with_the_wrong_argument_count_produces_no_finding() {
    // The callee takes one INPUT parameter and the call site passes three. The
    // link is the deliverable; checking a signature against it is rule work and
    // would break R11, so this must stay silent — asserted here so adding the
    // check later is a failing test rather than a surprise in a dogfood run.
    let source = "RUN post-order.p (INPUT 1, INPUT 2, INPUT 3).\n";
    let (stmts, sem) = with_index(source, &WORKSPACE);
    assert!(
        matches!(run_resolution(&stmts, &sem), Some(Resolution::Resolved(_))),
        "the target resolves, which is what makes the silence meaningful"
    );
    assert!(lint0001_with_index(source, &WORKSPACE).is_empty());
    assert!(lint0004_with_index(source, &WORKSPACE).is_empty());
    assert!(
        sem.diagnostics.is_empty(),
        "no semantic diagnostic either: {:?}",
        sem.diagnostics
    );

    // Not inert: the same call site with an undefined argument does fire, so the
    // rule is live over `RUN` arguments and the silence above is about arity.
    let live = "RUN post-order.p (INPUT v-never-declared).\n";
    assert_eq!(lint0001_with_index(live, &WORKSPACE).len(), 1);
}

#[test]
fn attaching_an_index_adds_no_diagnostic_to_any_scenario() {
    // The R11 firewall, swept over every fixture in this file at once: the
    // diagnostic set is identical with and without an index. A new finding
    // arriving from cross-file resolution shows up here first.
    for source in [
        "DEFINE VARIABLE c-name AS CHARACTER NO-UNDO.\nRUN VALUE(c-name).\n",
        "RUN post-order.p (INPUT 1).\n",
        "RUN \"orders/recalc-total.p\" (INPUT 1).\n",
        "RUN never-shipped.p.\n",
        "RUN post-order.p (INPUT 1, INPUT 2, INPUT 3).\n",
        "PROCEDURE post-order:\nMESSAGE \"local\".\nEND PROCEDURE.\nRUN post-order.\n",
        SHARED_CONSUMER,
    ] {
        let (_, with) = with_index(source, &WORKSPACE);
        let (_, without) = without_index(source);
        assert_eq!(
            with.diagnostics.len(),
            without.diagnostics.len(),
            "semantic diagnostics differ for: {source}"
        );
        assert_eq!(
            lint0001_with_index(source, &WORKSPACE).len(),
            lint0001_without_index(source).len(),
            "LINT0001 differs for: {source}"
        );
        assert_eq!(
            lint0004_with_index(source, &WORKSPACE).len(),
            lint0004_without_index(source).len(),
            "LINT0004 differs for: {source}"
        );
    }
}
