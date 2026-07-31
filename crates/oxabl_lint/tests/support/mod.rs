//! Shared harness for the `cross_file_*` integration tests.
//!
//! The three cross-file test binaries — inheritance, `USING`, and
//! `RUN`/`SHARED` — all need the same thing: analyze one source string against a
//! real [`BatchIndex`] over an in-memory filesystem, analyze it again with no
//! index at all, and then ask narrow questions of the resulting [`Semantic`].
//! Keeping that in one place is what makes "the no-index run must produce
//! today's answer" a single definition rather than three copies that can drift.
//!
//! Every helper here takes the workspace as an argument rather than hard-coding
//! one, so each test file supplies its own fixtures.
//
// Each integration-test binary compiles its own copy of this module and uses
// only the subset it needs, so unused helpers would otherwise fail the
// `-D warnings` gate.
#![allow(dead_code)]

use std::path::PathBuf;

use oxabl_ast::{NodeId, Statement};
use oxabl_common::{Diagnostic, FileId, FileSpan};
use oxabl_index::BatchIndex;
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0004, lint_file, type_mismatch_assignment, undefined_symbol};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{
    AnalysisContext, NamespaceId, Resolution, ScopeId, Semantic, SymbolId, UnresolvedReason,
    analyze_file,
};
use oxabl_workspace::InMemoryFileSystem;

// ---------------------------------------------------------------------------
// Analyzing a source string
// ---------------------------------------------------------------------------

pub fn parse(source: &str) -> Vec<Statement> {
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
pub fn with_paths(
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
pub fn with_index(source: &str, workspace: &[(&str, &str)]) -> (Vec<Statement>, Semantic) {
    with_paths(source, workspace, &["/src"])
}

/// Analyze `source` the way every client does today: no index at all.
pub fn without_index(source: &str) -> (Vec<Statement>, Semantic) {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    (stmts, sem)
}

// ---------------------------------------------------------------------------
// Running one rule
// ---------------------------------------------------------------------------

/// A single lint rule's entry point.
type Rule = fn(&[Statement], &Semantic, &AnalysisContext) -> Vec<Diagnostic>;

/// Findings under `code` that `rule` produces for `source`, with a batch index
/// over `workspace` rooted at `/src`.
fn findings_with_index(
    source: &str,
    workspace: &[(&str, &str)],
    code: &str,
    rule: Rule,
) -> Vec<Diagnostic> {
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
    rule(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == code)
        .collect()
}

/// Findings under `code` that `rule` produces for `source`, with no index.
fn findings_without_index(source: &str, code: &str, rule: Rule) -> Vec<Diagnostic> {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    rule(&stmts, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == code)
        .collect()
}

/// `undefined-symbol` findings for `source`, with an index attached.
pub fn lint0001_with_index(source: &str, workspace: &[(&str, &str)]) -> Vec<Diagnostic> {
    findings_with_index(source, workspace, LINT0001, undefined_symbol::run)
}

/// `undefined-symbol` findings for `source`, with no index at all.
pub fn lint0001_without_index(source: &str) -> Vec<Diagnostic> {
    findings_without_index(source, LINT0001, undefined_symbol::run)
}

/// `type-mismatch-assignment` findings for `source`, with an index attached.
pub fn lint0004_with_index(source: &str, workspace: &[(&str, &str)]) -> Vec<Diagnostic> {
    findings_with_index(source, workspace, LINT0004, type_mismatch_assignment::run)
}

/// `type-mismatch-assignment` findings for `source`, with no index at all.
pub fn lint0004_without_index(source: &str) -> Vec<Diagnostic> {
    findings_without_index(source, LINT0004, type_mismatch_assignment::run)
}

// ---------------------------------------------------------------------------
// Sweeping every rule at once
// ---------------------------------------------------------------------------

/// One diagnostic reduced to the parts a firewall sweep compares: its code, its
/// severity, and its **byte** span.
///
/// Byte spans, not rendered positions, for the reason the cross-client parity
/// suite gives: an encoding conversion is not a pipeline difference, and
/// comparing the raw span keeps the sweep from mistaking one for the other. The
/// message is deliberately excluded — it interpolates a display name, and a
/// wording change is not a behavior change.
pub type DiagnosticShape = (&'static str, oxabl_common::Severity, FileSpan);

fn shapes(diags: Vec<Diagnostic>) -> Vec<DiagnosticShape> {
    diags
        .into_iter()
        .map(|d| (d.code.0, d.severity, d.span))
        .collect()
}

/// Every finding all **six** rules produce for `source`, with a batch index over
/// `workspace` rooted at `/src`.
///
/// Goes through [`oxabl_lint::lint_file`] rather than calling the six rule
/// functions here, so a seventh rule is swept the moment it is added rather than
/// silently skipped — the firewall has to hold for whatever rules exist, not for
/// the six that existed when the sweep was written.
pub fn all_lints_with_index(source: &str, workspace: &[(&str, &str)]) -> Vec<DiagnosticShape> {
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
    shapes(lint_file(&stmts, &sem, &ctx))
}

/// Every finding all six rules produce for `source`, with no index at all —
/// today's answer, and the one the index-attached run must match.
pub fn all_lints_without_index(source: &str) -> Vec<DiagnosticShape> {
    let schema = Schema::empty();
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&stmts, &ctx);
    shapes(lint_file(&stmts, &sem, &ctx))
}

/// Assert that attaching an index **adds** no diagnostic to `source` — from any
/// of the six lint rules, or from the semantic pass.
///
/// The whole phase reduces to this one property, so it is one helper rather than
/// a per-file copy: a new finding arriving from cross-file resolution shows up
/// here first, in whichever fixture file introduced the shape that produced it.
///
/// **Added, not "identical", and the asymmetry is the point.** Removing a finding
/// is what cross-file resolution is *for*: `calc-total()` called inside a
/// subclass is `NotInScope` with no index — a `LINT0001` false positive — and
/// resolving it against the parent file is the fix. Every such removal is a name
/// the index positively accounted for. An *addition* is the opposite: a verdict
/// reached from a type or a resolution the single-file run did not have, on code
/// that was silent before, which is exactly the drift this phase must not
/// produce and which the follow-up unit owns deciding about deliberately.
///
/// So the check is a multiset containment: every diagnostic the index-attached
/// run produces must already be produced without one. Multiset, not set — two
/// findings of the same code at the same span collapsing into one would itself be
/// a behavior change worth catching.
pub fn assert_index_adds_no_diagnostic(source: &str, workspace: &[(&str, &str)]) {
    let (_, with) = with_index(source, workspace);
    let (_, without) = without_index(source);
    assert_eq!(
        with.diagnostics, without.diagnostics,
        "semantic diagnostics differ for:\n{source}"
    );

    let with_lints = all_lints_with_index(source, workspace);
    let mut available = all_lints_without_index(source);
    for finding in &with_lints {
        match available.iter().position(|f| f == finding) {
            Some(i) => {
                available.swap_remove(i);
            }
            None => panic!(
                "attaching an index ADDED a finding that the single-file run does not \
                 produce: {finding:?}\nwith an index:    {with_lints:?}\nwithout one:      \
                 {:?}\nfor:\n{source}",
                all_lints_without_index(source)
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Asking questions of the model
// ---------------------------------------------------------------------------

/// Every distinct symbol some reference resolved to under the name `name`.
pub fn resolved_to(sem: &Semantic, name: &str) -> Vec<SymbolId> {
    let atom = OxablAtom::from(name);
    let mut hits: Vec<SymbolId> = sem
        .references
        .iter()
        .filter_map(|(_, res)| match res {
            Resolution::Resolved(sym) if sem.symbols.get(*sym).name == atom => Some(*sym),
            Resolution::Resolved(_) | Resolution::Unresolved { .. } => None,
        })
        .collect();
    hits.sort_by_key(|s| s.raw());
    hits.dedup();
    hits
}

/// The one symbol references under `name` resolved to. Panics unless exactly one
/// symbol answers, so a test can never accidentally assert about two.
pub fn sole_resolved(sem: &Semantic, name: &str) -> SymbolId {
    let hits = resolved_to(sem, name);
    assert_eq!(
        hits.len(),
        1,
        "expected exactly one resolved symbol named `{name}`, got {hits:?}"
    );
    hits[0]
}

/// Reasons carried by every unresolved reference under `name`.
pub fn unresolved_reasons(sem: &Semantic, name: &str) -> Vec<UnresolvedReason> {
    let atom = OxablAtom::from(name);
    sem.references
        .iter()
        .filter_map(|(_, res)| match res {
            Resolution::Unresolved { name, reason } if *name == atom => Some(*reason),
            Resolution::Unresolved { .. } | Resolution::Resolved(_) => None,
        })
        .collect()
}

/// Symbols in the table carrying `name`, whether declared or synthesized.
pub fn symbols_named(sem: &Semantic, name: &str) -> Vec<SymbolId> {
    let atom = OxablAtom::from(name);
    sem.symbols
        .iter()
        .filter(|(_, s)| s.name == atom)
        .map(|(id, _)| id)
        .collect()
}

/// The symbol a name is declared as, insisting on exactly one.
pub fn sole_symbol(sem: &Semantic, name: &str) -> SymbolId {
    let hits = symbols_named(sem, name);
    assert_eq!(
        hits.len(),
        1,
        "expected one symbol named `{name}`: {hits:?}"
    );
    hits[0]
}

/// Assert `sym` follows the synthesized-cross-file-symbol conventions: no
/// declaration node, the root scope, and absent from every scope tree binding.
pub fn assert_synthesized(sem: &Semantic, sym: SymbolId, ns: NamespaceId) {
    let symbol = sem.symbols.get(sym);
    assert_eq!(
        symbol.declaration,
        NodeId::DUMMY,
        "a symbol synthesized from the index declares no node in this file"
    );
    assert_eq!(symbol.declared_in, ScopeId::ROOT);
    assert_eq!(symbol.namespace, ns);
    for (scope, _) in sem.scope_tree.iter() {
        assert_ne!(
            sem.scope_tree.get(scope).get_in(ns, &symbol.name),
            Some(sym),
            "a synthesized symbol must never be bound in the scope tree"
        );
    }
}
