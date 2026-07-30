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
use oxabl_common::{Diagnostic, FileId};
use oxabl_index::BatchIndex;
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0004, type_mismatch_assignment, undefined_symbol};
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
