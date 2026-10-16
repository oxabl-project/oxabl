//! CL003 `signature-mismatch` and CL004 `unresolved-call`.
//!
//! The rules read the cross-file index, so the harness drives the real
//! [`BatchIndex`] over an in-memory filesystem rooted at `/src`. Each rule
//! test also asserts a sibling shape that does fire, so a pass cannot come
//! from an inert rule.

mod cl003_signature_mismatch;
mod cl004_unresolved_call;
mod doc_examples;
mod registry_run;

use std::path::PathBuf;

use oxabl_common::{Diagnostic, FileId};
use oxabl_index::BatchIndex;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, SourceContext, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

type RuleFn =
    fn(&[oxabl_ast::Statement], &oxabl_semantic::Semantic, &AnalysisContext) -> Vec<Diagnostic>;

/// How the harness builds the context.
#[derive(Default)]
pub struct Setup<'a> {
    /// Files of the workspace, as `(path, text)`.
    pub files: &'a [(&'a str, &'a str)],
    /// Search paths; `None` is `/src`.
    pub paths: Option<&'a [&'a str]>,
    /// No index at all.
    pub no_index: bool,
    /// The index holds no list of the workspace's files.
    pub unseeded: bool,
    pub fragment: bool,
    /// Offsets the preprocessor left a gap at.
    pub elided_includes: &'a [u32],
    /// The parts of the source that include files wrote.
    pub include_regions: &'a [oxabl_ast::Span],
}

pub fn run_with(rule: RuleFn, source: &str, setup: &Setup) -> Vec<Diagnostic> {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in setup.files {
        fs.insert(PathBuf::from(path), *text);
    }
    let paths: Vec<PathBuf> = match setup.paths {
        Some(paths) => paths.iter().map(PathBuf::from).collect(),
        None => vec![PathBuf::from("/src")],
    };
    let known: Vec<PathBuf> = setup.files.iter().map(|(p, _)| PathBuf::from(p)).collect();
    let mut index = BatchIndex::new(&fs, &paths);
    if !setup.unseeded {
        index = index.seeded_with(&known);
    }
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "parse errors: {:?}\n{source}",
        program.errors
    );
    let schema = Schema::empty();
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema)
        .with_tokens(&tokens)
        .with_elided_includes(setup.elided_includes)
        .with_include_regions(setup.include_regions)
        .with_source_context(if setup.fragment {
            SourceContext::IncludeFragment
        } else {
            SourceContext::CompilationUnit
        });
    if !setup.no_index {
        ctx = ctx.with_index(&index);
    }
    let sem = analyze_file(&program.statements, &ctx);
    rule(&program.statements, &sem, &ctx)
}

/// The 1-based line each diagnostic is anchored on.
pub fn lines(source: &str, diags: &[Diagnostic]) -> Vec<usize> {
    diags
        .iter()
        .map(|d| {
            source[..d.span.span.start as usize]
                .bytes()
                .filter(|&b| b == b'\n')
                .count()
                + 1
        })
        .collect()
}

pub fn messages(diags: &[Diagnostic]) -> Vec<&str> {
    diags.iter().map(|d| d.message.as_str()).collect()
}
