//! Wave 1 public-API surface tests.
//!
//! Every item below is reached through **`oxabl::…` only** — no `oxabl_*`
//! sub-crate appears in these imports. That is the standing regression guard
//! for issue #55's Critical item: a consumer needs a single `oxabl` dependency.
//! Later waves extend this file with `render_diagnostics`, `analyze`, and
//! `format_source`.

// A parse error is unreachable from the top level *by construction*: it lives
// under `oxabl::parser`. This line documents that internal recovery helpers are
// NOT part of the surface (U3) — uncommenting it must not compile:
//   let _ = oxabl::parser::Parser::skip_to_period; // pub(crate), unreachable

#[test]
fn parse_happy_path_returns_recovered_program() {
    let program = oxabl::parse("MESSAGE \"x\".");
    assert!(program.is_ok());
    assert!(program.errors.is_empty());
    assert_eq!(program.statements.len(), 1);
    assert!(program.first_error().is_none());
}

#[test]
fn parse_recovers_all_errors_not_just_first() {
    // Two independent broken statements; error recovery must collect both,
    // proving `parse` uses `parse_program` semantics rather than fail-fast.
    let program = oxabl::parse("DEFINE VARIABLE .\nDEFINE VARIABLE .");
    assert!(!program.is_ok());
    assert!(
        program.errors.len() >= 2,
        "expected recovery to collect multiple errors, got {}",
        program.errors.len()
    );
}

#[test]
fn program_into_result_and_first_error() {
    let clean = oxabl::parse("MESSAGE \"x\".");
    assert!(clean.first_error().is_none());
    assert!(clean.into_result().is_ok());

    let broken = oxabl::parse("DEFINE VARIABLE .");
    assert!(broken.first_error().is_some());
    let err = broken.into_result().unwrap_err();
    assert!(!err.is_empty());
}

#[test]
fn program_into_diagnostics_carries_file_id() {
    use oxabl::common::FileId;
    let fid = FileId::new(7);
    let program = oxabl::parse("DEFINE VARIABLE .");
    let n_errors = program.errors.len();
    let diags = program.into_diagnostics(fid);
    assert_eq!(diags.len(), n_errors);
    assert!(diags.iter().all(|d| d.span.file == fid));
}

#[test]
fn parse_error_displays_message_and_is_std_error() {
    let program = oxabl::parse("DEFINE VARIABLE .");
    let err = program.first_error().expect("expected a parse error");
    let shown = format!("{err}");
    assert!(!shown.is_empty());
    assert_eq!(shown, err.message);
    // `ParseError` coerces to `dyn std::error::Error` (U4 / additional item A).
    let _dyn: &dyn std::error::Error = err;
}

/// Every curated module is reachable from a single `oxabl` dependency (U1).
/// Referencing the items — as values, fn-pointers, or constructors — is the
/// compile-gate; no sub-crate import is present anywhere in this test file.
#[test]
fn every_module_is_reachable() {
    use oxabl::ast::{Expression, Statement};
    use oxabl::common::SourceMap;
    use oxabl::lexer::{Token, tokenize};
    use oxabl::parser::{ParseError, Parser};
    use oxabl::preprocessor::Preprocessor;
    use oxabl::schema::Schema;
    use oxabl::semantic::{AnalysisContext, Semantic, analyze_file};
    use oxabl::style::StyleGuide;

    // Concrete, cheap calls where possible.
    let tokens: Vec<Token> = tokenize("MESSAGE \"x\".");
    assert!(!tokens.is_empty());
    let _sm = SourceMap::new("MESSAGE \"x\".");
    let schema = Schema::empty();
    let style = StyleGuide::default_base();

    // parse → semantic → lint → format, all via the facade.
    let program = oxabl::parse("MESSAGE \"x\".");
    let ctx = AnalysisContext::new(oxabl::common::FileId::new(1), "MESSAGE \"x\".", &schema);
    let sem: Semantic = analyze_file(&program.statements, &ctx);
    let _lints = oxabl::lint::lint_file(&program.statements, &sem, &ctx);
    let formatted = oxabl::formatter::format("MESSAGE \"x\".", &program, &style);
    assert!(formatted.is_ok());

    // Remaining items resolve: construct/reference them directly.
    let mut parser = Parser::new(&tokens, "MESSAGE \"x\".");
    assert!(parser.parse_program().is_ok());
    let _pe: fn(ParseError, oxabl::common::FileId) -> oxabl::common::Diagnostic =
        ParseError::into_diagnostic;
    let _stmt: Option<&Statement> = program.statements.first();
    let _expr: fn() -> Option<Expression> = || None;
    let _pp = Preprocessor::new;
    let _cd: Option<oxabl::analyze::CollectedDiagnostics> = None;
    let _ws: Option<oxabl::workspace::RealFileSystem> = None;
}
