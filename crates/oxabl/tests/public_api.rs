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

#[test]
fn render_diagnostics_produces_positioned_snippet() {
    use oxabl::common::{FileId, SourceResolver};
    let source = "MESSAGE \"a\".\nDEFINE VARIABLE .";
    let fid = FileId::new(1);
    let diags = oxabl::parse(source).into_diagnostics(fid);
    assert!(!diags.is_empty());
    let resolver = SourceResolver::new(fid, "t.p", source);
    let rendered = oxabl::render_diagnostics(&diags, &resolver);
    assert!(
        rendered.contains("t.p:2:"),
        "expected a position: {rendered}"
    );
    assert!(
        rendered.contains("DEFINE VARIABLE ."),
        "expected a snippet: {rendered}"
    );
}

#[test]
fn diagnostic_display_one_line_form() {
    use oxabl::common::FileId;
    let diags = oxabl::parse("DEFINE VARIABLE .").into_diagnostics(FileId::new(1));
    let shown = format!("{}", diags[0]);
    // severity[code]: message
    assert!(shown.starts_with("error[PARSE001]: "), "got: {shown}");
}

/// The `serde` feature is on by default for the umbrella, so a consumer can
/// serialize the shared `Diagnostic` directly — no hand-mirrored struct (U7).
#[test]
fn diagnostic_serializes_via_facade() {
    use oxabl::common::FileId;
    let diags = oxabl::parse("DEFINE VARIABLE .").into_diagnostics(FileId::new(1));
    let v = serde_json::to_value(&diags[0]).unwrap();
    assert_eq!(v["code"], "PARSE001");
    assert_eq!(v["severity"], "error");
    assert!(v["message"].is_string());
    assert_eq!(v["span"]["file"], 1);
}

#[test]
fn analyze_default_options_reports_diagnostics() {
    // Unused variable → LINT0002; proves parse → semantic → lint runs end to end
    // through the single-call `analyze` with default options.
    let (sem, collected) = oxabl::analyze(
        "DEFINE VARIABLE unusedVar AS INTEGER NO-UNDO.",
        &oxabl::AnalyzeOptions::default(),
    );
    assert!(sem.is_some());
    let codes: Vec<&str> = collected.all().map(|c| c.diagnostic.code.0).collect();
    assert!(
        codes.contains(&"LINT0002"),
        "expected LINT0002, got {codes:?}"
    );
}

#[test]
fn analyze_flows_schema_through_options() {
    // Load a Customer(CustNum) schema from a temp .df dir via Schema::from_df_dir.
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(
        dir.path().join("s.df"),
        "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\n",
    )
    .unwrap();
    let (schema, sdiags) = oxabl::schema::Schema::from_df_dir(dir.path());
    assert!(sdiags.is_empty(), "schema should load cleanly: {sdiags:?}");

    // A reference to an unknown field on a known table fires LINT0003 — proving
    // the schema flowed through AnalyzeOptions into resolution.
    let source = "FIND FIRST Customer.\nDISPLAY Customer.NoSuchField.\n";
    let opts = oxabl::AnalyzeOptions {
        schema,
        schema_loaded: true,
        ..Default::default()
    };
    let (_sem, collected) = oxabl::analyze(source, &opts);
    let codes: Vec<&str> = collected.all().map(|c| c.diagnostic.code.0).collect();
    assert!(
        codes.contains(&"LINT0003"),
        "expected LINT0003, got {codes:?}"
    );
}

#[test]
fn analyze_with_fs_runs_preprocess_path() {
    use oxabl::workspace::InMemoryFileSystem;
    let fs = InMemoryFileSystem::new();
    let opts = oxabl::AnalyzeOptions {
        preprocess: true,
        ..Default::default()
    };
    // A scoped-define expanded before analysis; proves the fs-injection +
    // preprocess path runs and yields a model.
    let src = "&SCOPED-DEFINE MSG \"hi\"\nMESSAGE {&MSG}.\n";
    let (sem, _collected) = oxabl::analyze_with_fs(src, &fs, &opts);
    assert!(sem.is_some());
}

#[test]
fn format_source_formats_and_is_idempotent() {
    use oxabl::style::StyleGuide;
    let style = StyleGuide::default_base();
    let src = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";
    let once = oxabl::format_source(src, &style).expect("should format");
    let twice = oxabl::format_source(&once, &style).expect("should format");
    assert_eq!(once, twice, "formatter must be idempotent");
}

#[test]
fn format_source_bails_on_parse_errors_without_mangling() {
    use oxabl::formatter::FormatBail;
    use oxabl::style::StyleGuide;
    // A parse-broken buffer must bail (ParseErrors), never emit altered bytes.
    let err = oxabl::format_source("DEFINE VARIABLE .", &StyleGuide::default_base())
        .expect_err("parse-dirty input should bail");
    assert_eq!(err, FormatBail::ParseErrors);
}

#[test]
fn lexer_iterator_matches_tokenize_via_facade() {
    use oxabl::lexer::{Lexer, tokenize};
    let src = "DEFINE VARIABLE x AS INTEGER NO-UNDO.";
    let streamed: Vec<_> = Lexer::new(src).collect();
    assert_eq!(streamed, tokenize(src));
    // Laziness: take fewer than the full stream without lexing the rest.
    assert_eq!(Lexer::new(src).take(2).count(), 2);
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
