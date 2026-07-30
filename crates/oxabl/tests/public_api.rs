//! Wave 1 public-API surface tests.
//!
//! Every item below is reached through **`oxabl::…` only** — no `oxabl_*`
//! sub-crate appears in these imports. That is the standing regression guard
//! for issue #55's Critical item: a consumer needs a single `oxabl` dependency.
//! Later waves extend this file with `render_diagnostics`, `analyze`, and
//! `format_source`.
//!
//! The tests reach for the **fallible** entry points (`try_parse`,
//! `try_analyze`, `try_format_source`), which are the canonical surface; the
//! panicking originals are kept for compatibility and pinned by
//! `deprecated_panicking_surface_still_resolves` alone.

// A parse error is unreachable from the top level *by construction*: it lives
// under `oxabl::parser`. This line documents that internal recovery helpers are
// NOT part of the surface (U3) — uncommenting it must not compile:
//   let _ = oxabl::parser::Parser::skip_to_period; // pub(crate), unreachable

/// `try_parse` is the canonical entry point, so every test below parses through
/// it. `expect` is right here: no input in this repo panics, so a panic would be
/// a genuine regression rather than an expected outcome.
fn parse(source: &str) -> oxabl::Program {
    oxabl::try_parse(source).expect("no internal panic")
}

/// Same for the analyze pair.
fn analyze(
    source: &str,
    options: &oxabl::AnalyzeOptions,
) -> (
    Option<oxabl::semantic::Semantic>,
    oxabl::analyze::CollectedDiagnostics,
) {
    oxabl::try_analyze(source, options).expect("no internal panic")
}

fn analyze_with_fs(
    source: &str,
    fs: &dyn oxabl::workspace::FileSystem,
    options: &oxabl::AnalyzeOptions,
) -> (
    Option<oxabl::semantic::Semantic>,
    oxabl::analyze::CollectedDiagnostics,
) {
    oxabl::try_analyze_with_fs(source, fs, options).expect("no internal panic")
}

#[test]
fn parse_happy_path_returns_recovered_program() {
    let program = parse("MESSAGE \"x\".");
    assert!(program.is_ok());
    assert!(program.errors.is_empty());
    assert_eq!(program.statements.len(), 1);
    assert!(program.first_error().is_none());
}

#[test]
fn parse_recovers_all_errors_not_just_first() {
    // Two independent broken statements; error recovery must collect both,
    // proving `parse` uses `parse_program` semantics rather than fail-fast.
    let program = parse("DEFINE VARIABLE .\nDEFINE VARIABLE .");
    assert!(!program.is_ok());
    assert!(
        program.errors.len() >= 2,
        "expected recovery to collect multiple errors, got {}",
        program.errors.len()
    );
}

#[test]
fn program_into_result_and_first_error() {
    let clean = parse("MESSAGE \"x\".");
    assert!(clean.first_error().is_none());
    assert!(clean.into_result().is_ok());

    let broken = parse("DEFINE VARIABLE .");
    assert!(broken.first_error().is_some());
    let err = broken.into_result().unwrap_err();
    assert!(!err.is_empty());
}

#[test]
fn program_into_diagnostics_carries_file_id() {
    use oxabl::common::FileId;
    let fid = FileId::new(7);
    let program = parse("DEFINE VARIABLE .");
    let n_errors = program.errors.len();
    let diags = program.into_diagnostics(fid);
    assert_eq!(diags.len(), n_errors);
    assert!(diags.iter().all(|d| d.span.file == fid));
}

#[test]
fn parse_error_displays_message_and_is_std_error() {
    let program = parse("DEFINE VARIABLE .");
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
    let diags = parse(source).into_diagnostics(fid);
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
    let diags = parse("DEFINE VARIABLE .").into_diagnostics(FileId::new(1));
    let shown = format!("{}", diags[0]);
    // severity[code]: message
    assert!(shown.starts_with("error[PARSE001]: "), "got: {shown}");
}

/// The `serde` feature is on by default for the umbrella, so a consumer can
/// serialize the shared `Diagnostic` directly — no hand-mirrored struct (U7).
#[test]
fn diagnostic_serializes_via_facade() {
    use oxabl::common::FileId;
    let diags = parse("DEFINE VARIABLE .").into_diagnostics(FileId::new(1));
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
    let (sem, collected) = analyze(
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
    let (_sem, collected) = analyze(source, &opts);
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
    let (sem, _collected) = analyze_with_fs(src, &fs, &opts);
    assert!(sem.is_some());
}

#[test]
fn format_source_formats_and_is_idempotent() {
    use oxabl::style::StyleGuide;
    let style = StyleGuide::default_base();
    let src = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";
    let once = oxabl::try_format_source(src, &style).expect("should format");
    let twice = oxabl::try_format_source(&once, &style).expect("should format");
    assert_eq!(once, twice, "formatter must be idempotent");
}

#[test]
fn format_source_bails_on_parse_errors_without_mangling() {
    use oxabl::formatter::{FormatBail, FormatFailure};
    use oxabl::style::StyleGuide;
    // A parse-broken buffer must bail (ParseErrors), never emit altered bytes.
    // `FormatBail` is still compared by value (R4): the guard adds an arm around
    // it rather than changing it.
    let err = oxabl::try_format_source("DEFINE VARIABLE .", &StyleGuide::default_base())
        .expect_err("parse-dirty input should bail");
    assert_eq!(err, FormatFailure::Bail(FormatBail::ParseErrors));
    assert!(
        !matches!(err, FormatFailure::Panic(_)),
        "a bail must stay distinguishable from a contained panic"
    );
}

/// The panicking originals stay reachable for compatibility, deprecated in favor
/// of the `try_*` siblings. This is the only place that uses them.
#[test]
#[allow(deprecated)]
fn deprecated_panicking_surface_still_resolves() {
    use oxabl::style::StyleGuide;
    use oxabl::workspace::InMemoryFileSystem;

    assert!(oxabl::parse("MESSAGE \"x\".").is_ok());
    let (sem, _diags) = oxabl::analyze("MESSAGE \"x\".", &oxabl::AnalyzeOptions::default());
    assert!(sem.is_some());
    let (sem, _diags) = oxabl::analyze_with_fs(
        "MESSAGE \"x\".",
        &InMemoryFileSystem::new(),
        &oxabl::AnalyzeOptions::default(),
    );
    assert!(sem.is_some());
    assert!(oxabl::format_source("MESSAGE \"x\".", &StyleGuide::default_base()).is_ok());
}

/// The deprecated wrappers are now thin adapters over `oxabl::pipeline`, so they
/// must answer exactly what the pipeline answers for the same input.
///
/// Pinned because the re-pointing is the whole content of that change: if the
/// adapter drifted — a different root file id, a lost diagnostic, a different
/// collection order — a consumer still on the deprecated surface would silently
/// get a different analysis than every in-repo client.
#[test]
#[allow(deprecated)]
fn the_deprecated_wrappers_agree_with_the_pipeline_they_delegate_to() {
    use oxabl::pipeline::{LintPipeline, PipelineConfig};
    use oxabl::style::StyleGuide;
    use oxabl::workspace::InMemoryFileSystem;

    let fs = InMemoryFileSystem::new();
    // Something with a finding in every stage the collector labels: a recovered
    // parse error and an unused variable.
    let source = "DEFINE VARIABLE neverUsed AS INTEGER NO-UNDO.\n@ @ @\n";

    let options = oxabl::AnalyzeOptions::default();
    let (wrapper_model, wrapper_diags) = oxabl::analyze_with_fs(source, &fs, &options);

    let config: PipelineConfig = (&options).into();
    let direct = LintPipeline::new(&config, &fs)
        .with_preprocess(options.preprocess)
        .run(source);

    assert_eq!(&wrapper_diags, direct.diagnostics());
    assert_eq!(wrapper_model.is_some(), direct.semantic().is_some());
    assert!(
        wrapper_diags
            .all()
            .any(|d| d.diagnostic.code.0 == "LINT0002"),
        "the fixture must actually produce findings: {wrapper_diags:?}"
    );

    // The format side likewise: `try_format_source` folds the pipeline's
    // `Unchanged` arm into `Ok(original bytes)` and nothing else.
    let style = StyleGuide::default_base();
    let already = "MESSAGE \"x\".\n";
    assert_eq!(
        oxabl::try_format_source(already, &style).as_deref(),
        Ok(already),
        "an unchanged file comes back as its own bytes, not an error"
    );
}

/// The fallible surface's shapes, pinned as fn-pointers so a later refactor
/// cannot quietly reshape them while the reachability compile-gate still passes.
#[test]
fn fallible_entry_point_signatures_are_pinned() {
    use oxabl::analyze::CollectedDiagnostics;
    use oxabl::common::InternalPanic;
    use oxabl::formatter::FormatFailure;
    use oxabl::semantic::Semantic;
    use oxabl::style::StyleGuide;
    use oxabl::workspace::FileSystem;

    // `try_parse` preserves `Program` whole — recovered errors ride in the `Ok`.
    let _try_parse: fn(&str) -> Result<oxabl::Program, InternalPanic> = oxabl::try_parse;
    let recovered = oxabl::try_parse("DEFINE VARIABLE .").expect("a parse error is not a panic");
    assert!(!recovered.errors.is_empty());

    // The analyze pair preserves the whole tuple, `None` model arm included.
    type Analyzed = Result<(Option<Semantic>, CollectedDiagnostics), InternalPanic>;
    let _try_analyze: fn(&str, &oxabl::AnalyzeOptions) -> Analyzed = oxabl::try_analyze;
    let _try_analyze_with_fs: fn(&str, &dyn FileSystem, &oxabl::AnalyzeOptions) -> Analyzed =
        oxabl::try_analyze_with_fs;

    // The formatter's failure channel is flat, not a nested `Result`.
    let _try_format: fn(&str, &StyleGuide) -> Result<String, FormatFailure> =
        oxabl::try_format_source;
}

/// `catch_panic` and `InternalPanic` are reachable through the facade, and the
/// guard's two outcomes behave as documented.
#[test]
fn panic_guard_is_reachable_through_the_facade() {
    use oxabl::common::{InternalPanic, catch_panic};

    assert_eq!(catch_panic(|| 1 + 1), Ok(2));

    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let err: InternalPanic = catch_panic(|| panic!("boom")).unwrap_err();
    std::panic::set_hook(previous);
    assert!(err.to_string().contains("boom"), "got {err}");
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
    let program = parse("MESSAGE \"x\".");
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

    // The fallible surface and its shared guard, reached only through `oxabl::`.
    type Guarded = Result<u8, oxabl::common::InternalPanic>;
    let _guard: fn(fn() -> u8) -> Guarded = oxabl::common::catch_panic;
    let _ip = oxabl::common::InternalPanic::new("x");
    let _tp = oxabl::try_parse;
    let _tpm = oxabl::parser::try_parse;
    let _ta = oxabl::try_analyze;
    let _tawf = oxabl::try_analyze_with_fs;
    let _tf = oxabl::try_format_source;
    let _ff: Option<oxabl::formatter::FormatFailure> = None;
}
