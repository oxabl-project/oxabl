//! Oxabl — a high-performance tooling suite for Progress ABL.
//!
//! This umbrella crate is the single dependency a downstream consumer needs to
//! parse, analyze, lint, format, and render diagnostics for ABL source. Rather
//! than re-exporting every sub-crate wholesale, it presents a **curated**
//! surface: each layer lives under a named module ([`ast`], [`parser`],
//! [`semantic`], [`lint`], [`schema`], [`analyze`], [`formatter`], …) that
//! re-exports only the consumer-facing items of its sub-crate, plus a small set
//! of top-level conveniences ([`parse`], [`Program`], [`Diagnostic`]).
//!
//! ```
//! let program = oxabl::try_parse("MESSAGE \"hello\".").expect("no internal panic");
//! assert!(program.is_ok());
//! ```
//!
//! Curating the surface (rather than globbing each sub-crate at the crate root)
//! keeps internal helpers — e.g. the parser's recovery methods — unreachable by
//! construction and avoids cross-crate name collisions as the workspace grows.
//!
//! # Panics are contained, not documented away
//!
//! The lexer and parser can panic on some malformed input, so the three entry
//! points that run them come in guarded form: [`try_parse`], [`try_analyze`] /
//! [`try_analyze_with_fs`], and [`try_format_source`]. Each returns the panic as
//! an [`InternalPanic`](common::InternalPanic) carrying its message rather than
//! unwinding into the caller. These are the canonical entry points; the
//! panicking originals ([`parse`], [`analyze`], [`analyze_with_fs`],
//! [`format_source`]) remain for compatibility and are deprecated.
//!
//! The guard adds exactly one arm and changes nothing else: a **recovered parse
//! error is not a panic** (it still arrives in `Program::errors`), and a
//! **formatter bail is not a panic** (it still arrives as a
//! [`FormatBail`](formatter::FormatBail), now inside
//! [`FormatFailure`](formatter::FormatFailure)).
//!
//! Two conditions on the guarantee, both documented on
//! [`catch_panic`](common::catch_panic): it requires the **unwinding** panic
//! strategy, so a `panic = "abort"` profile anywhere in the build silently
//! reduces every guard to a pass-through; and it is an explicit pass-through on
//! `wasm32-unknown-unknown`, where stable Rust builds with `-Cpanic=abort` and a
//! panic traps instead of unwinding. It does not cover hangs.

use std::path::PathBuf;

use oxabl_analyze::CollectedDiagnostics;
use oxabl_common::{InternalPanic, LintSeverityMap, catch_panic};
use oxabl_formatter::FormatFailure;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_pipeline::{FormatOutcome, FormatPipeline, LintPipeline, PipelineConfig};
use oxabl_schema::Schema;
use oxabl_semantic::Semantic;
use oxabl_style::StyleGuide;
use oxabl_workspace::{FileSystem, RealFileSystem};

/// Parse ABL `source` into a [`Program`], using the parser's error-recovery
/// mode: parsing continues past errors, so the returned `Program` carries both
/// the recovered statements and every [`ParseError`](parser::ParseError) in its
/// `errors` field.
///
/// This is the one-call entry point that folds tokenization and parser
/// construction together — consumers no longer hand-roll
/// `tokenize → Parser::new → parse_program`.
///
/// For `Result`-style flow, use [`Program::into_result`]; for the fail-fast
/// "did it parse, and if not, the first error" shape, use
/// [`Program::first_error`].
///
/// # Panics
///
/// Like the underlying lexer and parser, `parse` may panic on some malformed
/// inputs, which is why it is deprecated in favor of [`try_parse`] — the
/// fallible sibling that contains the panic and reports its message.
#[deprecated(note = "may panic on malformed input; use `try_parse`, which contains the panic")]
pub fn parse(source: &str) -> Program {
    let tokens = tokenize(source);
    Parser::new(&tokens, source).parse_program()
}

/// Parse ABL `source` into a [`Program`], containing any internal panic instead
/// of letting it kill the caller — the fallible sibling of [`parse`] and the
/// entry point new code should reach for.
///
/// Recovered parse errors are **not** a failure here: they arrive in the `Ok`
/// value's `errors` field exactly as they do from [`parse`]. An
/// `Err(`[`InternalPanic`](common::InternalPanic)`)` means an oxabl bug.
///
/// ```
/// let program = oxabl::try_parse("MESSAGE \"x\".").expect("no internal panic");
/// assert!(program.is_ok());
/// ```
pub use oxabl_parser::try_parse;

/// The result of parsing an ABL source file — recovered statements plus any
/// [`ParseError`](parser::ParseError)s. Re-exported at the top level as the
/// return type of [`parse`].
pub use oxabl_parser::Program;

/// A diagnostic (error/warning) with a message, span, severity, and optional
/// labels and help. Re-exported at the top level as the common currency of the
/// analysis layers.
pub use oxabl_common::Diagnostic;

/// Inputs to [`analyze`] / [`analyze_with_fs`], wrapping the five configurable
/// arguments of the underlying pipeline so callers don't juggle a long
/// positional list. Every field has a sensible default (empty schema, no
/// preprocessing, no include paths, built-in lint severities).
///
/// ```
/// let opts = oxabl::AnalyzeOptions { preprocess: true, ..Default::default() };
/// let (_model, diags) = oxabl::try_analyze("MESSAGE \"x\".", &opts).expect("no internal panic");
/// let _ = diags;
/// ```
pub struct AnalyzeOptions {
    /// Schema driving schema-backed resolution (`unknown-table-or-field`, field
    /// typing). Defaults to [`Schema::empty`].
    pub schema: Schema,
    /// Whether a schema was actually loaded. When `false`, schema-dependent
    /// diagnostics stay silent. Kept explicit (not derived from
    /// `Schema::is_empty`) so an intentionally empty `.df` still reads as loaded.
    pub schema_loaded: bool,
    /// PROPATH-style include search directories (only consulted when
    /// `preprocess` is `true`).
    pub include_paths: Vec<PathBuf>,
    /// Per-rule lint severity overrides. Empty keeps every rule's built-in
    /// severity.
    pub lint_severities: LintSeverityMap,
    /// Whether to run preprocessor expansion (include resolution, `&IF`) before
    /// analysis.
    pub preprocess: bool,
}

impl Default for AnalyzeOptions {
    /// # Why the severity map is not empty (R19)
    ///
    /// An empty [`LintSeverityMap`] does not mean "the defaults" — it means *no
    /// rule has a configured severity*, so each diagnostic keeps whatever
    /// severity `oxabl_lint` constructs it with. For `unknown-table-or-field` and
    /// `type-mismatch-assignment` that built-in value is `Error`, while
    /// `[workspace.lint]`'s documented default is `Warn`.
    ///
    /// Leaving it empty here gave an embedding caller a *different answer for the
    /// same input* than the CLI or the editor, both of which resolve
    /// configuration and therefore materialize the documented defaults even with
    /// no `oxabl.toml` present. The client is not allowed to be a variable in the
    /// answer, so this reads from the same [`LintConfig::default`] table
    /// [`PipelineConfig`](pipeline::PipelineConfig)'s own `Default` does.
    fn default() -> Self {
        AnalyzeOptions {
            schema: Schema::empty(),
            schema_loaded: false,
            include_paths: Vec::new(),
            lint_severities: workspace::LintConfig::default().to_severity_map(),
            preprocess: false,
        }
    }
}

/// The bridge from this crate's caller-facing options onto the shared
/// [`PipelineConfig`](pipeline::PipelineConfig) the run actually consumes.
///
/// `AnalyzeOptions` is **not** deprecated: it stays the live input type on the
/// `try_*` surface, and it is the browser client's only configuration handle. So
/// it does not *become* a `PipelineConfig`, it converts into one — the two differ
/// deliberately in both directions:
///
/// * `preprocess` has no counterpart here. Whether to expand macros is a property
///   of the *run*, not of the configuration, and it lives on
///   [`LintPipeline::with_preprocess`](pipeline::LintPipeline::with_preprocess).
///   The format pipeline has no such switch at all, by construction.
/// * `style` has no counterpart in `AnalyzeOptions`. Analysis does not format, so
///   asking a caller of `try_analyze` for a style guide would be asking for an
///   input that cannot affect the answer; the config's default stands in.
impl From<&AnalyzeOptions> for PipelineConfig {
    fn from(options: &AnalyzeOptions) -> Self {
        PipelineConfig {
            include_paths: options.include_paths.clone(),
            lint_severities: options.lint_severities.clone(),
            schema: options.schema.clone(),
            schema_loaded: options.schema_loaded,
            ..PipelineConfig::default()
        }
    }
}

/// Run the full parse → semantic → lint pipeline over an in-memory `source`,
/// returning the [`Semantic`](semantic::Semantic) model (absent only on a fatal
/// preprocessing failure) and the collected diagnostics.
///
/// This is the one-call convenience over the pipeline's long positional form:
/// `source` is the argument, five inputs come from `options`, and the file
/// system defaults to [`RealFileSystem`](workspace::RealFileSystem) (so include
/// resolution reads from disk). Consumers that must stay off disk — the LSP, or
/// tests — should use [`analyze_with_fs`] with an in-memory file system.
///
/// # Panics
///
/// The pipeline parses `source`, so like [`parse`] it may panic on some
/// malformed inputs. That is why this function is deprecated in favor of
/// [`try_analyze`], the fallible sibling that contains the panic.
#[deprecated(note = "may panic on malformed input; use `try_analyze`, which contains the panic")]
pub fn analyze(source: &str, options: &AnalyzeOptions) -> (Option<Semantic>, CollectedDiagnostics) {
    analyze_inner(source, &RealFileSystem, options)
}

/// Like [`analyze`], but with a caller-provided [`FileSystem`](workspace::FileSystem)
/// for include resolution — e.g. an
/// [`InMemoryFileSystem`](workspace::InMemoryFileSystem) so analysis never
/// touches disk.
///
/// # Panics
///
/// Same transitive panic surface as [`analyze`]; use [`try_analyze_with_fs`].
#[deprecated(
    note = "may panic on malformed input; use `try_analyze_with_fs`, which contains the panic"
)]
pub fn analyze_with_fs(
    source: &str,
    fs: &dyn FileSystem,
    options: &AnalyzeOptions,
) -> (Option<Semantic>, CollectedDiagnostics) {
    analyze_inner(source, fs, options)
}

/// Run the full parse → semantic → lint pipeline over `source`, containing any
/// internal panic — the fallible sibling of [`analyze`] and the entry point new
/// code should reach for.
///
/// The success shape is preserved whole, including the `None` model arm that
/// signals a fatal preprocessing failure: the guard adds a panic arm and
/// changes nothing else.
pub fn try_analyze(
    source: &str,
    options: &AnalyzeOptions,
) -> Result<(Option<Semantic>, CollectedDiagnostics), InternalPanic> {
    try_analyze_with_fs(source, &RealFileSystem, options)
}

/// Like [`try_analyze`], but with a caller-provided
/// [`FileSystem`](workspace::FileSystem) — the fallible sibling of
/// [`analyze_with_fs`].
///
/// # Platform caveat
///
/// The guard is a documented pass-through on `wasm32-unknown-unknown`; see
/// [`catch_panic`](common::catch_panic).
pub fn try_analyze_with_fs(
    source: &str,
    fs: &dyn FileSystem,
    options: &AnalyzeOptions,
) -> Result<(Option<Semantic>, CollectedDiagnostics), InternalPanic> {
    catch_panic(|| analyze_inner(source, fs, options))
}

/// The shared body, so the fallible entry points do not have to call their own
/// deprecated twins.
///
/// This is a thin adapter over [`LintPipeline`](pipeline::LintPipeline) (KTD13),
/// not a second orchestration: the root file id, the expansion, the collection
/// order, and the test-panic injection site all come from the shared pipeline, so
/// this surface and the CLI/LSP/browser cannot answer differently for the same
/// input.
///
/// It drives the **unguarded** `expand`/`collect` phases rather than the guarded
/// `run`, which is load-bearing and not an oversight. `run` contains a panic and
/// reports it as a failed result; this function's callers are split precisely on
/// that question — [`try_analyze`] wraps it in the guard, while the deprecated
/// [`analyze`] is documented to panic. Calling `run` here would make `analyze`
/// silently return an empty model on an internal panic and make `try_analyze`
/// incapable of ever returning `Err`.
fn analyze_inner(
    source: &str,
    fs: &dyn FileSystem,
    options: &AnalyzeOptions,
) -> (Option<Semantic>, CollectedDiagnostics) {
    let config: PipelineConfig = options.into();
    let pipeline = LintPipeline::new(&config, fs).with_preprocess(options.preprocess);
    let expansion = pipeline.expand(source);
    pipeline.collect(&expansion).into_parts()
}

/// Format ABL `source` with `style`, returning the reformatted string or a
/// [`FormatBail`](formatter::FormatBail) explaining why the file was left
/// untouched.
///
/// One-call convenience mirroring [`parse`], folding tokenize + parse + the
/// layout formatter; on any bail the original bytes are returned unchanged. Like
/// [`parse`], it may panic on some malformed inputs, so it is deprecated in
/// favor of [`try_format_source`].
#[deprecated(
    note = "may panic on malformed input; use `try_format_source`, which contains the panic"
)]
#[allow(deprecated)]
pub fn format_source(
    source: &str,
    style: &StyleGuide,
) -> Result<String, oxabl_formatter::FormatBail> {
    oxabl_formatter::format_source(source, style)
}

/// Format ABL `source` with `style`, containing any internal panic — the
/// fallible sibling of [`format_source`] and the entry point new code should
/// reach for.
///
/// Its [`FormatFailure`](formatter::FormatFailure) distinguishes a deliberate
/// bail (leave the file alone; the input was not formattable) from a contained
/// panic (leave the file alone; oxabl has a bug), without nesting `Result`s.
///
/// The body drives the shared
/// [`FormatPipeline`](pipeline::FormatPipeline) (KTD13), so this surface, the
/// CLI, the language server, and the browser all format through one handle. The
/// mapping back to the flat `Result` is total and lossless:
/// [`Unchanged`](pipeline::FormatOutcome::Unchanged) is `Ok` with the original
/// bytes — the pipeline splits "ran, output identical" out as its own answer,
/// while this signature has always folded it into `Ok`.
pub fn try_format_source(source: &str, style: &StyleGuide) -> Result<String, FormatFailure> {
    match FormatPipeline::new(style.clone()).format(source) {
        FormatOutcome::Unchanged => Ok(source.to_string()),
        FormatOutcome::Reformatted(formatted) => Ok(formatted),
        FormatOutcome::DidNotFormat(not_formatted) => Err(not_formatted.failure().clone()),
    }
}

/// Render a slice of [`Diagnostic`]s to `path:line:col: severity[code]: message`
/// text with source snippets, using a [`SourceResolver`](common::SourceResolver)
/// for positions. The reusable renderer consumers (and the CLI) share instead of
/// reinventing `file:line:col` formatting.
pub use oxabl_common::render_diagnostics;

/// AST node definitions — statements, expressions, data types, literals, and
/// source-location types. This module surfaces `oxabl_ast`'s full public model:
/// the node set is large and deeply interlinked (matching on a [`Statement`]
/// reaches its [`StatementKind`], payload structs, and [`Span`]), so it is
/// re-exported as one curated data-model namespace rather than an item list
/// that would drift on every new node.
pub mod ast {
    pub use oxabl_ast::*;
}

/// Tokenizer for ABL source — the batch [`tokenize`](oxabl_lexer::tokenize)
/// entry point plus the token model.
pub mod lexer {
    pub use oxabl_lexer::oxabl_atom::OxablAtom;
    pub use oxabl_lexer::{
        BUILTIN_FUNCTIONS, CALLABLE_FUNCTION_KINDS, Kind, Lexer, Token, TokenValue,
        is_builtin_function, is_callable_kind, tokenize,
    };
}

/// Parser for ABL source. Prefer the top-level [`parse`] convenience for new
/// code; [`Parser`] and the two entry-point methods (`parse_program` for error
/// recovery, `parse_statements` for fail-fast) remain available for finer
/// control.
pub mod parser {
    pub use oxabl_parser::{ParseError, ParseResult, Parser, Program, try_parse};
}

/// Shared primitives — source maps, file identity, spans, and the diagnostic
/// model shared across every analysis layer.
pub mod common {
    pub use oxabl_ast::Span;
    pub use oxabl_common::{
        Diagnostic, DiagnosticCode, FileId, FileSet, FileSpan, InternalPanic, Label,
        LintSeverityMap, Severity, SourceMap, SourceResolver, VirtualSpan, blank_lines_between,
        catch_panic, render_diagnostics,
    };
}

/// Preprocessor — include expansion and `&IF`/`&DEFINE` evaluation, with
/// virtual-span mapping back to real source.
pub mod preprocessor {
    pub use oxabl_preprocessor::{PreprocVarTable, PreprocessedFile, Preprocessor, SpanNode};
}

/// Schema model and `.df` loader for schema-backed semantic analysis.
pub mod schema {
    pub use oxabl_schema::{
        Field, FieldResolution, Index, IndexField, ParseOutcome, Schema, SchemaLoader,
        SchemaRevision, SchemaType, Table, TableId, fold_atom, parse_df,
    };
}

/// Semantic analysis — the declare/resolve/check pipeline over the side-table
/// model. [`analyze_file`](semantic::analyze_file) composes the passes.
pub mod semantic {
    pub use oxabl_semantic::{
        AnalysisContext, BindingMap, NamespaceId, PrimitiveTy, Resolution, ResolvedType, Scope,
        ScopeId, ScopeKind, ScopeTree, Semantic, Symbol, SymbolFlags, SymbolId, SymbolKind,
        SymbolTable, UnresolvedReason, analyze_file, check_pass, declare_pass, resolve_pass,
    };
}

/// Lint rules over the semantic model. [`lint_file`](lint::lint_file) runs the
/// full rule set; the individual `LINT000x` codes name each rule.
pub mod lint {
    pub use oxabl_lint::{LINT0001, LINT0002, LINT0003, LINT0004, lint_file};
}

/// Analysis dump + diagnostic collection — the JSON/text envelope over the
/// semantic model and the one-call collection pipeline.
pub mod analyze {
    pub use oxabl_analyze::{
        CollectedDiagnostic, CollectedDiagnostics, DiagnosticSource, ExpandedFile,
        collect_diagnostics, collect_from_expanded, collect_with_model, dump_json,
        dump_json_with_diagnostics, dump_text, dump_text_with_diagnostics, expand_source, is_loud,
    };
}

/// Layout formatter — the pure [`format`](formatter::format) entry point and
/// its [`FormatBail`](formatter::FormatBail) failure channel, plus the
/// [`FormatFailure`](formatter::FormatFailure) channel that
/// [`try_format_source`] adds a contained-panic arm to.
pub mod formatter {
    pub use oxabl_formatter::{FormatBail, FormatFailure, format};
}

/// Style guide — typed, configurable formatting/diagnostic rules and the two
/// named presets.
pub mod style {
    pub use oxabl_style::{
        AndOrPlacement, BufferNaming, ClassNaming, CommentStyle, FileNameCasing, IndentStyle,
        KeywordAbbreviation, KeywordCase, MethodCase, ParameterPrefix, PeriodPlacement, Placement,
        Scope, StaticMemberRef, StyleGuide, SubstitutePolicy, TempTablePrefix, VariableCase,
        VariableDeclAlignment,
    };
}

/// Shared lint/format pipelines — one resolution of `oxabl.toml` into everything
/// a run needs, and the run itself, so the CLI, the LSP, and the browser client
/// are renderers of one shared result rather than three parallel orchestrations
/// (R1).
///
/// [`PipelineConfig::resolve`](pipeline::PipelineConfig::resolve) reads
/// `oxabl.toml` once and returns its non-fatal problems as
/// [`ConfigWarning`](pipeline::ConfigWarning) data, leaving *how* to surface them
/// to the client (R6, R7). [`ROOT_FILE_ID`](pipeline::ROOT_FILE_ID) is the one
/// synthetic root file id every client shares.
///
/// The **run handles** are here too — [`LintPipeline`](pipeline::LintPipeline),
/// [`FormatPipeline`](pipeline::FormatPipeline), their result types, and the
/// [`position`](pipeline::position) helper. Narrowing this to the configuration
/// surface would mean an external consumer could resolve a `PipelineConfig` and
/// then have nothing to hand it to: the shared run would be reachable only by
/// taking a direct dependency on `oxabl_pipeline`, which is exactly the "one
/// dependency is enough" promise this umbrella exists to keep. The two in-repo
/// clients that *do* take the direct edge (`oxabl_lsp`, `oxabl_wasm`) keep it —
/// they compile `oxabl_pipeline` regardless, so routing through the re-export
/// would add an indirection without removing anything.
///
/// This module is **not** gated on the `cli` feature, and must not become gated:
/// `oxabl_wasm` depends on this crate with `default-features = false`, so a gated
/// re-export would be invisible to the browser client.
pub mod pipeline {
    pub use oxabl_pipeline::{
        ConfigOverrides, ConfigWarning, Expansion, FormatOutcome, FormatPipeline, LintPipeline,
        LintResult, NotFormatted, NotFormattedKind, PipelineConfig, ROOT_FILE_ID, position,
        resolve_from_config,
    };
}

/// Workspace — the file-system abstraction, `oxabl.toml` parsing, and root-file
/// discovery.
///
/// For resolved *configuration*, reach for [`pipeline::PipelineConfig`] rather
/// than assembling surfaces here: it reads `oxabl.toml` once and derives include
/// paths, lint severities, style, and schema together. The per-surface
/// `resolved_lint_config` and `resolved_style` helpers this module used to
/// re-export are gone, because each re-parsed the file and they could disagree.
/// `resolved_include_paths` remains for the one caller that wants that single
/// surface without a full resolution.
pub mod workspace {
    pub use oxabl_workspace::{
        FileSystem, InMemoryFileSystem, LintConfig, LintSeverity, ROOT_EXTENSIONS, RealFileSystem,
        Workspace, WorkspaceConfig, discover_path, find_workspace_root, is_root_file,
        resolved_include_paths, walk_directory,
    };
}
