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

use oxabl_analyze::{CollectedDiagnostics, collect_with_model};
use oxabl_common::{
    FileId, InternalPanic, LintSeverityMap, catch_panic, panic_if_injected, panic_sites,
};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::Semantic;
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

/// The synthetic root [`FileId`](common::FileId) that [`analyze`] uses for an
/// in-memory source. It matches the id the CLI and collector use, so rendered
/// positions line up.
const ANALYZE_ROOT: FileId = FileId::new(1);

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
    fn default() -> Self {
        AnalyzeOptions {
            schema: Schema::empty(),
            schema_loaded: false,
            include_paths: Vec::new(),
            lint_severities: LintSeverityMap::new(),
            preprocess: false,
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
fn analyze_inner(
    source: &str,
    fs: &dyn FileSystem,
    options: &AnalyzeOptions,
) -> (Option<Semantic>, CollectedDiagnostics) {
    panic_if_injected(panic_sites::ANALYZE, source);
    collect_with_model(
        ANALYZE_ROOT,
        source,
        fs,
        &options.include_paths,
        &options.schema,
        options.schema_loaded,
        &options.lint_severities,
        options.preprocess,
    )
}

/// Format ABL `source` with `style`, returning the reformatted string or a
/// [`FormatBail`](formatter::FormatBail) explaining why the file was left
/// untouched.
///
/// One-call convenience mirroring [`parse`], folding tokenize + parse + the
/// layout formatter. Defined in `oxabl_formatter` so the CLI, LSP, and this
/// umbrella all format through one shared entry point; on any bail the original
/// bytes are returned unchanged. Like [`parse`], it may panic on some malformed
/// inputs, so it is deprecated in favor of [`try_format_source`].
#[allow(deprecated)]
pub use oxabl_formatter::format_source;

/// Format ABL `source` with `style`, containing any internal panic — the
/// fallible sibling of [`format_source`] and the entry point new code should
/// reach for.
///
/// Its [`FormatFailure`](formatter::FormatFailure) distinguishes a deliberate
/// bail (leave the file alone; the input was not formattable) from a contained
/// panic (leave the file alone; oxabl has a bug), without nesting `Result`s.
pub use oxabl_formatter::try_format_source;

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

/// Workspace — the file-system abstraction and `oxabl.toml` config resolution.
pub mod workspace {
    pub use oxabl_workspace::{
        FileSystem, InMemoryFileSystem, LintConfig, LintSeverity, RealFileSystem, Workspace,
        WorkspaceConfig, find_workspace_root, resolved_include_paths, resolved_lint_config,
        resolved_style,
    };
}
