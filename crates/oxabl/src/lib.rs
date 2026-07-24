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
//! let program = oxabl::parse("MESSAGE \"hello\".");
//! assert!(program.is_ok());
//! ```
//!
//! Curating the surface (rather than globbing each sub-crate at the crate root)
//! keeps internal helpers — e.g. the parser's recovery methods — unreachable by
//! construction and avoids cross-crate name collisions as the workspace grows.

use oxabl_lexer::tokenize;
use oxabl_parser::Parser;

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
/// inputs. A consumer that must isolate such panics (as the CLI and LSP do)
/// should wrap the call in [`std::panic::catch_unwind`]. A panic-catching
/// variant is not part of v1.
pub fn parse(source: &str) -> Program {
    let tokens = tokenize(source);
    Parser::new(&tokens, source).parse_program()
}

/// The result of parsing an ABL source file — recovered statements plus any
/// [`ParseError`](parser::ParseError)s. Re-exported at the top level as the
/// return type of [`parse`].
pub use oxabl_parser::Program;

/// A diagnostic (error/warning) with a message, span, severity, and optional
/// labels and help. Re-exported at the top level as the common currency of the
/// analysis layers.
pub use oxabl_common::Diagnostic;

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
    pub use oxabl_parser::{ParseError, ParseResult, Parser, Program};
}

/// Shared primitives — source maps, file identity, spans, and the diagnostic
/// model shared across every analysis layer.
pub mod common {
    pub use oxabl_ast::Span;
    pub use oxabl_common::{
        Diagnostic, DiagnosticCode, FileId, FileSet, FileSpan, Label, LintSeverityMap, Severity,
        SourceMap, SourceResolver, VirtualSpan, blank_lines_between, render_diagnostics,
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
/// its [`FormatBail`](formatter::FormatBail) failure channel.
pub mod formatter {
    pub use oxabl_formatter::{FormatBail, format};
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
