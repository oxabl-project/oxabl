//! Shared lint and format pipelines for every oxabl client.
//!
//! Before this crate existed, `oxabl` had a shared *library surface* but not a
//! shared *run*: the CLI, the LSP, and the browser WASM client each resolved
//! their own configuration and orchestrated their own pipeline, so a fix to one
//! never reached the other two. `oxabl_pipeline` owns config resolution, the run
//! itself, and one result model, reducing each client to a renderer of that
//! result (R1).
//!
//! # Where this crate sits
//!
//! Deliberately *beneath* `oxabl_lsp`, `oxabl_wasm`, and the `oxabl` umbrella
//! (KTD1). The umbrella depends on `oxabl_lsp` (optionally, for `oxabl lsp`) and
//! `oxabl_wasm` depends on the umbrella, so hosting the pipelines in the
//! umbrella would make the LSP depend on a crate that depends on it — a package
//! cycle Cargo rejects. The umbrella re-exports this crate as `oxabl::pipeline`
//! instead.
//!
//! Two consequences of that position are load-bearing rather than incidental:
//!
//! * This crate must **never** depend on `salsa` (KTD6). Incremental
//!   recomputation and cancellation stay the LSP's concern. The umbrella
//!   re-exports this crate unconditionally and the browser bundle is built
//!   through the umbrella, so a `salsa` edge here would be shipped to every
//!   browser visitor.
//! * The `oxabl::pipeline` re-export must **not** sit behind the umbrella's
//!   `cli` feature: `oxabl_wasm` depends on the umbrella with
//!   `default-features = false, features = ["serde"]`, and a `cli`-gated
//!   re-export would be invisible to the browser client.
//!
//! # What lives here
//!
//! [`PipelineConfig`] — one resolution of `oxabl.toml` into include paths, lint
//! severities, a style guide, and a schema, with non-fatal problems returned as
//! [`ConfigWarning`] data (R6, R7, KTD3) — plus the shared [`ROOT_FILE_ID`].
//!
//! [`LintPipeline`] — the lint run itself, built on that configuration: two
//! phases ([`expand`](LintPipeline::expand) then
//! [`collect`](LintPipeline::collect), the split the language server's early
//! cutoff needs) plus a guarded [`run`](LintPipeline::run) convenience, all
//! producing one [`LintResult`] (R2, R3, KTD2, KTD6).
//!
//! [`SharedRun`] — what the run handles of a walk on several threads share:
//! one cross-file index and the analyses of conditional include targets, so a
//! build tool that asks for every file's dependency edges reads and analyses
//! what the files share once.
//!
//! [`LintPipeline::edges_of_observed`] — the same edge set, with the files the
//! analysis read, the paths it found absent and the directories it listed. A
//! build tool that stores edges stores these too, so it knows which files a new
//! or deleted file puts in question. The run reads through an
//! [`ObservingFileSystem`](oxabl_workspace::ObservingFileSystem).
//!
//! [`FormatPipeline`] — the format run, built from a resolved style guide and
//! *nothing else*, so "the formatter never sees expanded macros" is a property
//! of the type rather than a caller's discipline (R4, KTD4). Every client
//! renders the one [`FormatOutcome`], whose refusal arm keeps the
//! bail-versus-internal-panic distinction structural (R5, R20).
//!
//! [`outline`] — the declarations of one file as a tree, for an editor's outline
//! view, breadcrumbs and "Go to Symbol in File". It reads the raw source, like
//! [`FormatPipeline`], so every span is a byte range of the buffer as written.
//! [`outline_with_includes`] adds the declarations of the include files the
//! preprocessor expanded under the symbol of each `{file.i}` reference.
//!
//! [`schema_view`] — the configured `.df` files as a tree of databases, tables,
//! fields, indexes and sequences, for an editor's schema browser, and
//! [`schema_table`], one table as the browser and the hover show it.
//!
//! [`document_syntax`] — the folding ranges and nested selection spans of the
//! same raw source. One result lets a client share the parse between both
//! requests. Include text never enters these ranges.
//!
//! [`resolve_diagnostic`] and its siblings — the single derivation of line and
//! column from a byte span, for the byte-offset clients. The language server
//! deliberately does not use it (R13, KTD5); see [`position`].

// `test-support` is how the three downstream parity legs reach the table; `test`
// is how this crate's own leg (`parity`) does, because a self `path = "."`
// dev-dependency — the only way to feature-enable this crate for its own
// `tests/` target — reads as a dependency cycle to release-please.
#[cfg(any(test, feature = "test-support"))]
pub mod fixtures;

mod builtin_signatures;
mod effects;
mod explain;
pub use effects::{EffectFile, Elsewhere, Routine, WorkspaceEffects};
pub mod completion;
mod config;
mod format;
mod format_range;
mod hierarchy;
mod hints;
mod includers;
mod keyword_docs;
mod lint;
mod navigate;
mod outline;
mod outline_includes;
#[cfg(test)]
mod parity;
pub mod position;
mod references;
mod reverse;
pub mod schema_hover;
mod schema_view;
mod syntax;

pub use config::{
    ConfigOverrides, ConfigWarning, PipelineConfig, configured_schema_files, resolve_from_config,
    resolve_from_config_deferred,
};
pub use format::{FormatOutcome, FormatPipeline, NotFormatted, NotFormattedKind};
pub use format_range::{RangeBail, RangeEdit, RangeFailure};
pub use hierarchy::{
    ClassDecl, MethodAt, MethodDecl, MethodsAt, TypeAt, class_decl, class_name_span,
};
pub use hints::{HintKinds, InlayHint, InlayKind};
pub use includers::{IncluderPools, PoolVerdict};
pub use lint::{
    Expansion, LintPipeline, LintResult, NotObserving, ObservedEdges, SharedRun,
    encrypted_root_diagnostics,
};
pub use navigate::{
    Highlight, Hover, Link, Navigation, NavigationModel, SemanticKind, SemanticModifiers,
    SemanticToken, SignatureHelp, SignatureInfo, Target, close_open_call,
};
pub use outline::{OutlineKind, OutlineSymbol, outline};
pub use outline_includes::{
    IncludeFile, IncludeOutline, MAX_INCLUDED_SYMBOLS, OutlineNode, marker as outline_marker,
    outline_with_includes,
};
// Re-exported so a client of navigation and the call hierarchy can name the
// byte spans their answers carry.
pub use oxabl_ast::Span;
pub use references::{
    CallItem, CallItemKind, Gap, IncomingCall, ItemKey, Occurrence, Outgoing, OutgoingCall, Reach,
    Referent, Search, UnitIncoming, UnitReferences, dedupe_spans, fold_gaps,
};
pub use schema_view::{
    SchemaDatabase, SchemaField, SchemaIndex, SchemaIndexField, SchemaProblem, SchemaSequence,
    SchemaTable, SchemaView, TextPosition, TextRange, schema_database, schema_table, schema_view,
    schema_view_table, schema_view_until,
};
// Re-exported so a consumer of `LintResult::include_origin` can name its rows.
pub use oxabl_analyze::{IncludeOccurrence, IncludeOrigin, IncludeOriginDiagnostic, IncludeStep};
pub use position::{
    Position, ResolvedSpan, resolve_diagnostic, resolve_file_span, resolve_offset, resolve_offsets,
};
pub use reverse::{
    Degraded, Dependent, Dependents, ReverseGraph, Subject, Unanalysed, UnnameableEdge,
    UnresolvedRow, dependency_section,
};
pub use syntax::{DocumentSyntax, Fold, FoldKind, document_syntax};
// Re-exported so a consumer of the reverse query need not also depend on
// `oxabl_index` to name the edge kind every answer is grouped by.
pub use oxabl_index::{
    BranchKind, ClosureReference, Completeness, ConditionalBranch, ConditionalInclude,
    ConditionalIncludeClosure, ConditionalReference, DegradedReason, DependencyEdge,
    DependencyEdges, EdgeKind, EdgeTarget, IncludeContext, ReferenceSyntax, SharedProducers,
    UnresolvedReference, definitions_in,
};

use oxabl_common::FileId;

/// The synthetic [`FileId`] of the file under analysis — the root of the
/// preprocessor's include tree.
///
/// One constant, owned here, because every client needs the *same* one: the
/// preprocessor assigns include file ids starting at `root + 1`, and rendered
/// diagnostic positions only line up if the collector and the surfacing helper
/// agree on which id is the root. A fixed root of 1 never collides with an
/// include.
///
/// # Scheduled convergence (KTD12)
///
/// Two constants in the workspace are already `FileId::new(1)`, by coincidence
/// rather than by declaration: `oxabl_lsp::db::ROOT_FILE_ID` and the umbrella's
/// private `ANALYZE_ROOT`. Nothing today states that they must agree, so nothing
/// stops one of them from drifting. This constant is their single owner going
/// forward; the duplication that remains is **scheduled, not accidental** — the
/// units that rewire the LSP and the umbrella onto the pipeline delete their
/// local copies and point their call sites here. Do not add a third.
pub const ROOT_FILE_ID: FileId = FileId::new(1);
