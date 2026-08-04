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
//! [`FormatPipeline`] — the format run, built from a resolved style guide and
//! *nothing else*, so "the formatter never sees expanded macros" is a property
//! of the type rather than a caller's discipline (R4, KTD4). Every client
//! renders the one [`FormatOutcome`], whose refusal arm keeps the
//! bail-versus-internal-panic distinction structural (R5, R20).
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

mod config;
mod format;
mod lint;
#[cfg(test)]
mod parity;
pub mod position;
mod reverse;

pub use config::{ConfigOverrides, ConfigWarning, PipelineConfig, resolve_from_config};
pub use format::{FormatOutcome, FormatPipeline, NotFormatted, NotFormattedKind};
pub use lint::{Expansion, LintPipeline, LintResult};
pub use position::{
    Position, ResolvedSpan, resolve_diagnostic, resolve_file_span, resolve_offset, resolve_offsets,
};
pub use reverse::{
    Dependent, Dependents, ReverseGraph, Subject, Unanalysed, UnresolvedRow, dependency_section,
};
// Re-exported so a consumer of the reverse query need not also depend on
// `oxabl_index` to name the edge kind every answer is grouped by.
pub use oxabl_index::{DependencyEdge, DependencyEdges, EdgeKind, EdgeTarget, UnresolvedReference};

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
