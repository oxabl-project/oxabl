//! `missing-widget-pool` lint (HD005), group `strict.handles`.
//!
//! Fires once per file that creates handle-based objects with no widget pool
//! scoped to the procedure. The diagnostic sits on the first such create, and
//! its message gives the number of them.
//!
//! # Why
//!
//! A handle-based object (a query, buffer, temp-table, dataset, call or widget)
//! goes into the most recent unnamed widget pool of the session, or into the
//! session pool when there is none. That pool is the caller's. The objects that
//! this procedure forgets to delete, on an error path or by mistake, stay
//! there until some other procedure ends. A widget pool of the procedure
//! catches them: `DELETE WIDGET-POOL`, or the end of the procedure, frees
//! whatever is left.
//!
//! ```abl
//! DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
//! CREATE QUERY hQuery.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
//! CREATE WIDGET-POOL.
//! CREATE QUERY hQuery.
//! ```
//!
//! # What is and is not reported
//!
//! The scope of a pool is the unit that creates it: an unnamed
//! `CREATE WIDGET-POOL` that dominates the create, the unnamed pool of the main
//! block (it covers the file's other procedures), or a named pool that the file
//! creates and the object names with `IN WIDGET-POOL`.
//!
//! The rule counts an object when all of these hold:
//!
//! - its kind goes into a widget pool. Persistent procedures, MEMPTRs, COM
//!   objects, class instances, server and socket objects, and `CREATE VALUE`
//!   whose type is not known do not;
//! - no pool of the file holds it;
//! - it does not escape. An object that the procedure hands to its caller (an
//!   `OUTPUT` parameter, `RETURN h`, a member) must outlive the procedure, and
//!   a pool of the procedure would delete it.
//!
//! An object that this procedure deletes on every path is counted too. The
//! pool is a safety net for the paths that the delete misses, so the rule is
//! advice and not a proof of a leak.
//!
//! # With `handle-not-deleted` (HD001)
//!
//! HD001 reports an unpooled object that leaks as well. See that rule for why
//! the two are not merged.
//!
//! # Include files
//!
//! The diagnostic is reported where its first create is. A root file reports
//! only a create that is in the root file itself, so when the first such
//! create sits in an include, the include reports it and the root reports
//! nothing. An include may count on a pool that its includer creates, so a
//! finding in an include says so and is lower confidence.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_handles::{FileHandles, Placement};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::HD005;
use super::handle_support::{warning, with_fragment_note};

/// Kebab-case rule name.
pub const NAME: &str = "missing-widget-pool";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.handles";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "Dynamic objects created with no widget pool scoped to the procedure";

/// Entry point: analyse the file, then [`check`].
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(&oxabl_handles::analyze_file(program, ctx.source), ctx)
}

/// The findings over an analysed file.
pub fn check(handles: &FileHandles, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let unpooled: Vec<_> = handles
        .objects()
        .map(|(_, o)| o)
        .filter(|o| {
            o.kind.uses_widget_pool() && o.placement == Placement::Unscoped && o.escapes.is_empty()
        })
        .collect();
    let Some(first) = unpooled.iter().min_by_key(|o| o.create.span.start) else {
        return Vec::new();
    };
    let (count, they) = match unpooled.len() {
        1 => ("1 dynamic object is".to_string(), "it goes"),
        n => (format!("{n} dynamic objects are"), "they go"),
    };
    let d = warning(
        ctx,
        HD005,
        format!(
            "{count} created in this file with no widget pool scoped to it, so {they} into the \
             widget pool of the caller or the session pool"
        ),
        "Add `CREATE WIDGET-POOL.` before the first create, so the pool frees what the \
         procedure leaves behind when it ends. A pool that an object must outlive needs a \
         name and `IN WIDGET-POOL`."
            .to_string(),
        first.create.span,
    );
    vec![with_fragment_note(
        d,
        ctx.source_context.is_include_fragment(),
    )]
}
