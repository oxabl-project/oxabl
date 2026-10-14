//! `delete-not-in-finally` lint (HD002), group `strict.handles`.
//!
//! Fires on a statement that deletes a dynamic object on the success path
//! only. Every path that raises nothing deletes the object, but an error, a
//! `THROW`, a `RETURN ERROR`, `STOP` or `ENDKEY` between the create and the
//! delete skips the delete.
//!
//! # Why
//!
//! Code that runs a query, a call or a persistent procedure can fail in the
//! middle. When it does, control goes to a `CATCH` block or to the caller, and
//! the lines after the failing statement do not run. A delete among them is
//! skipped, and the object stays alive. A `FINALLY` block runs on every exit
//! of its block, so a delete there runs also on error.
//!
//! ```abl
//! PROCEDURE load-orders:
//!     DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
//!     CREATE QUERY hQuery.
//!     hQuery:QUERY-PREPARE("for each Order").
//!     hQuery:QUERY-OPEN().
//!     DELETE OBJECT hQuery.
//! END PROCEDURE.
//! ```
//!
//! Fixed — the delete is in a `FINALLY` block:
//!
//! ```abl
//! PROCEDURE load-orders:
//!     DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
//!     DO ON ERROR UNDO, THROW:
//!         CREATE QUERY hQuery.
//!         hQuery:QUERY-PREPARE("for each Order").
//!         hQuery:QUERY-OPEN().
//!         FINALLY:
//!             IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
//!         END FINALLY.
//!     END.
//! END PROCEDURE.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per delete statement, anchored on the delete. The message
//! names the first statement after which an error can skip it (`via`). Any
//! delete form counts: `DELETE OBJECT`, `DELETE PROCEDURE`, `DELETE WIDGET`,
//! `RELEASE OBJECT`, `SET-SIZE(m) = 0`, `APPLY "CLOSE"`.
//!
//! Not reported: an object that is also lost on a path without an error
//! (`handle-not-deleted`, HD001, reports it), an object whose delete is already
//! in a `FINALLY` block, and an object that escapes or is in an unknown state.
//!
//! The rule reads every statement that can raise an error as a place where the
//! delete may be skipped, even one that you handle with `NO-ERROR`, because
//! `NO-ERROR` does not stop `STOP` or `ENDKEY`. That makes this rule noisy for
//! short procedures, which is the reason it is in preview.
//!
//! # Include files
//!
//! A delete in an include is reported in the include, once.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_handles::FileHandles;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::HD002;
use super::handle_support::{describe, quote, warning};
use super::syntactic::file_span;

/// Kebab-case rule name.
pub const NAME: &str = "delete-not-in-finally";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.handles";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "A delete that an error between the create and the delete skips";

/// Entry point: analyse the file, then [`check`].
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(&oxabl_handles::analyze_file(program, ctx.source), ctx)
}

/// The findings over an analysed file.
pub fn check(handles: &FileHandles, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for (_, o) in handles.objects() {
        if !o.leaks_on_abnormal_path_only() || o.frees.is_empty() {
            continue;
        }
        let via = o
            .leaks
            .iter()
            .filter_map(|l| l.via)
            .min_by_key(|s| s.span.start);
        let mut seen = Vec::new();
        for free in o.frees.iter().filter(|f| !f.in_finally) {
            if seen.contains(&free.site.span) {
                continue;
            }
            seen.push(free.site.span);
            let cause = match via {
                Some(step) if step.span.start < step.span.end => format!(
                    "an error raised by `{}` skips the delete",
                    quote(ctx.source, step.span)
                ),
                _ => "an error raised before it skips the delete".to_string(),
            };
            let mut d = warning(
                ctx,
                HD002,
                format!(
                    "`{}` frees {} on the success path only: {cause}",
                    quote(ctx.source, free.site.span),
                    describe(o)
                ),
                "Move the delete into a `FINALLY` block of the block that creates the object, \
                 and guard it with `IF VALID-HANDLE(...)`, so it also runs when an error is \
                 raised."
                    .to_string(),
                free.site.span,
            );
            if let Some(step) = via.filter(|s| s.span.start < s.span.end) {
                d = d.with_label(
                    file_span(ctx, step.span),
                    "an error here skips the delete".to_string(),
                );
            }
            diags.push(d);
        }
    }
    diags
}
