//! `handle-not-deleted` lint (HD001), group `strict.handles`.
//!
//! Fires on a handle-based object, widget or COM object that some path of its
//! procedure loses without deleting it, when that path raises no error.
//!
//! # Why
//!
//! A dynamic query, buffer, temp-table, dataset, call or widget lives until
//! something deletes it. The AVM does not collect it when the variable goes out
//! of scope. A procedure that creates one and returns without a `DELETE OBJECT`
//! leaves it in the widget pool of its caller, or in the session pool. In a
//! server that runs for days the objects pile up, and each one holds memory and
//! buffers.
//!
//! ```abl
//! DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
//! CREATE QUERY hQuery.
//! hQuery:SET-BUFFERS(BUFFER Customer:HANDLE).
//! IF NOT lOpen THEN RETURN.
//! hQuery:QUERY-OPEN().
//! DELETE OBJECT hQuery.
//! ```
//!
//! The early `RETURN` skips the delete. Fixed — every path deletes, and a
//! `FINALLY` block also covers the error paths:
//!
//! ```abl
//! DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
//! DO ON ERROR UNDO, THROW:
//!     CREATE QUERY hQuery.
//!     hQuery:SET-BUFFERS(BUFFER Customer:HANDLE).
//!     IF NOT lOpen THEN RETURN.
//!     hQuery:QUERY-OPEN().
//!     FINALLY:
//!         IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
//!     END FINALLY.
//! END.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per object. It sits on the create statement, with a label on
//! the statement after which the object is lost. When the only loss is an
//! overwrite (a second `CREATE` into the same variable, `hQuery = ?`), it sits
//! on the overwriting statement.
//!
//! Not reported:
//!
//! - an object that every path deletes, hands to an owner outside the
//!   procedure (an `OUTPUT` parameter, `RETURN h`, a class member, a shared or
//!   outer variable, a field), or leaves in a widget pool the procedure
//!   deletes;
//! - an object that is lost only on a path that raises an error, a `THROW` or a
//!   `RETURN ERROR`: that is `delete-not-in-finally` (HD002) when a delete
//!   exists;
//! - a persistent procedure (`persistent-procedure-leak`, HD003) and a MEMPTR
//!   (`memptr-not-freed`, HD004);
//! - a class instance, which the AVM collects;
//! - an object whose lifetime the analysis cannot see to the end: a statement
//!   the parser skips that names the handle, a dynamic call that receives it, a
//!   call the file does not define (`RUN other.p (INPUT h)`, `RUN p IN hLib
//!   (h)`, a prototype such as `PROCEDURE p IN SUPER`, a method of an object of
//!   another class) that receives it, a delete of something that is not the
//!   handle itself, or an include that was not expanded. An `INPUT` handle
//!   passed to a procedure, function or method of the file that deletes its
//!   parameter counts as a delete; one that stores, returns or hands on its
//!   parameter is an owner outside the procedure.
//!
//! `IF VALID-HANDLE(h) THEN DELETE OBJECT h.` counts as a delete on every
//! path: a live object has a valid handle.
//!
//! # With `missing-widget-pool` (HD005)
//!
//! A leaking object that sits in no widget pool of the procedure is also what
//! HD005 counts. Both rules still report it. They answer different questions
//! and have different fixes: HD001 names the one object that nothing deletes,
//! and HD005 asks the procedure for a pool as a safety net for all of them. A
//! pool does not replace the delete, because a pool frees its objects only
//! when it is deleted, so an object that a loop creates on every pass still
//! grows until then. Once a pool of the procedure holds the object, HD001
//! stops: the pool frees what is left.
//!
//! # Include files
//!
//! A finding in an include is reported in the include, once. An include that
//! ends with the object live may leave the delete to its includer, so the
//! finding there says so and is lower confidence.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_handles::{FileHandles, LeakKind, ObjectKind};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::HD001;
use super::handle_support::{
    describe, includer_may_free, noun, quote, reported_leak, scope, warning, with_fragment_note,
};
use super::syntactic::file_span;

/// Kebab-case rule name.
pub const NAME: &str = "handle-not-deleted";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.handles";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "A dynamic object that some path loses without deleting it";

/// Entry point: analyse the file, then [`check`].
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(&oxabl_handles::analyze_file(program, ctx.source), ctx)
}

/// The findings over an analysed file.
pub fn check(handles: &FileHandles, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for (unit, o) in handles.objects() {
        if matches!(o.kind, ObjectKind::PersistentProcedure | ObjectKind::Memptr)
            || !o.leaks_on_normal_path()
        {
            continue;
        }
        let Some(leak) = reported_leak(o) else {
            continue;
        };
        let holder = o
            .holder
            .as_deref()
            .map_or("The handle".to_string(), |h| format!("`{h}`"));
        let (verb, statement) = if o.kind == ObjectKind::Automation {
            ("Release", "RELEASE OBJECT")
        } else {
            ("Delete", "DELETE OBJECT")
        };
        let help = format!(
            "{verb} the {} with `{statement}` on every path that leaves {}, or move it into a \
             `FINALLY` block. If the caller owns the object, hand it back in an `OUTPUT` \
             parameter.",
            noun(o.kind),
            scope(unit),
        );
        let d = match leak.kind {
            LeakKind::Exit(_) => {
                let mut d = warning(
                    ctx,
                    HD001,
                    format!(
                        "{} is not deleted on every path that leaves {}",
                        describe(o),
                        scope(unit)
                    ),
                    help,
                    o.create.span,
                );
                if leak.at.start < leak.at.end {
                    d = d.with_label(
                        file_span(ctx, leak.at),
                        format!("path ends after `{}`", quote(ctx.source, leak.at)),
                    );
                }
                d
            }
            LeakKind::Overwrite => warning(
                ctx,
                HD001,
                format!(
                    "{holder} is assigned here while it still holds a {}, which nothing deletes",
                    noun(o.kind)
                ),
                help,
                leak.at,
            ),
        };
        diags.push(with_fragment_note(d, includer_may_free(ctx, unit, leak)));
    }
    diags
}
