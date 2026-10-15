//! `persistent-procedure-leak` lint (HD003), group `strict.handles`.
//!
//! Fires on `RUN prog.p PERSISTENT SET h` when the procedure that `h` names is
//! never deleted: no path of the procedure runs `DELETE PROCEDURE h` or
//! `APPLY "CLOSE" TO h`, and the handle does not leave to an owner.
//!
//! # Why
//!
//! A persistent procedure keeps its variables, its temp-tables and its
//! triggers until someone deletes it. It is not collected. A caller that starts
//! one on every call and keeps no handle to delete it adds one instance per
//! call, and every instance runs its own main block.
//!
//! ```abl
//! DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
//! RUN pricing-lib.p PERSISTENT SET hLib.
//! RUN calc-price IN hLib (INPUT iOrder, OUTPUT dPrice).
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
//! RUN pricing-lib.p PERSISTENT SET hLib.
//! RUN calc-price IN hLib (INPUT iOrder, OUTPUT dPrice).
//! FINALLY:
//!     IF VALID-HANDLE(hLib) THEN DELETE PROCEDURE hLib.
//! END FINALLY.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per `RUN ... PERSISTENT SET h` on the `RUN`. The loss must
//! happen on a path that raises nothing.
//!
//! Not reported:
//!
//! - `RUN prog.p PERSISTENT` without `SET`: it keeps no handle here, which is
//!   the usual form for a singleton or a procedure that manages itself;
//! - a handle that is deleted on every path, passed back in an `OUTPUT`
//!   parameter, stored in a shared or outer variable, a class member or a
//!   field, or given to `ADD-SUPER-PROCEDURE`;
//! - a handle that the procedure passes to `RUN destroy... IN h` (any name that
//!   starts with `destroy`, or `RUN dispatch IN h ('destroy')`): by convention
//!   the procedure deletes itself there, so the state is unknown;
//! - a procedure that sweeps `FIRST-PROCEDURE` / `NEXT-SIBLING`, which may
//!   delete any persistent procedure.
//!
//! A persistent procedure is not in a widget pool of its caller, so
//! `missing-widget-pool` (HD005) does not count it either.
//!
//! # Include files
//!
//! A finding in an include is reported in the include, once. An include that
//! ends with the procedure live may leave the delete to its includer, so the
//! finding there says so and is lower confidence.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_handles::{FileHandles, ObjectKind};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::HD003;
use super::handle_support::{
    includer_may_free, not_judged, quote, reported_leak, warning, with_fragment_note,
};

/// Kebab-case rule name.
pub const NAME: &str = "persistent-procedure-leak";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.handles";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "A persistent procedure that nothing deletes";

/// Entry point: analyse the file, then [`check`].
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(&oxabl_handles::analyze_file(program, ctx.source), ctx)
}

/// The findings over an analysed file.
pub fn check(handles: &FileHandles, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    if not_judged(ctx) {
        return Vec::new();
    }
    let mut diags = Vec::new();
    for (unit, o) in handles.objects() {
        if o.kind != ObjectKind::PersistentProcedure || !o.leaks_on_normal_path() {
            continue;
        }
        let Some(leak) = reported_leak(o) else {
            continue;
        };
        let holder = o.holder.as_deref().unwrap_or("the handle");
        let d = warning(
            ctx,
            HD003,
            format!(
                "`{}` starts a persistent procedure that nothing deletes",
                quote(ctx.source, o.create.span)
            ),
            format!(
                "Run `DELETE PROCEDURE {holder}` when you finish with the procedure, on every \
                 path (a `FINALLY` block covers errors). If the caller owns it, hand the handle \
                 back in an `OUTPUT` parameter."
            ),
            o.create.span,
        );
        diags.push(with_fragment_note(d, includer_may_free(ctx, unit, leak)));
    }
    diags
}
