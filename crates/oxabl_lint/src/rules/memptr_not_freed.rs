//! `memptr-not-freed` lint (HD004), group `strict.handles`.
//!
//! Fires on `SET-SIZE(m) = n`, with `n` not the literal 0, or on
//! `COPY-LOB ... TO m`, when no path that raises nothing runs `SET-SIZE(m) = 0`
//! afterwards.
//!
//! # Why
//!
//! A MEMPTR owns a block of native memory. Setting its size allocates the
//! block, and only `SET-SIZE(m) = 0` gives it back. The garbage collector does
//! not know about the block, so a MEMPTR that goes out of scope with a size
//! leaks its memory until the session ends.
//!
//! ```abl
//! DEFINE VARIABLE mBuffer AS MEMPTR NO-UNDO.
//! SET-SIZE(mBuffer) = 4096.
//! COPY-LOB FROM FILE cName TO mBuffer.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE mBuffer AS MEMPTR NO-UNDO.
//! SET-SIZE(mBuffer) = 4096.
//! COPY-LOB FROM FILE cName TO mBuffer.
//! FINALLY:
//!     SET-SIZE(mBuffer) = 0.
//! END FINALLY.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per `SET-SIZE(m) = n` statement, on that statement. A
//! `SET-SIZE` on a MEMPTR that already has a size raises an error and frees
//! nothing, so it is not a second allocation.
//!
//! Not reported:
//!
//! - a MEMPTR that is set to size 0 on every path that raises nothing;
//! - a MEMPTR that leaves the procedure: an `OUTPUT` parameter, a class member,
//!   a shared or outer variable, a field;
//! - a size set to the literal 0, which is the free itself;
//! - a MEMPTR passed to a callee whose code the file does not show (a method
//!   that a subclass can override, `RUN other.p`), or as an `OUTPUT` argument;
//! - a loss only on a path that raises an error (`delete-not-in-finally`,
//!   HD002, covers a free that exists);
//! - a file the parser had to recover from.
//!
//! A MEMPTR passed `INPUT` to an `EXTERNAL` library procedure is only used:
//! the library does not free it, so it is still reported.
//!
//! Assigning a MEMPTR copies the pointer, not the memory, so a free through
//! either variable counts. `SET-SIZE` allocates, and so does `COPY-LOB ... TO m`
//! when the `m` it names is declared `AS MEMPTR` and no earlier `SET-SIZE` or
//! `COPY-LOB` of `m` always runs first. Other statements that fill a MEMPTR are
//! not tracked.
//!
//! # Include files
//!
//! A finding in an include is reported in the include, once. An include that
//! ends with the MEMPTR live may leave the free to its includer, so the finding
//! there says so and is lower confidence.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_handles::{FileHandles, ObjectKind};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::HD004;
use super::handle_support::{
    includer_may_free, not_judged, quote, reported_leak, warning, with_fragment_note,
};

/// Kebab-case rule name.
pub const NAME: &str = "memptr-not-freed";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.handles";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "A MEMPTR given a size and never set back to 0";

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
        if o.kind != ObjectKind::Memptr || !o.leaks_on_normal_path() {
            continue;
        }
        let Some(leak) = reported_leak(o) else {
            continue;
        };
        let holder = o.holder.as_deref().unwrap_or("the MEMPTR");
        let d = warning(
            ctx,
            HD004,
            format!(
                "`{}` allocates memory that is not freed on every path",
                quote(ctx.source, o.create.span)
            ),
            format!(
                "Run `SET-SIZE({holder}) = 0` when you finish with the memory, on every path (a \
                 `FINALLY` block covers errors)."
            ),
            o.create.span,
        );
        diags.push(with_fragment_note(d, includer_may_free(ctx, unit, leak)));
    }
    diags
}
