//! What the call rules (CL003 and CL004) share: the static `RUN` statements of
//! a file with the call each one makes.

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_index::calls::{CallSite, FileCalls};
use oxabl_semantic::AnalysisContext;

use super::syntactic::{Visitor, walk_block};

/// Call `f` for every `RUN` statement of `program` that the call graph lists,
/// with its call.
pub(super) fn for_each_run(
    program: &[Statement],
    calls: &FileCalls,
    f: impl FnMut(&Statement, &CallSite),
) {
    struct Runs<'a, F> {
        calls: &'a FileCalls,
        f: F,
    }
    impl<F: FnMut(&Statement, &CallSite)> Visitor for Runs<'_, F> {
        fn statement(&mut self, stmt: &Statement) -> bool {
            if matches!(stmt.kind, StatementKind::Run { .. })
                && let Ok(i) = self
                    .calls
                    .sites
                    .binary_search_by_key(&(stmt.span.start, stmt.span.end), |s| {
                        (s.at.start, s.at.end)
                    })
            {
                (self.f)(stmt, &self.calls.sites[i]);
            }
            true
        }
    }
    walk_block(&mut Runs { calls, f }, program);
}

/// Whether the preprocessor left a gap inside `span`: the compiler may see
/// text there.
pub(super) fn holds_elided(ctx: &AnalysisContext, span: Span) -> bool {
    let from = ctx.elided.partition_point(|&at| at < span.start);
    ctx.elided.get(from).is_some_and(|&at| at <= span.end)
}

/// A `RUN` target with no path or extension: it may be an internal procedure
/// that the file's includer or a super procedure supplies.
pub(super) fn is_bare(name: &str) -> bool {
    !name.contains(['.', '/', '\\'])
}
