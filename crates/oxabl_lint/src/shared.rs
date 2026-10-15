//! Facts that one registry run computes once for several rules.

use std::cell::OnceCell;

use oxabl_ast::Statement;
use oxabl_handles::FileHandles;
use oxabl_scope::{FileScopes, ScopeOptions};
use oxabl_semantic::AnalysisContext;

/// What the rules of one [`Registry::run_where`](crate::Registry::run_where)
/// share. Each fact is computed on first use, so a run that enables no rule
/// that needs it pays nothing.
#[derive(Default)]
pub struct RunShared {
    handles: OnceCell<FileHandles>,
    scopes: OnceCell<FileScopes>,
}

impl RunShared {
    /// The dynamic objects of the file, analysed once. `source` is the text
    /// `program` was parsed from.
    pub fn handles(&self, program: &[Statement], source: &str) -> &FileHandles {
        self.handles
            .get_or_init(|| oxabl_handles::analyze_file(program, source))
    }

    /// The buffer scopes, transaction blocks and record reads of the file,
    /// analysed once against the context's schema (none when it is not
    /// loaded).
    pub fn scopes(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileScopes {
        self.scopes.get_or_init(|| analyze_scopes(program, ctx))
    }
}

/// [`oxabl_scope::analyze_tokens`] under `ctx`, with its tokens when it has
/// them.
pub(crate) fn analyze_scopes(program: &[Statement], ctx: &AnalysisContext) -> FileScopes {
    let options = ScopeOptions {
        parse_recovered: ctx.parse_recovered,
        elided: ctx.elided,
        elided_includes: ctx.elided_includes,
        fragment: ctx.source_context.is_include_fragment(),
        ..ScopeOptions::default()
    };
    let schema = ctx.schema_loaded.then_some(ctx.schema);
    if ctx.tokens.is_empty() {
        return oxabl_scope::analyze_file_with(program, ctx.source, schema, &options);
    }
    oxabl_scope::analyze_tokens(program, ctx.source, ctx.tokens, schema, &options)
}
