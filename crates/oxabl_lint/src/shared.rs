//! Facts that one registry run computes once for several rules.

use std::cell::OnceCell;

use oxabl_ast::Statement;
use oxabl_flow::{FileFlow, FlowOptions};
use oxabl_handles::FileHandles;
use oxabl_scope::{FileScopes, ScopeOptions};
use oxabl_semantic::AnalysisContext;

/// What the rules of one [`Registry::run_where`](crate::Registry::run_where)
/// share. Each fact is computed on first use, so a run that enables no rule
/// that needs it pays nothing.
#[derive(Default)]
pub struct RunShared {
    /// The scopes will be analysed: their CFGs of every unit serve the
    /// dynamic objects too.
    share_flow: bool,
    flow: OnceCell<FileFlow>,
    handles: OnceCell<FileHandles>,
    scopes: OnceCell<FileScopes>,
}

impl RunShared {
    /// Facts for a run that analyses the scopes when `scopes` is set.
    pub(crate) fn new(scopes: bool) -> Self {
        RunShared {
            share_flow: scopes,
            ..RunShared::default()
        }
    }

    /// The CFGs of every unit, built once.
    fn flow(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileFlow {
        self.flow.get_or_init(|| {
            oxabl_flow::build_file_tokens(program, ctx.source, ctx.tokens, &FlowOptions::default())
        })
    }

    /// The dynamic objects of the file, analysed once.
    pub fn handles(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileHandles {
        self.handles.get_or_init(|| {
            if self.share_flow {
                let flow = self.flow(program, ctx);
                return oxabl_handles::analyze_flow(program, ctx.source, ctx.tokens, flow);
            }
            oxabl_handles::analyze_tokens(program, ctx.source, ctx.tokens, &FlowOptions::default())
        })
    }

    /// The buffer scopes, transaction blocks and record reads of the file,
    /// analysed once against the context's schema (none when it is not
    /// loaded).
    pub fn scopes(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileScopes {
        self.scopes.get_or_init(|| {
            let options = scope_options(ctx);
            let schema = ctx.schema_loaded.then_some(ctx.schema);
            let flow = self.flow(program, ctx);
            let lexed;
            let tokens = if ctx.tokens.is_empty() {
                lexed = oxabl_lexer::tokenize(ctx.source);
                &lexed
            } else {
                ctx.tokens
            };
            oxabl_scope::analyze_flow(program, ctx.source, tokens, flow, schema, &options)
        })
    }
}

/// The scope options of `ctx`.
fn scope_options<'c>(ctx: &AnalysisContext<'c>) -> ScopeOptions<'c> {
    ScopeOptions {
        parse_recovered: ctx.parse_recovered,
        elided: ctx.elided,
        elided_includes: ctx.elided_includes,
        fragment: ctx.source_context.is_include_fragment(),
        ..ScopeOptions::default()
    }
}

/// [`oxabl_scope::analyze_tokens`] under `ctx`, with its tokens when it has
/// them.
pub(crate) fn analyze_scopes(program: &[Statement], ctx: &AnalysisContext) -> FileScopes {
    let options = scope_options(ctx);
    let schema = ctx.schema_loaded.then_some(ctx.schema);
    if ctx.tokens.is_empty() {
        return oxabl_scope::analyze_file_with(program, ctx.source, schema, &options);
    }
    oxabl_scope::analyze_tokens(program, ctx.source, ctx.tokens, schema, &options)
}
