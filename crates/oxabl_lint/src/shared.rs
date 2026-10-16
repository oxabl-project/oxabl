//! Facts that one registry run computes once for several rules.

use std::cell::{Cell, OnceCell};

use oxabl_ast::Statement;
use oxabl_effects::{EffectOptions, FileEffects};
use oxabl_flow::{FileFlow, FlowOptions};
use oxabl_handles::FileHandles;
use oxabl_index::calls::FileCalls;
use oxabl_nulls::{FileNulls, NullOptions};
use oxabl_scope::{FileScopes, ScopeOptions};
use oxabl_semantic::AnalysisContext;

/// What the rules of one [`Registry::run_where`](crate::Registry::run_where)
/// share. Each fact is computed on first use, so a run that enables no rule
/// that needs it pays nothing.
#[derive(Default)]
pub struct RunShared<'a> {
    summaries: Option<&'a dyn CalleeSummaries>,
    /// The scopes will be analysed: the CFGs of every unit serve the dynamic
    /// objects too.
    share_flow: Cell<bool>,
    handles: OnceCell<FileHandles>,
    tokens: OnceCell<Vec<oxabl_lexer::Token>>,
    flow: OnceCell<FileFlow>,
    scopes: OnceCell<FileScopes>,
    effects: OnceCell<FileEffects>,
    nulls: OnceCell<FileNulls>,
}

/// Answers in expanded byte coordinates. Missing and partial answers are Unknown.
pub trait CalleeSummaries {
    fn callee_summary(&self, at: u32) -> Option<&oxabl_effects::summary::Summary>;
}

impl<'a> RunShared<'a> {
    /// Supply cross-file summaries for this file.
    pub fn with_summaries(mut self, summaries: &'a dyn CalleeSummaries) -> Self {
        self.summaries = Some(summaries);
        self
    }

    /// Note whether the run will analyse the scopes too.
    pub(crate) fn share_flow(&self, scopes: bool) {
        self.share_flow.set(scopes);
    }

    /// The complete or partial summary of the callee at this call site.
    pub fn callee_summary(&self, at: u32) -> Option<&oxabl_effects::summary::Summary> {
        self.summaries?.callee_summary(at)
    }

    /// The dynamic objects of the file, analysed once.
    pub fn handles(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileHandles {
        self.handles.get_or_init(|| {
            if self.share_flow.get() || self.flow.get().is_some() {
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
            oxabl_scope::analyze_flow(
                program,
                ctx.source,
                self.tokens(ctx),
                self.flow(program, ctx),
                schema,
                &options,
            )
        })
    }

    /// The database effects of the file, analysed once over
    /// [`scopes`](Self::scopes).
    pub fn effects(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileEffects {
        self.effects.get_or_init(|| {
            let scopes = self.scopes(program, ctx);
            let tokens = self.tokens(ctx);
            let calls = FileCalls::extract(program, ctx.source, tokens);
            let schema = ctx.schema_loaded.then_some(ctx.schema);
            oxabl_effects::analyze_file(
                program,
                ctx.source,
                tokens,
                self.flow(program, ctx),
                scopes,
                &calls,
                schema,
                &EffectOptions { elided: ctx.elided },
            )
        })
    }

    /// The failed finds of the file and the field references they reach,
    /// analysed once over [`scopes`](Self::scopes).
    pub fn nulls(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileNulls {
        self.nulls.get_or_init(|| {
            let scopes = self.scopes(program, ctx);
            oxabl_nulls::analyze_file(
                ctx.source,
                self.tokens(ctx),
                self.flow(program, ctx),
                scopes,
                &NullOptions { elided: ctx.elided },
            )
            .with_values(
                program,
                self.flow(program, ctx),
                &NullOptions { elided: ctx.elided },
            )
        })
    }

    fn tokens<'s>(&'s self, ctx: &'s AnalysisContext) -> &'s [oxabl_lexer::Token] {
        if !ctx.tokens.is_empty() {
            return ctx.tokens;
        }
        self.tokens
            .get_or_init(|| oxabl_lexer::tokenize(ctx.source))
    }

    fn flow(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileFlow {
        self.flow.get_or_init(|| {
            oxabl_flow::build_file_tokens(
                program,
                ctx.source,
                self.tokens(ctx),
                &scope_options(ctx).flow,
            )
        })
    }
}

fn scope_options<'a>(ctx: &AnalysisContext<'a>) -> ScopeOptions<'a> {
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
