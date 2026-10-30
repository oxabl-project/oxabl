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
    reach: Option<&'a dyn crate::rules::security::reach::SecurityReach>,
    /// Where each include expansion starts, with its path.
    includes: &'a [(u32, std::path::PathBuf)],
    /// The scopes will be analysed: the CFGs of every unit serve the dynamic
    /// objects too.
    share_flow: Cell<bool>,
    handles: OnceCell<FileHandles>,
    tokens: OnceCell<Vec<oxabl_lexer::Token>>,
    calls: OnceCell<FileCalls>,
    flow: OnceCell<FileFlow>,
    scopes: OnceCell<FileScopes>,
    effects: OnceCell<FileEffects>,
    nulls: OnceCell<FileNulls>,
    null_values: OnceCell<FileNulls>,
    null_fields: OnceCell<FileNulls>,
    taint: OnceCell<crate::rules::security::taint::FileTaint>,
    reach_facts: OnceCell<crate::rules::security::reach::FileFacts>,
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

    /// Supply the reachability answers of this file.
    pub fn with_reach(
        mut self,
        reach: &'a dyn crate::rules::security::reach::SecurityReach,
    ) -> Self {
        self.reach = Some(reach);
        self
    }

    /// Supply where each include expansion starts in the expanded text, with
    /// the path it was read from.
    pub fn with_includes(mut self, includes: &'a [(u32, std::path::PathBuf)]) -> Self {
        self.includes = includes;
        self
    }

    /// The reachability answers of this file, when the client has them.
    pub fn reach(&self) -> Option<&'a dyn crate::rules::security::reach::SecurityReach> {
        self.reach
    }

    /// Where each include expansion starts, with its path.
    pub fn includes(&self) -> &'a [(u32, std::path::PathBuf)] {
        self.includes
    }

    /// The reachability facts of the file, read once.
    pub fn reach_facts(
        &self,
        program: &[Statement],
        ctx: &AnalysisContext,
    ) -> &crate::rules::security::reach::FileFacts {
        self.reach_facts.get_or_init(|| {
            crate::rules::security::reach::file_facts(
                program,
                ctx,
                self,
                ctx.lint_severities.security(),
            )
        })
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
            let calls = self.calls(program, ctx);
            let schema = ctx.schema_loaded.then_some(ctx.schema);
            oxabl_effects::analyze_file(
                program,
                ctx.source,
                tokens,
                self.flow(program, ctx),
                scopes,
                calls,
                schema,
                &EffectOptions { elided: ctx.elided },
            )
        })
    }

    /// The calls the file makes and what each one calls, extracted once.
    pub fn calls(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileCalls {
        self.calls
            .get_or_init(|| FileCalls::extract(program, ctx.source, self.tokens(ctx)))
    }

    /// The failed finds of the file and the field references they reach,
    /// analysed once over [`scopes`](Self::scopes).
    pub fn nulls(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileNulls {
        self.nulls.get_or_init(|| {
            oxabl_nulls::analyze_file(
                ctx.source,
                self.tokens(ctx),
                self.flow(program, ctx),
                self.scopes(program, ctx),
                &NullOptions { elided: ctx.elided },
            )
        })
    }

    /// The local value facts of the file and the uses of possible `?`
    /// values, analysed once and apart from [`nulls`](Self::nulls).
    pub fn null_values(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileNulls {
        self.null_values.get_or_init(|| {
            oxabl_nulls::analyze_values(
                ctx.source,
                self.tokens(ctx),
                program,
                self.flow(program, ctx),
                self.scopes(program, ctx),
                &NullOptions { elided: ctx.elided },
            )
        })
    }

    /// The uses of database fields the loaded schema starts at `?`, analysed
    /// once and apart from [`null_values`](Self::null_values). Empty when no
    /// schema is loaded.
    pub fn null_fields(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileNulls {
        self.null_fields.get_or_init(|| {
            if !ctx.schema_loaded {
                return FileNulls::default();
            }
            oxabl_nulls::analyze_fields(
                ctx.source,
                self.tokens(ctx),
                program,
                self.flow(program, ctx),
                &oxabl_nulls::FieldSources {
                    scopes: self.scopes(program, ctx),
                    schema: ctx.schema,
                },
                &NullOptions { elided: ctx.elided },
            )
        })
    }

    pub fn tokens<'s>(&'s self, ctx: &'s AnalysisContext) -> &'s [oxabl_lexer::Token] {
        if !ctx.tokens.is_empty() {
            return ctx.tokens;
        }
        self.tokens
            .get_or_init(|| oxabl_lexer::tokenize(ctx.source))
    }

    /// The taint facts of the file, analysed once under the context's
    /// security options.
    pub(crate) fn taint(
        &self,
        program: &[Statement],
        ctx: &AnalysisContext,
    ) -> &crate::rules::security::taint::FileTaint {
        self.taint.get_or_init(|| {
            crate::rules::security::taint::analyze(program, ctx, self.flow(program, ctx))
        })
    }

    /// The control-flow graphs of the file's units, built once.
    pub fn flow(&self, program: &[Statement], ctx: &AnalysisContext) -> &FileFlow {
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

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_common::FileId;

    #[test]
    fn the_failed_finds_and_the_value_facts_are_computed_apart() {
        let source = "DEFINE VARIABLE d AS DATE NO-UNDO.\nIF d > TODAY THEN MESSAGE 1.\n";
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        let schema = oxabl_schema::Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);

        let shared = RunShared::default();
        shared.nulls(&program.statements, &ctx);
        assert!(shared.null_values.get().is_none());

        let shared = RunShared::default();
        assert_eq!(
            shared.null_values(&program.statements, &ctx).values().len(),
            1
        );
        assert!(shared.nulls.get().is_none());
    }
}
