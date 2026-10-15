//! `shared-state` lint (CL005), group `strict.calls`.
//!
//! Fires on every definition that creates or binds shared state: the `NEW
//! SHARED`, `NEW GLOBAL SHARED` and `SHARED` forms of `DEFINE`.
//!
//! # What counts
//!
//! `DEFINE [NEW [GLOBAL]] SHARED` accepts: `VARIABLE`, `TEMP-TABLE`, `BUFFER`,
//! `DATASET`, `QUERY`, `BROWSE`, `FRAME`, `STREAM`, `MENU`, `SUB-MENU`,
//! `BUTTON`, `IMAGE`, `RECTANGLE` and the legacy `WORK-TABLE` / `WORKFILE`.
//! All of them are reported. One diagnostic per definition, anchored on the
//! defined name when the parser models the form (variables, temp-tables,
//! buffers, datasets, streams, frames) and on the `DEFINE ... <kind>` words for
//! the rest. The message says which of the three modes was written:
//!
//! - `NEW SHARED` — the producer; its callees see the object.
//! - `NEW GLOBAL SHARED` — the producer, visible to the whole session.
//! - `SHARED` — a consumer; it binds an object some caller created and has to
//!   repeat its definition exactly.
//!
//! # Why
//!
//! A shared object is a hidden parameter. Nothing in a procedure's signature
//! says it reads or writes one, the caller and the callee have to agree on a
//! definition that is written out twice, and a call from anywhere else
//! compiles and then fails or misbehaves at run time. It also makes the call
//! graph matter for correctness: whether a read sees data depends on who
//! called it.
//!
//! ```abl
//! /* caller */
//! DEFINE NEW SHARED VARIABLE gCompany AS CHARACTER NO-UNDO.
//! RUN price-order.p.
//!
//! /* callee */
//! DEFINE SHARED VARIABLE gCompany AS CHARACTER NO-UNDO.
//! ```
//!
//! Fixed — the dependency is in the signature:
//!
//! ```abl
//! /* callee */
//! DEFINE INPUT PARAMETER pCompany AS CHARACTER NO-UNDO.
//! ```
//!
//! A shared temp-table becomes a `TABLE` or `TABLE-HANDLE` parameter, and
//! shared state that really is session-wide becomes a property of a class.
//!
//! # Where the rule looks
//!
//! The parser records the three modes on variables, temp-tables, buffers and
//! datasets. It does not on streams, frames or the forms it skips
//! (`QUERY`, `BROWSE`, `MENU`, `BUTTON`, ...), so for those the rule lexes the
//! first words of the statement's source text. A `DEFINE` whose modifiers are
//! hidden behind a comment or an unexpanded include argument is therefore not
//! seen when it is one of those forms.

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::CL005;
use super::syntactic::{Visitor, file_span, leading_tokens, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "shared-state";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.calls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "NEW SHARED and SHARED definitions pass state outside the signature";

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = SharedDefinitions {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

#[derive(Clone, Copy)]
enum Mode {
    NewShared,
    NewGlobalShared,
    Shared,
}

impl Mode {
    fn from_flags(new_shared: bool, shared: bool, new_global_shared: bool) -> Option<Mode> {
        match (new_global_shared, new_shared, shared) {
            (true, _, _) => Some(Mode::NewGlobalShared),
            (_, true, _) => Some(Mode::NewShared),
            (_, _, true) => Some(Mode::Shared),
            _ => None,
        }
    }

    fn keywords(self) -> &'static str {
        match self {
            Mode::NewShared => "NEW SHARED",
            Mode::NewGlobalShared => "NEW GLOBAL SHARED",
            Mode::Shared => "SHARED",
        }
    }
}

struct SharedDefinitions<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl SharedDefinitions<'_> {
    fn report(&mut self, span: Span, mode: Mode, noun: &str, name: Option<&str>) {
        let message = match name {
            Some(name) => format!("`{}` {noun} `{name}` is shared state", mode.keywords()),
            None => format!("`{}` {noun} is shared state", mode.keywords()),
        };
        self.diags.push(
            Diagnostic::warning(CL005, message, file_span(self.ctx, span)).with_help(
                "Pass the value as a parameter, or keep it as a property of a class.".to_string(),
            ),
        );
    }

    /// A `DEFINE` the parser keeps no `SHARED` flag for: read the modifiers
    /// from the statement's own tokens.
    fn report_from_tokens(&mut self, stmt: &Statement, name: Option<(&str, Span)>) {
        let tokens = leading_tokens(self.ctx.source, stmt.span, 12);
        let Some((define, _)) = tokens.first() else {
            return;
        };
        if define.kind != Kind::Define {
            return;
        }
        let mut i = 1;
        let skip_refs = |i: &mut usize| {
            while tokens
                .get(*i)
                .is_some_and(|(t, _)| matches!(t.kind, Kind::Preprop | Kind::IncludeArgReference))
            {
                *i += 1;
            }
        };
        skip_refs(&mut i);
        // Same grammar as the parser: `NEW` must lead into `SHARED` or `GLOBAL`.
        let kind_at = |i: usize| tokens.get(i).map(|(t, _)| t.kind);
        let mode = if kind_at(i) == Some(Kind::New)
            && matches!(kind_at(i + 1), Some(Kind::Shared | Kind::Global))
        {
            i += 1;
            let global = kind_at(i) == Some(Kind::Global);
            if global {
                i += 1;
            }
            if kind_at(i) == Some(Kind::Shared) {
                i += 1;
            }
            if global {
                Mode::NewGlobalShared
            } else {
                Mode::NewShared
            }
        } else if kind_at(i) == Some(Kind::Shared) {
            i += 1;
            Mode::Shared
        } else {
            return;
        };
        skip_refs(&mut i);
        let Some((noun_token, noun)) = tokens.get(i) else {
            return;
        };
        let noun = noun.to_ascii_lowercase();
        match name {
            Some((name, span)) => self.report(span, mode, &noun, Some(name)),
            None => {
                let start = stmt.span.start;
                let end = start + noun_token.end as u32;
                self.report(Span { start, end }, mode, &noun, None);
            }
        }
    }
}

impl Visitor for SharedDefinitions<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        match &stmt.kind {
            StatementKind::VariableDeclaration {
                name,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => self.report_flags(
                name,
                "variable",
                *is_new_shared,
                *is_shared,
                *is_new_global_shared,
            ),
            StatementKind::DefineTempTable { name, .. } if self.is_workfile(stmt) => {
                self.report_from_tokens(stmt, Some((&name.name, name.span)));
            }
            StatementKind::DefineTempTable {
                name,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => self.report_flags(
                name,
                "temp-table",
                *is_new_shared,
                *is_shared,
                *is_new_global_shared,
            ),
            StatementKind::DefineBuffer {
                name,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => self.report_flags(
                name,
                "buffer",
                *is_new_shared,
                *is_shared,
                *is_new_global_shared,
            ),
            StatementKind::DefineDataset {
                name,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => self.report_flags(
                name,
                "dataset",
                *is_new_shared,
                *is_shared,
                *is_new_global_shared,
            ),
            StatementKind::DefineStream { name }
            | StatementKind::DefineFrame { name, .. }
            | StatementKind::DefineWidget { name } => {
                self.report_from_tokens(stmt, Some((&name.name, name.span)));
            }
            StatementKind::Skipped { .. } => self.report_from_tokens(stmt, None),
            _ => {}
        }
        true
    }
}

impl SharedDefinitions<'_> {
    /// The parser models `WORKFILE` / `WORK-TABLE` as a temp-table; the
    /// keyword is only in the tokens.
    fn is_workfile(&self, stmt: &Statement) -> bool {
        leading_tokens(self.ctx.source, stmt.span, 8)
            .iter()
            .any(|(t, _)| matches!(t.kind, Kind::Workfile | Kind::WorkTable))
    }

    fn report_flags(
        &mut self,
        name: &oxabl_ast::Identifier,
        noun: &str,
        new_shared: bool,
        shared: bool,
        new_global_shared: bool,
    ) {
        if let Some(mode) = Mode::from_flags(new_shared, shared, new_global_shared) {
            self.report(name.span, mode, noun, Some(&name.name));
        }
    }
}
