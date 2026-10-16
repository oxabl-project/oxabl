//! `unresolved-call` lint (CL004), group `strict.calls`.
//!
//! Fires on a static `RUN name` that nothing in the workspace can run: `name`
//! is not an internal procedure of the file, no program of that name is on the
//! search paths, and no file of the workspace defines a procedure of that name.
//!
//! # Why
//!
//! ```abl
//! DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
//! RUN post-invoice (INPUT iOrder).
//! ```
//!
//! `post-invoice` is neither a procedure of this file nor a program the
//! workspace holds. The compiler does not check a `RUN` of another program, so
//! a typo, a renamed program or a missing directory in the search path is a
//! run-time error, found when that line runs.
//!
//! Fixed — the procedure is in the file:
//!
//! ```abl
//! DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
//! PROCEDURE post-invoice:
//!     DEFINE INPUT PARAMETER iOrder AS INTEGER NO-UNDO.
//! END PROCEDURE.
//! RUN post-invoice (INPUT iOrder).
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per `RUN`, anchored on the target. The call graph
//! ([`oxabl_index::calls`]) must say the target is `not-found`: the index
//! searched every configured path and nothing supplies the name. These are
//! never reported, because the callee is not known to be absent:
//!
//! - `RUN VALUE(...)`, `RUN p IN h`, `RUN SUPER`, and targets that still hold
//!   preprocessor text (the `dynamic`, `handle`, `super` and `unexpanded`
//!   reasons);
//! - a name that starts with `..`: it is relative to a directory the search
//!   paths do not cover;
//! - a name that several files match, or a file that exists and cannot be
//!   read or parsed (`ambiguous`, `unusable`);
//! - `RUN ... ASYNCHRONOUS` and `RUN ... ON SERVER h`: the program may exist on
//!   the other side only;
//! - a run with no index, or an index with no search path: nothing was
//!   searched;
//! - in an include checked alone, and in a file with an include that did not
//!   arrive, a bare name: it may be a procedure of the includer;
//! - in a file that binds a web service port type (`RUN port-type SET h ON
//!   SERVER s`), a bare name: it is an operation of the service;
//! - a path into a directory the OpenEdge install ships (`adecomm/`, `adeshar/`,
//!   `prodict/`, ...): a workspace that does not configure the install cannot
//!   supply it.
//!
//! A bare name (no extension, no path) may also be a procedure of a super
//! procedure or of a persistent procedure, and a plain `RUN name` reaches it
//! at run time. The rule reports a bare name only when the index lists the
//! files of the workspace and none of them defines a `PROCEDURE` of that name,
//! as written in the file or in an include file that a file of the workspace
//! names. A run whose index has no such list reports no bare name.
//!
//! # `LINT0001` already reports some of these
//!
//! `undefined-symbol` (`LINT0001`, always on) reports a `RUN` target that
//! names a file (`RUN post-invoice.p`) when no search path supplies it. This
//! rule does not report that case again. It reports the targets `LINT0001`
//! leaves out: a name with no extension (`RUN post-invoice`), and a name with
//! a path and no extension. A `RUN` is never reported by both rules.

use oxabl_ast::{RunTarget, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_index::calls::{
    CallKind, CallTarget, FileCalls, ProgramAnswer, UnknownCall, resolve_program,
};
use oxabl_lexer::{Kind, Token};
use oxabl_semantic::{
    AnalysisContext, IndexName, Semantic, run_miss_is_reported, run_target_climbs_out,
};

use super::CL004;
use super::call_support::{for_each_run, holds_elided, is_bare};
use super::syntactic::file_span;
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "unresolved-call";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.calls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A RUN of a name that is neither a procedure of the file nor a program on the search paths";

/// Entry point without shared facts.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(program, ctx, &RunShared::default())
}

/// Shared entry point.
pub fn check(program: &[Statement], ctx: &AnalysisContext, shared: &RunShared) -> Vec<Diagnostic> {
    if !ctx.index_loaded || !ctx.index_searches_paths || ctx.parse_recovered {
        return Vec::new();
    }
    let fragment = ctx.source_context.is_include_fragment();
    let calls = shared.calls(program, ctx);
    let port_type = binds_port_type(calls, shared.tokens(ctx));
    let mut diags = Vec::new();
    for_each_run(program, calls, |stmt, site| {
        let CallTarget::Program(name) = &site.target else {
            return;
        };
        if !matches!(site.kind, CallKind::Run | CallKind::Persistent) {
            return;
        }
        // `LINT0001` reports a missing file; this rule reports the rest.
        if run_miss_is_reported(name) || run_target_climbs_out(name) {
            return;
        }
        if is_bare(name) && (fragment || port_type || !ctx.elided_includes.is_empty()) {
            return;
        }
        if shipped_library(name) {
            return;
        }
        if holds_elided(ctx, stmt.span) {
            return;
        }
        let StatementKind::Run {
            target: RunTarget::Literal { name_span, .. },
            ..
        } = &stmt.kind
        else {
            return;
        };
        // The parser reads a name up to a digit after a slash and stops
        // (`RUN dir/870-recon.p` reads `dir/870`); the text that follows is part of it.
        let mut rest = ctx
            .source
            .get(name_span.end as usize..)
            .unwrap_or("")
            .chars();
        let continues = match rest.next() {
            Some('.') => rest.next().is_some_and(char::is_alphanumeric),
            Some(c) => c.is_alphanumeric() || matches!(c, '-' | '_' | '/' | '\\'),
            None => false,
        };
        if continues {
            return;
        }
        if resolve_program(ctx.index, name) != ProgramAnswer::Unknown(UnknownCall::NotFound) {
            return;
        }
        // A super or persistent procedure may supply a bare name.
        if is_bare(name)
            && ctx.index.workspace_defines_procedure(&IndexName::new(name)) != Some(false)
        {
            return;
        }
        diags.push(
            Diagnostic::warning(
                CL004,
                format!(
                    "`RUN {name}` names no procedure of this file and no program on the search paths"
                ),
                file_span(ctx, *name_span),
            )
            .with_help(
                "Check the name. If it is a program, give it its extension or add its directory \
                 to `[workspace.sources].include_paths`; if a super procedure supplies it, run it \
                 `IN` the handle."
                    .to_string(),
            ),
        );
    });
    diags
}

/// Directories of the OpenEdge install that programs run by path
/// (`RUN adecomm/_osprint.p`). The install is on the search path of a
/// deployment, and a workspace that does not configure it cannot supply them.
const SHIPPED_DIRECTORIES: [&str; 11] = [
    "adecomm", "adeedit", "adeshar", "adeuib", "adeicon", "adeweb", "adetran", "adm", "adm2",
    "prodict", "protools",
];

fn shipped_library(name: &str) -> bool {
    name.split_once(['/', '\\']).is_some_and(|(dir, _)| {
        SHIPPED_DIRECTORIES
            .iter()
            .any(|d| d.eq_ignore_ascii_case(dir))
    })
}

/// Whether the file binds a web service port type: `RUN port-type SET h ON
/// SERVER s`, with no `PERSISTENT`, which starts an AppServer program instead.
/// The operations of that port type are then run by their bare names, and
/// nothing in the workspace defines them.
fn binds_port_type(calls: &FileCalls, tokens: &[Token]) -> bool {
    calls
        .sites
        .iter()
        .filter(|site| site.kind == CallKind::Remote)
        .any(|site| {
            let from = tokens.partition_point(|t| t.start < site.at.start as usize);
            let mut words = tokens[from..]
                .iter()
                .take_while(|t| t.end <= site.at.end as usize)
                .map(|t| t.kind);
            let kinds: Vec<Kind> = words.by_ref().collect();
            kinds.contains(&Kind::Set) && !kinds.contains(&Kind::Persistent)
        })
}
