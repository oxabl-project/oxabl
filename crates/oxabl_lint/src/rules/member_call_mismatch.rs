//! `member-call-mismatch` lint (CL006), group `strict.calls`.
//!
//! Fires on a call of a method of a built-in type, or a `NEW` of a built-in
//! class, that no overload can take: no overload has the number of
//! arguments, or each overload with that number has an argument whose type
//! never converts to its parameter.
//!
//! # Why
//!
//! ```abl
//! DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.
//! oErr = NEW Progress.Lang.AppError("Order not found", 0).
//! MESSAGE oErr:GetMessage().
//! ```
//!
//! `GetMessage` takes the index of the message. The call does not compile.
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.
//! oErr = NEW Progress.Lang.AppError("Order not found", 0).
//! MESSAGE oErr:GetMessage(1).
//! ```
//!
//! # What is and is not reported
//!
//! The overloads come from the catalog of the OpenEdge documentation
//! (`docs/design/builtin-classes.md`). The catalog has errors, so the rule
//! judges only a type whose whole surface the catalog knows, and only a
//! receiver whose type is that built-in type (a user class that inherits one
//! is not judged). The catalog has no parameter modes, so an argument type
//! removes an overload only when it converts to the parameter in no mode: a
//! `CHARACTER` where an `INTEGER` is wanted, a class where a primitive is
//! wanted. A narrowing, `?`, an argument whose type oxabl does not know, and a
//! pair of classes give no finding.
//!
//! Never reported: a call with an `{include}` or a preprocessor reference
//! among its arguments, or text the preprocessor removed; a call in an
//! include analysed alone; `SUPER(...)` and `THIS-OBJECT(...)` in a
//! constructor. A set with static and instance overloads is judged only by
//! the count.

use oxabl_ast::{Expression, ExpressionKind, Statement};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, MisfitKind, ResolvedType, Semantic, builtin_call_misfit};

use super::CL006;
use super::syntactic::{Visitor, file_span, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "member-call-mismatch";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.calls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A call of a built-in method, or a NEW of a built-in class, that no overload takes";

/// Entry point.
pub fn run(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = Calls {
        sem,
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

struct Calls<'a> {
    sem: &'a Semantic,
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl Visitor for Calls<'_> {
    fn expression(&mut self, expr: &Expression) {
        if !matches!(
            expr.kind,
            ExpressionKind::MethodCall { .. } | ExpressionKind::New { .. }
        ) {
            return;
        }
        let Some(misfit) = builtin_call_misfit(self.sem, self.ctx, expr) else {
            return;
        };
        let what = match misfit.member {
            Some(member) => format!("no overload of `{}:{member}`", misfit.class.name),
            None => format!("no constructor of `{}`", misfit.class.name),
        };
        let message = match &misfit.kind {
            MisfitKind::Count(1) => format!("{what} takes 1 argument"),
            MisfitKind::Count(n) => format!("{what} takes {n} arguments"),
            MisfitKind::Types(types) => {
                let types: Vec<String> = types
                    .iter()
                    .map(|t| match t {
                        ResolvedType::Unknown | ResolvedType::Error => "`?`".to_string(),
                        t => format!("`{}`", t.display_abl(&self.sem.symbols, self.ctx.schema)),
                    })
                    .collect();
                format!(
                    "{what} takes arguments of these types: {}",
                    types.join(", ")
                )
            }
        };
        self.diags.push(
            Diagnostic::warning(CL006, message, file_span(self.ctx, misfit.span)).with_help(
                "Check the parameters of the method in the Progress documentation.".to_string(),
            ),
        );
    }
}
