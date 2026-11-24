//! `dynamic-invoke` lint (CL002), group `strict.calls`.
//!
//! Fires where code calls something whose target is a string or a handle
//! resolved at run time, rather than a name the compiler checked.
//!
//! # The set
//!
//! | Form | Why it is in |
//! |---|---|
//! | `DYNAMIC-FUNCTION(name ...)` (and the `DYNAMIC-FUNC` abbreviation) | calls a function chosen by string, optionally `IN handle` |
//! | `DYNAMIC-INVOKE(target, name ...)` | calls a method or procedure chosen by string |
//! | `DYNAMIC-NEW class (...)` | instantiates a class chosen by string |
//! | `DYNAMIC-PROPERTY(obj, name)` | reads or writes a property chosen by string |
//! | `CREATE CALL handle` | the `CALL` object: the target is set by assigning `CALL-NAME` and the signature by the `NUM-PARAMETERS` and `SET-PARAMETER` calls that follow |
//!
//! Each shares one property: the callee is not in the source text, so a name
//! search, a call graph and a parameter check all miss it. `RUN VALUE(...)`
//! has the same property and has its own rule, `run-value` (CL001).
//!
//! Left out on purpose: `DYNAMIC-CAST` checks a type and calls nothing;
//! `DYNAMIC-CURRENT-VALUE` and `DYNAMIC-NEXT-VALUE` pick a *sequence* by name
//! and are data access, not dispatch; `DYNAMIC-ENUM` looks up a constant.
//! Reflection through `Progress.Lang.Class` (`:Invoke`, `:New`) and
//! `Progress.Reflect.Method` is also left out: a method call named `Invoke`
//! cannot be told from a user method of the same name without type
//! information.
//!
//! `CREATE CALL` is the one place a `CALL` object is born, so it stands in for
//! the `:INVOKE()` that eventually runs it. A call handle that arrives as a
//! parameter, and is invoked there without a `CREATE CALL`, is not seen; that
//! needs type information the rule does not use.
//!
//! # Why
//!
//! ```abl
//! cResult = DYNAMIC-FUNCTION("calc-" + cKind, iOrder).
//! ```
//!
//! Which function runs depends on a string built at run time. A typo is a
//! run-time error, a rename misses it, and the call is invisible to every
//! analysis that follows calls.
//!
//! Fixed — the target is named and checked at compile time:
//!
//! ```abl
//! CASE cKind:
//!     WHEN "tax"  THEN cResult = calc-tax(iOrder).
//!     WHEN "duty" THEN cResult = calc-duty(iOrder).
//! END CASE.
//! ```
//!
//! # Where the rule looks
//!
//! Expressions in every modelled statement, `CREATE CALL`, and — by lexing the
//! statement's source text, because the parser keeps no expression tree there —
//! statements the parser recognizes but does not model
//! (`StatementKind::Skipped`: `PUT`, `EXPORT`, `UPDATE`, `OPEN QUERY`, ...). A
//! dynamic call inside a clause that a *modelled* statement discards (a
//! `FORMAT` or `LABEL` expression, say) is not seen.

use oxabl_ast::{
    CreateTarget, CreateTargetKind, Expression, ExpressionKind, Span, Statement, StatementKind,
};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::{Kind, tokenize};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::CL002;
use super::syntactic::{Visitor, file_span, source_of, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "dynamic-invoke";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.calls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "DYNAMIC-FUNCTION, DYNAMIC-INVOKE, DYNAMIC-NEW, DYNAMIC-PROPERTY and CALL objects are resolved at run time";

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = DynamicCalls {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

/// The canonical spelling of a dynamic-call keyword, or `None`.
///
/// `DYNAMIC-FUNCTION` may be abbreviated down to `DYNAMIC-FUNC`.
pub(crate) fn dynamic_name(text: &str) -> Option<&'static str> {
    const FUNCTION: &str = "DYNAMIC-FUNCTION";
    if text.len() >= "DYNAMIC-FUNC".len()
        && text.len() <= FUNCTION.len()
        && FUNCTION[..text.len()].eq_ignore_ascii_case(text)
    {
        return Some(FUNCTION);
    }
    ["DYNAMIC-INVOKE", "DYNAMIC-NEW", "DYNAMIC-PROPERTY"]
        .into_iter()
        .find(|name| name.eq_ignore_ascii_case(text))
}

struct DynamicCalls<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl DynamicCalls<'_> {
    fn report(&mut self, span: Span, what: &str) {
        self.diags.push(
            Diagnostic::warning(
                CL002,
                format!("`{what}` is a dynamic call, resolved at run time"),
                file_span(self.ctx, span),
            )
            .with_help(
                "Call the function or method by name so the compiler and analysis can check it."
                    .to_string(),
            ),
        );
    }

    /// Lex a skipped statement and report each dynamic-call keyword in it.
    fn scan_skipped(&mut self, stmt: &Statement) {
        let Some(text) = source_of(self.ctx.source, stmt.span) else {
            return;
        };
        let tokens = tokenize(text);
        for (i, tok) in tokens.iter().enumerate() {
            if !matches!(
                tok.kind,
                Kind::Identifier | Kind::DynamicFunction | Kind::DynamicInvoke | Kind::DynamicNew
            ) {
                continue;
            }
            let Some(name) = dynamic_name(&text[tok.start..tok.end]) else {
                continue;
            };
            // DYNAMIC-NEW is reserved; the others must be called to count, since
            // a field may carry the same name.
            let called = tokens.get(i + 1).is_some_and(|t| t.kind == Kind::LeftParen);
            if called || name == "DYNAMIC-NEW" {
                let start = stmt.span.start + tok.start as u32;
                let end = stmt.span.start + tok.end as u32;
                self.report(Span { start, end }, name);
            }
        }
    }
}

impl Visitor for DynamicCalls<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        match &stmt.kind {
            StatementKind::Create {
                target:
                    CreateTarget::Handle {
                        kind: CreateTargetKind::Call,
                        handle,
                        ..
                    },
                ..
            } => {
                let start = stmt.span.start.min(handle.span.start);
                self.report(
                    Span {
                        start,
                        end: handle.span.end,
                    },
                    "CREATE CALL",
                );
            }
            StatementKind::Skipped { .. } => self.scan_skipped(stmt),
            _ => {}
        }
        true
    }

    fn expression(&mut self, expr: &Expression) {
        if let ExpressionKind::FunctionCall { name, .. } = &expr.kind
            && let Some(canonical) = dynamic_name(&name.name)
        {
            self.report(name.span, canonical);
        }
    }
}
