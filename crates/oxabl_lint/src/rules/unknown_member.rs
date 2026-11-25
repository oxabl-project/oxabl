//! `unknown-member` lint (CL007), group `strict.calls`.
//!
//! Fires on a member name that a built-in type does not have: no method,
//! property, event or enum member of the type or of its supertypes has it.
//!
//! # Why
//!
//! ```abl
//! DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.
//! oErr = NEW Progress.Lang.AppError("Order not found", 0).
//! MESSAGE oErr:GetMesage(1).
//! ```
//!
//! `GetMesage` is a typing error. The call does not compile.
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
//! The members come from the catalog of the OpenEdge documentation
//! (`docs/design/builtin-classes.md`). The catalog has errors, so the rule
//! judges only a type whose whole surface the catalog knows: a type with a
//! member the catalog could not read, a generic type, a type with a .NET or
//! undocumented supertype, and an enum whose members the catalog does not list
//! are not judged. Only a receiver whose type is the built-in type is judged,
//! not a user class that inherits one. A name the type has in another
//! namespace (a property called as a method) is no finding of this rule.
//! Nothing in an include analysed alone is reported, and nothing when
//! `[workspace.preprocessor].proversion` is newer than the catalog (12.8): a
//! later version may have added the member.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::CL007;
use super::syntactic::file_span;

/// Kebab-case rule name.
pub const NAME: &str = "unknown-member";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.calls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "A member name that a built-in type does not have";

/// Entry point.
pub fn run(_program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    sem.symbols
        .unknown_members()
        .iter()
        .map(|m| {
            Diagnostic::warning(
                CL007,
                format!(
                    "built-in type `{}` has no member `{}`",
                    m.class.name, m.name
                ),
                file_span(ctx, m.span),
            )
            .with_help("Check the name in the Progress documentation.".to_string())
        })
        .collect()
}
