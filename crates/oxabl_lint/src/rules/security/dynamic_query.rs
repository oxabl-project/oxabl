//! `dynamic-query` (ABLSEC001), group `security`.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

/// Kebab-case rule name.
pub const NAME: &str = "dynamic-query";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Error;
/// One-line description for listings.
pub const DESCRIPTION: &str = "QUERY-PREPARE, FIND-FIRST, FIND-LAST, FIND-UNIQUE and FILL-WHERE-STRING take a query built at run time";

/// Entry point.
pub fn run(_program: &[Statement], _sem: &Semantic, _ctx: &AnalysisContext) -> Vec<Diagnostic> {
    Vec::new()
}
