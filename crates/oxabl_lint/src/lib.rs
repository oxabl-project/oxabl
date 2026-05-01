//! Lint rules for Progress ABL, driven by the [`oxabl_semantic::Semantic`]
//! side-table model.
//!
//! v1 ships four rules, each implemented as a standalone function that
//! walks the program + semantic result and returns a `Vec<Diagnostic>`:
//!
//! | Code     | Name                       | Severity | What it fires on                                        |
//! |----------|----------------------------|----------|--------------------------------------------------------|
//! | LINT0001 | `undefined-symbol`         | Error    | [`Resolution::Unresolved { reason: NotInScope }`]      |
//! | LINT0002 | `unused-variable`          | Warning  | Variables / Parameters with `read_count == 0`          |
//! | LINT0003 | `unknown-table-or-field`   | Error    | Field references under `schema_loaded == true`         |
//! | LINT0004 | `type-mismatch-assignment` | Err/Warn | Assignment type mismatches / narrowing conversions     |
//!
//! No configuration mechanism exists in v1 (there is no oxabl config file
//! yet). When rule-toggle configuration lands it'll extend
//! [`AnalysisContext`] without touching the per-rule function signatures.

use oxabl_ast::Statement;
use oxabl_common::Diagnostic;
use oxabl_semantic::{AnalysisContext, Semantic};

mod rules;

pub use rules::{
    LINT0001, LINT0002, LINT0003, LINT0004, type_mismatch_assignment, undefined_symbol,
    unknown_table_or_field, unused_variable,
};

/// Run every lint rule over `program` + `sem` and return a combined list
/// of diagnostics, in a stable per-rule order (LINT0001 → LINT0004). Each
/// rule is independent; callers who need finer control can invoke the
/// individual rule functions directly.
pub fn lint_file(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    diags.extend(undefined_symbol::run(program, sem, ctx));
    diags.extend(unused_variable::run(program, sem, ctx));
    diags.extend(unknown_table_or_field::run(program, sem, ctx));
    diags.extend(type_mismatch_assignment::run(program, sem, ctx));
    diags
}
