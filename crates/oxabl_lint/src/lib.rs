//! Lint rules for Progress ABL, driven by the [`oxabl_semantic::Semantic`]
//! side-table model.
//!
//! Each rule is implemented as a standalone function that walks the program +
//! semantic result and returns a `Vec<Diagnostic>`:
//!
//! | Code     | Name                       | Severity | What it fires on                                        |
//! |----------|----------------------------|----------|--------------------------------------------------------|
//! | LINT0001 | `undefined-symbol`         | Error    | [`Resolution::Unresolved { reason: NotInScope }`]      |
//! | LINT0002 | `unused-variable`          | Warning  | Variables / Parameters with `read_count == 0`          |
//! | LINT0003 | `unknown-table-or-field`   | Error    | Field references under `schema_loaded == true`         |
//! | LINT0004 | `type-mismatch-assignment` | Err/Warn | Assignment type mismatches / narrowing conversions     |
//! | LINT0005 | `block-var-used-outside`   | Info     | Block-defined variable read outside its block, unset   |
//!
//! Per-rule severity (including *off*) is configured through
//! `[workspace.lint]` in `oxabl.toml` and applied via
//! [`AnalysisContext::lint_severities`] without touching the per-rule
//! function signatures.

use oxabl_ast::Statement;
use oxabl_common::Diagnostic;
use oxabl_semantic::{AnalysisContext, Semantic};

mod rules;

pub use rules::{
    LINT0001, LINT0002, LINT0003, LINT0004, LINT0005, block_var_used_outside,
    type_mismatch_assignment, undefined_symbol, unknown_table_or_field, unused_variable,
};

/// Run every lint rule over `program` + `sem` and return a combined list
/// of diagnostics, in a stable per-rule order (LINT0001 → LINT0004). Each
/// rule is independent; callers who need finer control can invoke the
/// individual rule functions directly.
///
/// The per-rule severity surface (`ctx.lint_severities`, KTD6) is applied
/// here without touching the individual rule signatures: a rule whose code is
/// configured *off* is skipped entirely, and a rule configured with an
/// explicit severity has every diagnostic it emits remapped to that level.
/// For LINT0004 — which emits both an error and a warning variant — a single
/// configured severity overrides *both* (documented, intended behavior).
pub fn lint_file(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    run_rule(&mut diags, LINT0001, ctx, || {
        undefined_symbol::run(program, sem, ctx)
    });
    run_rule(&mut diags, LINT0002, ctx, || {
        unused_variable::run(program, sem, ctx)
    });
    run_rule(&mut diags, LINT0003, ctx, || {
        unknown_table_or_field::run(program, sem, ctx)
    });
    run_rule(&mut diags, LINT0004, ctx, || {
        type_mismatch_assignment::run(program, sem, ctx)
    });
    run_rule(&mut diags, LINT0005, ctx, || {
        block_var_used_outside::run(program, sem, ctx)
    });
    diags
}

/// Run a rule only if it is enabled, then append its diagnostics to `out`,
/// remapping each to the configured severity when one is set. A rule
/// configured *off* is not executed at all. The per-rule function signatures
/// stay untouched — the severity surface is applied entirely here (KTD6).
fn run_rule(
    out: &mut Vec<Diagnostic>,
    code: &'static str,
    ctx: &AnalysisContext,
    produce: impl FnOnce() -> Vec<Diagnostic>,
) {
    if !ctx.lint_severities.is_enabled(code) {
        return;
    }
    for mut d in produce() {
        d.severity = ctx.lint_severities.effective(d.code.0, d.severity);
        out.push(d);
    }
}
