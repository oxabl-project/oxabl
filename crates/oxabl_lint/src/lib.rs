//! Lint rules for Progress ABL, driven by the [`oxabl_semantic::Semantic`]
//! side-table model.
//!
//! Each rule is implemented as a standalone function that walks the program +
//! semantic result and returns a `Vec<Diagnostic>`:
//!
//! | Code     | Name                       | Default  | What it fires on                                        |
//! |----------|----------------------------|----------|--------------------------------------------------------|
//! | LINT0001 | `undefined-symbol`         | Error    | [`Resolution::Unresolved { reason: NotInScope }`]      |
//! | LINT0002 | `unused-variable`          | Warning  | Variables / Parameters never referenced at all          |
//! | LINT0003 | `unknown-table-or-field`   | Warning  | Field references under `schema_loaded == true`         |
//! | LINT0004 | `type-mismatch-assignment` | Warning  | Assignment type mismatches / narrowing conversions     |
//! | LINT0005 | `block-var-used-outside`   | Info     | Block-defined variable read outside its block, unset   |
//! | LINT0006 | `assigned-but-never-read`  | Warning  | Dead store: written, never read; at the write site      |
//!
//! LINT0002 and LINT0006 divide one population — symbols nothing reads — by
//! whether anything ever wrote to them, so a symbol yields exactly one
//! diagnostic. A project that had silenced `unused-variable` therefore needs to
//! silence `assigned-but-never-read` as well to keep the write-only half quiet.
//!
//! # Registry, groups and configuration
//!
//! Every rule is registered in a [`Registry`] with its [`RuleMeta`]: code,
//! kebab name, group, description, default severity and stability.
//! [`Registry::builtin`] holds the rules above; a consumer adds its own with
//! [`Registry::register`] and runs a subset with [`Registry::run_where`].
//! [`lint_file`] runs the built-in registry.
//!
//! The rules above are in the always-on `default` group. The `strict.*`
//! groups ([`BUILTIN_GROUPS`]) are opt-in. Which rules run, and at what
//! severity, comes from [`AnalysisContext::lint_severities`], which
//! `[workspace.lint]` in `oxabl.toml` configures globally and per directory.
//! The per-rule function signatures do not change.

use oxabl_ast::Statement;
use oxabl_common::Diagnostic;
use oxabl_semantic::{AnalysisContext, Semantic};

mod registry;
mod rules;

pub use registry::{
    BUILTIN_GROUPS, BUILTIN_RULES, FnRule, Registry, RegistryError, Rule, RuleFn, RuleGroup,
    RuleMeta, Stability, groups,
};
pub use rules::{
    LINT0001, LINT0002, LINT0003, LINT0004, LINT0005, LINT0006, assigned_but_never_read,
    block_var_used_outside, type_mismatch_assignment, undefined_symbol, unknown_table_or_field,
    unused_variable,
};

/// Run every built-in rule that `ctx.lint_severities` enables over `program`
/// + `sem` and return the combined diagnostics, in registry order.
///
/// Each diagnostic is reported at its rule's resolved severity: the configured
/// one, else the rule's default. For LINT0004, which emits both an error and a
/// warning variant, that one severity applies to both. Callers who need a
/// subset or their own rules use [`Registry`] directly.
pub fn lint_file(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    Registry::builtin_shared().run(program, sem, ctx)
}
