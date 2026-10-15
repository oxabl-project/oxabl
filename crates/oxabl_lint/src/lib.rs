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
//! | LINT0003 | `unknown-table-or-field`   | Warning  | Field / sequence references under `schema_loaded`      |
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
//!
//! The syntactic strict rules below are registered in [`Registry::builtin`]
//! in their opt-in groups, so [`lint_file`] runs them only where the
//! configuration enables the group or the rule. They are in preview and
//! context-free ([`RuleMeta::context_free`]); each module carries its rule's
//! documentation and its metadata constants (`NAME`, `GROUP`,
//! `DEFAULT_SEVERITY`, `DESCRIPTION`):
//!
//! | Code  | Module                      | Group                 | What it fires on                                          |
//! |-------|-----------------------------|-----------------------|-----------------------------------------------------------|
//! | CL001 | [`run_value`]               | `strict.calls`        | `RUN VALUE(...)`                                          |
//! | CL002 | [`dynamic_invoke`]          | `strict.calls`        | `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `DYNAMIC-PROPERTY`, `CREATE CALL` |
//! | CL005 | [`shared_state`]            | `strict.calls`        | `NEW SHARED`, `NEW GLOBAL SHARED` and `SHARED` definitions |
//! | TX006 | [`missing_no_undo`]         | `strict.transactions` | variable, parameter, temp-table or property without `NO-UNDO` |
//! | HD001 | [`handle_not_deleted`]      | `strict.handles`      | a dynamic object that some path loses without a delete    |
//! | HD002 | [`delete_not_in_finally`]   | `strict.handles`      | a delete that an error before it skips                    |
//! | HD003 | [`persistent_procedure_leak`] | `strict.handles`    | `RUN ... PERSISTENT SET h` that nothing deletes           |
//! | HD004 | [`memptr_not_freed`]        | `strict.handles`      | `SET-SIZE(m) = n` with no `SET-SIZE(m) = 0`               |
//! | HD005 | [`missing_widget_pool`]     | `strict.handles`      | dynamic objects with no widget pool scoped to the procedure |
//!
//! The `HD` rules read one analysis of the file's dynamic objects
//! (`oxabl_handles`), made once per [`Registry::run_where`] through
//! [`RunShared`] and only when one of them is enabled.
//!
//! A finding that sits in an include file is dropped when the includer is
//! linted. A context-free rule reports it once, in the include file itself,
//! when a client lints that file as a root (`oxabl check <dir>` does so for
//! every `.i` file it walks; an editor does so for an open buffer). The fragment
//! is expanded with no includer, so a finding on a statement that an includer
//! can change (a `{...}` reference, an `&IF`) is lower confidence: see
//! [`fragment`].
//!

use oxabl_ast::Statement;
use oxabl_common::Diagnostic;
use oxabl_semantic::{AnalysisContext, Semantic};

pub mod fragment;
mod registry;
mod rules;
mod shared;

pub use registry::{
    BUILTIN_GROUPS, BUILTIN_RULES, FnRule, HandlesFn, Registry, RegistryError, Rule, RuleFn,
    RuleGroup, RuleMeta, Stability, groups,
};
pub use rules::{
    CL001, CL002, CL005, HD001, HD002, HD003, HD004, HD005, LINT0001, LINT0002, LINT0003, LINT0004,
    LINT0005, LINT0006, TX006, assigned_but_never_read, block_var_used_outside,
    delete_not_in_finally, dynamic_invoke, handle_not_deleted, memptr_not_freed, missing_no_undo,
    missing_widget_pool, persistent_procedure_leak, run_value, shared_state,
    type_mismatch_assignment, undefined_symbol, unknown_table_or_field, unused_variable,
};
pub use shared::RunShared;

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
