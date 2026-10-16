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
//! | LK001 | [`default_lock`]            | `strict.locks`        | a database read with no lock phrase (`SHARE-LOCK`)        |
//! | LK002 | [`lock_outlives_transaction`] | `strict.locks`      | a record still locked after its transaction, because the buffer scope is wider |
//! | LK003 | [`lock_upgrade_in_loop`]    | `strict.locks`        | a `NO-LOCK` read upgraded to `EXCLUSIVE-LOCK` on every iteration |
//! | TX001 | [`implicit_transaction`]    | `strict.transactions` | a block that is a transaction only because it updates the database |
//! | TX002 | [`procedure_wide_transaction`] | `strict.transactions` | a main block that is a transaction block                |
//! | TX003 | [`transaction_spans_ui`]    | `strict.transactions` | a statement that waits for the user inside a transaction  |
//! | TX004 | [`transaction_spans_call`]  | `strict.transactions` | a `RUN` inside a transaction whose callee is not in the file |
//! | TX005 | [`scope_widened`]           | `strict.transactions` | a reference that raises a `FOR` block's buffer scope to the procedure |
//! | DB001 | [`find_in_loop`]            | `strict.access`       | a `FIND` or `CAN-FIND` that runs once per row of a record loop (N+1) |
//! | NL001 | [`unknown_in_comparison`]   | `strict.nulls`        | a local that may hold `?` in an ordering comparison or as a condition |
//! | NL002 | [`unknown_propagation`]     | `strict.nulls`        | a local that may hold `?` in arithmetic whose result is stored or displayed |
//! | NL003 | [`field_after_failed_find`] | `strict.nulls`        | a buffer field used after a `FIND ... NO-ERROR` that may have failed |
//! | NL004 | [`nullable_db_field`]       | `strict.nulls`        | a database field that starts at `?` and is not mandatory, in a comparison or arithmetic |
//!
//! The `HD` rules read one analysis of the file's dynamic objects
//! (`oxabl_handles`), the `LK` rules and `TX001`-`TX005` one analysis of
//! its buffer scopes, transactions and locks (`oxabl_scope`), the `DB`
//! rules one analysis of its database effects (`oxabl_effects`) over the same
//! scopes, and the `NL` rules one analysis of its failed finds
//! (`oxabl_nulls`) over them too. Each is made once per [`Registry::run_where`] through [`RunShared`]
//! and only when one of its rules is enabled.
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
    BUILTIN_GROUPS, BUILTIN_RULES, EffectsFn, FnRule, HandlesFn, NullsFn, Registry, RegistryError,
    Rule, RuleFn, RuleGroup, RuleMeta, ScopesFn, Stability, groups,
};
pub use rules::{
    CL001, CL002, CL005, DB001, HD001, HD002, HD003, HD004, HD005, LINT0001, LINT0002, LINT0003,
    LINT0004, LINT0005, LINT0006, LK001, LK002, LK003, NL001, NL002, NL003, NL004, TX001, TX002,
    TX003, TX004, TX005, TX006, assigned_but_never_read, block_var_used_outside, default_lock,
    delete_not_in_finally, dynamic_invoke, field_after_failed_find, find_in_loop,
    handle_not_deleted, implicit_transaction, lock_outlives_transaction, lock_upgrade_in_loop,
    memptr_not_freed, missing_no_undo, missing_widget_pool, nullable_db_field,
    persistent_procedure_leak, procedure_wide_transaction, run_value, scope_widened, shared_state,
    transaction_spans_call, transaction_spans_ui, type_mismatch_assignment, undefined_symbol,
    unknown_in_comparison, unknown_propagation, unknown_table_or_field, unused_variable,
};
pub use shared::{CalleeSummaries, RunShared};

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
