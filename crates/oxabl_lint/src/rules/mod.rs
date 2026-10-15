//! The built-in rules.
//!
//! Each rule lives in its own file with inline tests. [`crate::Registry`]
//! registers and runs them.

pub mod assigned_but_never_read;
pub mod block_var_used_outside;
pub mod default_lock;
pub mod delete_not_in_finally;
pub mod dynamic_invoke;
pub mod handle_not_deleted;
pub mod lock_outlives_transaction;
pub mod lock_upgrade_in_loop;
pub mod memptr_not_freed;
pub mod missing_no_undo;
pub mod missing_widget_pool;
pub mod persistent_procedure_leak;
pub mod run_value;
pub mod shared_state;
pub mod type_mismatch_assignment;
pub mod undefined_symbol;
pub mod unknown_table_or_field;
pub mod unused_symbol_shared;
pub mod unused_variable;

// Wording and anchoring for the handle rules.
mod handle_support;
// Shared queries over the scope analysis for the lock rules.
mod lock_support;
// Walk and source helpers for the syntactic strict rules.
mod syntactic;

/// Diagnostic code for `undefined-symbol` (LINT0001).
pub const LINT0001: &str = "LINT0001";
/// Diagnostic code for `unused-variable` (LINT0002).
pub const LINT0002: &str = "LINT0002";
/// Diagnostic code for `unknown-table-or-field` (LINT0003).
pub const LINT0003: &str = "LINT0003";
/// Diagnostic code for `type-mismatch-assignment` (LINT0004).
pub const LINT0004: &str = "LINT0004";
/// Diagnostic code for `block-var-used-outside` (LINT0005).
pub const LINT0005: &str = "LINT0005";
/// Diagnostic code for `assigned-but-never-read` (LINT0006).
pub const LINT0006: &str = "LINT0006";

/// Diagnostic code for `run-value` (CL001).
pub const CL001: &str = "CL001";
/// Diagnostic code for `dynamic-invoke` (CL002).
pub const CL002: &str = "CL002";
/// Diagnostic code for `shared-state` (CL005).
pub const CL005: &str = "CL005";
/// Diagnostic code for `missing-no-undo` (TX006).
pub const TX006: &str = "TX006";

/// Diagnostic code for `handle-not-deleted` (HD001).
pub const HD001: &str = "HD001";
/// Diagnostic code for `delete-not-in-finally` (HD002).
pub const HD002: &str = "HD002";
/// Diagnostic code for `persistent-procedure-leak` (HD003).
pub const HD003: &str = "HD003";
/// Diagnostic code for `memptr-not-freed` (HD004).
pub const HD004: &str = "HD004";
/// Diagnostic code for `missing-widget-pool` (HD005).
pub const HD005: &str = "HD005";

/// Diagnostic code for `default-lock` (LK001).
pub const LK001: &str = "LK001";
/// Diagnostic code for `lock-outlives-transaction` (LK002).
pub const LK002: &str = "LK002";
/// Diagnostic code for `lock-upgrade-in-loop` (LK003).
pub const LK003: &str = "LK003";
