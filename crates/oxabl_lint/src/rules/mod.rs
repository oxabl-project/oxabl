//! The built-in rules.
//!
//! Each rule lives in its own file with inline tests. [`crate::Registry`]
//! registers and runs them.

pub mod assigned_but_never_read;
pub mod block_var_used_outside;
pub mod default_lock;
pub mod delete_not_in_finally;
pub mod dynamic_invoke;
pub mod field_after_failed_find;
pub mod find_in_loop;
pub mod handle_not_deleted;
pub mod implicit_transaction;
pub mod lock_outlives_transaction;
pub mod lock_upgrade_in_loop;
pub mod memptr_not_freed;
pub mod missing_no_undo;
pub mod missing_widget_pool;
pub mod nullable_db_field;
pub mod persistent_procedure_leak;
pub mod procedure_wide_transaction;
pub mod run_value;
pub mod scope_widened;
pub mod shared_state;
pub mod transaction_spans_call;
pub mod transaction_spans_ui;
pub mod type_mismatch_assignment;
pub mod undefined_symbol;
pub mod unknown_in_comparison;
pub mod unknown_propagation;
pub mod unknown_table_or_field;
pub mod unused_symbol_shared;
pub mod unused_variable;

// Wording and anchoring for the handle rules.
mod handle_support;
// Shared queries over the scope analysis for the lock and transaction rules.
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
/// Diagnostic code for `implicit-transaction` (TX001).
pub const TX001: &str = "TX001";
/// Diagnostic code for `procedure-wide-transaction` (TX002).
pub const TX002: &str = "TX002";
/// Diagnostic code for `transaction-spans-ui` (TX003).
pub const TX003: &str = "TX003";
/// Diagnostic code for `transaction-spans-call` (TX004).
pub const TX004: &str = "TX004";
/// Diagnostic code for `scope-widened` (TX005).
pub const TX005: &str = "TX005";
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

/// Diagnostic code for `find-in-loop` (DB001).
pub const DB001: &str = "DB001";

/// Diagnostic code for `field-after-failed-find` (NL003).
pub const NL003: &str = "NL003";

/// Diagnostic code for `nullable-db-field` (NL004).
pub const NL004: &str = "NL004";

/// Diagnostic code for `unknown-in-comparison` (NL001).
pub const NL001: &str = "NL001";

/// Diagnostic code for `unknown-propagation` (NL002).
pub const NL002: &str = "NL002";
