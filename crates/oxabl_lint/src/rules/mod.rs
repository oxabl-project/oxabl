//! The built-in rules.
//!
//! Each rule lives in its own file with inline tests. [`crate::Registry`]
//! registers and runs them.

pub mod assigned_but_never_read;
pub mod block_var_used_outside;
pub mod dynamic_invoke;
pub mod missing_no_undo;
pub mod run_value;
pub mod shared_state;
pub mod type_mismatch_assignment;
pub mod undefined_symbol;
pub mod unknown_table_or_field;
pub mod unused_symbol_shared;
pub mod unused_variable;

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
