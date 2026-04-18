//! Rule registry.
//!
//! Each rule lives in its own file with inline tests. The top-level
//! [`crate::lint_file`] dispatcher composes them.

pub mod type_mismatch_assignment;
pub mod undefined_symbol;
pub mod unknown_table_or_field;
pub mod unused_variable;

/// Diagnostic code for `undefined-symbol` (LINT0001).
pub const LINT0001: &str = "LINT0001";
/// Diagnostic code for `unused-variable` (LINT0002).
pub const LINT0002: &str = "LINT0002";
/// Diagnostic code for `unknown-table-or-field` (LINT0003).
pub const LINT0003: &str = "LINT0003";
/// Diagnostic code for `type-mismatch-assignment` (LINT0004).
pub const LINT0004: &str = "LINT0004";
