//! Diagnostic code inventory for `oxabl_schema`.
//!
//! Aligned to the repo-wide prefix convention (`PARSE###`, `PREPROC###`,
//! `SEM###`, `LINT###`).

/// `.df` parse / tokeniser error.
pub const SCHEMA0001: &str = "SCHEMA0001";
/// Duplicate table across merged `.df` files (warning; last-write-wins).
pub const SCHEMA0010: &str = "SCHEMA0010";
/// Duplicate field within a table (warning).
pub const SCHEMA0011: &str = "SCHEMA0011";
/// Field type conflict across merged `.df` files (error; field poisoned).
pub const SCHEMA0012: &str = "SCHEMA0012";
/// Schema path escapes the workspace root.
pub const SCHEMA0030: &str = "SCHEMA0030";
/// Resource soft cap exceeded (too many tables or fields).
pub const SCHEMA0031: &str = "SCHEMA0031";
