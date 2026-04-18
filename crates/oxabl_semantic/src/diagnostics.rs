//! Diagnostic codes emitted by the semantic layer.
//!
//! Mirrors the `PARSE###` / `PREPROC###` / `SCHEMA###` precedent already in
//! the codebase. Lint rules own the `LINT####` prefix; semantic owns
//! `SEM###` and `TYPE###`.

/// Duplicate declaration in the same scope.
pub const SEM0001: &str = "SEM0001";

/// Redeclaration across SHARED / NEW SHARED boundary mismatch.
pub const SEM0002: &str = "SEM0002";

/// `BLOB` / `CLOB` used as a local variable type (v1 rejects; allowed only
/// on temp-table and database fields).
pub const SEM0003: &str = "SEM0003";
