//! Test-only schema fixtures, gated behind the `test-support` feature.
//!
//! Downstream crates (`oxabl_semantic`, `oxabl_lint`, `oxabl_analyze`) enable
//! the feature from their dev-dependencies to share one canonical loader-built
//! schema instead of copy-pasting `.df` fixtures per test module. Schemas are
//! built through [`SchemaLoader`] so table ids and the [`SchemaRevision`]
//! (crate::SchemaRevision) are loader-minted, never hand-constructed.

use std::path::PathBuf;

use oxabl_workspace::InMemoryFileSystem;

use crate::{Schema, SchemaLoader};

/// `.df` source for the canonical test fixture:
/// `Customer(CustNum INTEGER, Name CHARACTER)`.
pub const CUSTOMER_DF: &str = r#"
ADD TABLE "Customer"
ADD FIELD "CustNum" OF "Customer" AS integer
ADD FIELD "Name" OF "Customer" AS character
"#;

/// Build a [`Schema`] from inline `.df` text via the real loader.
///
/// Panics if the text produces any load diagnostics — a fixture that fails
/// to load cleanly is a bug in the test, not a case to tolerate.
pub fn schema_from_df(df: &str) -> Schema {
    let path = PathBuf::from("/test.df");
    let mut fs = InMemoryFileSystem::new();
    fs.insert(path.clone(), df.to_string());
    let (schema, diags) = SchemaLoader::load_files(&[path], &fs);
    assert!(diags.is_empty(), "test .df should load cleanly: {diags:?}");
    schema
}

/// The canonical test schema: one `Customer` table with an INTEGER and a
/// CHARACTER field (see [`CUSTOMER_DF`]).
pub fn customer_schema() -> Schema {
    schema_from_df(CUSTOMER_DF)
}
