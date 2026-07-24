//! U11: `Schema::from_df_dir` + `&str` convenience getters.

use std::fs;

use oxabl_schema::{FieldResolution, Schema};

const CUSTOMER_DF: &str = r#"
ADD TABLE "Customer"
ADD FIELD "CustNum" OF "Customer" AS integer
ADD FIELD "Name" OF "Customer" AS character
"#;

#[test]
fn from_df_dir_loads_and_str_getters_work() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("customer.df"), CUSTOMER_DF).unwrap();
    // A non-`.df` sibling must be ignored by the directory walk.
    fs::write(dir.path().join("README.md"), "ignore me").unwrap();

    let (schema, diags) = Schema::from_df_dir(dir.path());
    assert!(diags.is_empty(), "clean .df should load cleanly: {diags:?}");
    assert!(!schema.is_empty());

    // `&str` table getter, case-insensitive; unknown → None.
    let table = schema.table("CUSTOMER").expect("Customer table present");
    assert!(schema.table("customer").is_some());
    assert!(schema.table("nope").is_none());

    // `&str` field getter, case-insensitive; unknown → None.
    assert!(table.field("custnum").is_some());
    assert!(table.field("NAME").is_some());
    assert!(table.field("missing").is_none());

    // `&str` prefix resolution mirrors the atom-keyed `resolve_field`.
    assert!(matches!(
        table.resolve_field_by_name("Cust"),
        FieldResolution::Unique(_)
    ));
}

#[test]
fn from_df_dir_empty_dir_yields_empty_schema() {
    let dir = tempfile::tempdir().unwrap();
    let (schema, diags) = Schema::from_df_dir(dir.path());
    assert!(schema.is_empty());
    assert!(diags.is_empty());
}

#[test]
fn from_df_dir_unreadable_dir_is_non_fatal() {
    let (schema, diags) = Schema::from_df_dir("/nonexistent/oxabl-xyz-123");
    assert!(schema.is_empty());
    assert!(diags.is_empty());
}
