//! Integration test against the Riverside Software `sp2k.df` golden
//! (MIT-licensed, vendored under `fixtures/`). This is the production
//! reference fixture from sonar-openedge's database-parser tests — if we
//! regress against it we're diverging from the established grammar.

use std::path::PathBuf;

use oxabl_schema::{SchemaLoader, fold_atom};
use oxabl_workspace::InMemoryFileSystem;

const SP2K: &str = include_str!("../fixtures/sp2k.df");

fn load() -> oxabl_schema::Schema {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/sp2k.df"), SP2K.to_string());
    let (schema, diags) = SchemaLoader::load_files(&[PathBuf::from("/sp2k.df")], &fs);
    assert!(
        diags
            .iter()
            .all(|d| d.severity != oxabl_common::Severity::Error),
        "sp2k.df should parse without errors; got: {:#?}",
        diags
    );
    schema
}

#[test]
fn sp2k_df_parses_known_table_count() {
    let schema = load();
    // sp2k has ~26 tables (Benefits, Bin, Customer, Department, Employee,
    // Family, Invoice, InvoiceLine, Item, Local-Default, Order, OrderLine,
    // PO, POLine, Ref-Call, State, Supplier, TimeSheet, Vacation, Warehouse,
    // + a few more). Assert a lower bound so fixture refreshes don't break
    // the test on benign additions.
    assert!(
        schema.len() >= 20,
        "expected at least 20 tables, got {}",
        schema.len()
    );
}

#[test]
fn sp2k_customer_table_has_expected_shape() {
    let schema = load();
    let customer = schema
        .get(&fold_atom("customer"))
        .expect("Customer table should exist in sp2k");
    assert!(
        customer.fields.len() >= 10,
        "Customer should have many fields, got {}",
        customer.fields.len()
    );
    let custnum = customer
        .get_field(&fold_atom("custnum"))
        .expect("CustNum field should exist");
    assert_eq!(custnum.display_name, "CustNum");
    assert_eq!(custnum.data_type, oxabl_schema::SchemaType::Integer);
}

#[test]
fn sp2k_benefits_table_picks_up_attributes() {
    let schema = load();
    let benefits = schema
        .get(&fold_atom("benefits"))
        .expect("Benefits table should exist");
    assert_eq!(benefits.area.as_deref(), Some("Employee"));
    assert_eq!(benefits.dump_name.as_deref(), Some("benefits"));
    assert!(benefits.description.is_some());
}

#[test]
fn sp2k_warehouse_indexes_parse() {
    let schema = load();
    let warehouse = schema
        .get(&fold_atom("warehouse"))
        .expect("Warehouse table should exist");
    let primary = warehouse
        .indexes
        .iter()
        .find(|i| i.primary)
        .expect("Warehouse should have a PRIMARY index");
    assert!(primary.unique, "warehouse PRIMARY index should be UNIQUE");
    assert!(
        !primary.fields.is_empty(),
        "PRIMARY index should have at least one INDEX-FIELD"
    );
}

#[test]
fn sp2k_loader_bumps_revision_once() {
    let schema = load();
    assert_eq!(schema.revision().raw(), 1);
}
