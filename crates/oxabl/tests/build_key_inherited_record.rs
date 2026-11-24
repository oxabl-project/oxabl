//! A subclass that reads a field through a buffer or temp-table its superclass
//! defines over a database table gets a new compile key, and is rebuilt, when
//! that table's schema changes. The tests drive `oxabl build --plan --probe`
//! with the fake AVM, one edit of the schema image between two plans.

mod common;

use common::built::Built;
use common::keyed::{Keyed, example_environment};
use serde_json::{Value, json};

const BASE: &str = "src/pkg/Base.cls";
const SUB: &str = "src/app/Sub.cls";
const SUB_TEXT: &str = "USING pkg.*.\nCLASS app.Sub INHERITS pkg.Base:\n    METHOD PUBLIC VOID Run():\n        MESSAGE ttCust.name cust_.name.\n    END METHOD.\nEND CLASS.\n";

fn environment() -> Value {
    let mut environment = example_environment();
    let schema = environment["databases"][0]["schema"]
        .as_array_mut()
        .unwrap();
    for record in [
        json!([
            "table",
            "item",
            "_can-read=*",
            "_crc=777",
            "_dump-name=item"
        ]),
        json!([
            "field",
            "item",
            "sku",
            "_data-type=character",
            "_decimals=?",
            "_format=x(10)",
            "_label=Sku",
            "_view-as=?"
        ]),
    ] {
        schema.push(record);
    }
    environment
}

fn workspace(base: &str) -> Keyed {
    let workspace = Keyed::simple();
    workspace.set_environment(&environment());
    workspace.preprocessor_line("databases = [\"ledger\"]");
    workspace.write(BASE, base);
    workspace.write(SUB, SUB_TEXT);
    workspace
}

const BASES: [&str; 2] = [
    "CLASS pkg.Base:\n    DEFINE PROTECTED TEMP-TABLE ttCust NO-UNDO LIKE customer.\n    DEFINE PROTECTED BUFFER cust_ FOR customer.\nEND CLASS.\n",
    "CLASS pkg.Base:\n    DEFINE PROTECTED TEMP-TABLE ttCust NO-UNDO LIKE customer.\n    DEFINE PROTECTED BUFFER cust_ FOR customer.\n    METHOD PUBLIC VOID Touch():\n        FIND FIRST cust_ NO-LOCK NO-ERROR.\n    END METHOD.\nEND CLASS.\n",
];

fn customer_label(workspace: &Keyed, label: &str) {
    workspace.edit_environment(|e| {
        e["databases"][0]["schema"][3][7] = json!(format!("_label={label}"));
    });
}

#[test]
fn the_table_of_an_inherited_record_changes_the_subclass_key() {
    for base in BASES {
        let workspace = workspace(base);
        let before = workspace.plan();
        customer_label(&workspace, "Customer Name");
        let after = workspace.plan();
        assert_ne!(before.key_of(SUB), after.key_of(SUB), "{base}");
    }
}

#[test]
fn the_table_of_an_inherited_record_rebuilds_the_subclass() {
    for base in BASES {
        let workspace = Built::new();
        workspace.set_environment(&environment());
        workspace.databases(&["ledger"]);
        workspace.write(BASE, base);
        workspace.write(SUB, SUB_TEXT);
        workspace.build().succeeded();
        assert_eq!(workspace.plan()["summary"]["rebuild"], 0, "{base}");

        workspace.set_environment(&{
            let mut changed = environment();
            changed["databases"][0]["schema"][3][7] = json!("_label=Customer Name");
            changed
        });

        let plan = workspace.plan();
        assert!(
            !Built::reasons(&plan, SUB).is_empty(),
            "{SUB} is not rebuilt: {plan}"
        );
    }
}
