//! The record operand of `AVAILABLE`, `CAN-FIND` and their siblings, through
//! `oxabl check`, with and without a schema.
//!
//! With no schema loaded oxabl cannot tell a table from an unknown name, so a
//! record name nothing declares gives no finding, as in `FIND` or `FOR EACH`.
//! With a schema loaded, a table the schema does not have is still reported.
//! All fixtures are synthetic ABL written for this file.

use std::fs;
use std::path::Path;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

/// The issue's repro: every record form names `customer`.
const RECORDS: &str = "\
FIND FIRST customer NO-LOCK NO-ERROR.
IF AVAILABLE customer THEN DISPLAY customer.name.
IF NOT AVAILABLE(customer) THEN RETURN.
FOR EACH customer NO-LOCK:
    DISPLAY customer.name.
END.
DEFINE BUFFER b-cust FOR customer.
IF CAN-FIND(FIRST customer) THEN RETURN.
RELEASE customer.
";

const CUSTOMER_SCHEMA: &str = r#"ADD TABLE "customer"
  AREA "Data"
  DUMP-NAME "customer"

ADD FIELD "name" OF "customer" AS character
  FORMAT "x(30)"
  INITIAL ""
  ORDER 10

.
"#;

const ORDERS_SCHEMA: &str = r#"ADD TABLE "orders"
  AREA "Data"
  DUMP-NAME "orders"

ADD FIELD "order-num" OF "orders" AS integer
  FORMAT ">>>>9"
  INITIAL "0"
  ORDER 10

.
"#;

const CONFIG: &str = "[workspace]\nname = \"repro\"\n";

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// Checks `source` as `t.p` in a project with `config` as its `oxabl.toml` and,
/// when given, `schema` as `db.df`. Returns the lint findings as
/// `line:column CODE message`, in order.
fn findings(config: &str, schema: Option<&str>, source: &str) -> Vec<String> {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("oxabl.toml"), config);
    write(&tmp.path().join("t.p"), source);
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_oxabl"));
    cmd.current_dir(tmp.path())
        .arg("check")
        .arg("--json")
        .arg("--no-format");
    if let Some(schema) = schema {
        write(&tmp.path().join("db.df"), schema);
        cmd.arg("--schema").arg("db.df");
    }
    let out = cmd.arg("t.p").output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let v: Value = serde_json::from_str(&stdout).unwrap_or_else(|e| {
        panic!(
            "stdout is not JSON ({e}):\n{stdout}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stderr)
        )
    });
    assert!(
        v["failures"].as_array().is_some_and(|f| f.is_empty()),
        "the run failed: {v}"
    );
    let mut found: Vec<(u64, u64, String)> = v["diagnostics"]
        .as_array()
        .expect("diagnostics array")
        .iter()
        .map(|d| {
            (
                d["start"]["line"].as_u64().unwrap(),
                d["start"]["column"].as_u64().unwrap(),
                format!(
                    "{} {}",
                    d["code"].as_str().unwrap(),
                    d["message"].as_str().unwrap()
                ),
            )
        })
        .collect();
    found.sort();
    found
        .into_iter()
        .map(|(line, col, rest)| format!("{line}:{col} {rest}"))
        .collect()
}

#[test]
fn without_a_schema_available_and_can_find_accept_a_table_name() {
    let found = findings(CONFIG, None, RECORDS);
    assert!(found.is_empty(), "got:\n{}", found.join("\n"));
}

#[test]
fn without_a_schema_but_with_a_database_name_available_and_can_find_accept_a_table_name() {
    let config = format!("{CONFIG}\n[workspace.preprocessor]\ndatabases = [\"sports\"]\n");
    let found = findings(&config, None, RECORDS);
    assert!(found.is_empty(), "got:\n{}", found.join("\n"));
}

#[test]
fn with_a_schema_that_has_the_table_there_are_no_findings() {
    let found = findings(CONFIG, Some(CUSTOMER_SCHEMA), RECORDS);
    assert!(found.is_empty(), "got:\n{}", found.join("\n"));
}

/// Fail closed: a loaded schema that does not have the table makes the name
/// undefined.
#[test]
fn with_a_schema_that_lacks_the_table_available_and_can_find_report_it() {
    let found = findings(CONFIG, Some(ORDERS_SCHEMA), RECORDS);
    for expected in [
        "2:14 LINT0001 undefined symbol `customer`",
        "3:18 LINT0001 undefined symbol `customer`",
        "8:19 LINT0001 undefined symbol `customer`",
    ] {
        assert!(
            found.iter().any(|f| f == expected),
            "expected `{expected}`, got:\n{}",
            found.join("\n")
        );
    }
}

/// Only the record operand is excused: a value nothing declares is still
/// undefined without a schema.
#[test]
fn without_a_schema_an_undefined_value_is_still_reported() {
    let source = "\
FIND FIRST customer NO-LOCK NO-ERROR.
IF AVAILABLE customer AND v-missing THEN RETURN.
IF CAN-FIND(FIRST customer WHERE customer.name = v-other) THEN RETURN.
MESSAGE STRING(v-third).
";
    let found = findings(CONFIG, None, source);
    assert_eq!(
        found,
        [
            "2:27 LINT0001 undefined symbol `v-missing`",
            "3:50 LINT0001 undefined symbol `v-other`",
            "4:16 LINT0001 undefined symbol `v-third`",
        ],
    );
}

/// Checks one built-in that takes a record, written as `form` with the record
/// `customer`:
///
/// * no schema: no finding;
/// * a schema without `customer`: one `LINT0001` at the operand (fail closed);
/// * a schema with `customer`: no finding.
fn check_record_function(form: &str) {
    let source = format!("FIND FIRST customer NO-LOCK NO-ERROR.\nIF {form} THEN RETURN.\n");
    let column = 4 + form.find("customer").expect("the form names `customer`");

    let found = findings(CONFIG, None, &source);
    assert!(
        found.is_empty(),
        "`{form}` without a schema, got:\n{}",
        found.join("\n")
    );

    let found = findings(CONFIG, Some(ORDERS_SCHEMA), &source);
    assert_eq!(
        found,
        [format!("2:{column} LINT0001 undefined symbol `customer`")],
        "`{form}` under a schema that lacks the table"
    );

    let found = findings(CONFIG, Some(CUSTOMER_SCHEMA), &source);
    assert!(
        found.is_empty(),
        "`{form}` under a schema that has the table, got:\n{}",
        found.join("\n")
    );
}

macro_rules! record_function_tests {
    ($($test:ident: $form:literal,)*) => {
        $(
            #[test]
            fn $test() {
                check_record_function($form);
            }
        )*
    };
}

record_function_tests! {
    available_bare: "AVAILABLE customer",
    available_parenthesized: "AVAILABLE(customer)",
    available_abbreviated: "AVAIL customer",
    not_available_parenthesized: "NOT AVAILABLE(customer)",
    ambiguous_bare: "AMBIGUOUS customer",
    ambiguous_abbreviated: "AMBIG customer",
    current_changed_bare: "CURRENT-CHANGED customer",
    locked_bare: "LOCKED customer",
    locked_parenthesized: "LOCKED(customer)",
    new_bare: "NEW customer",
    new_parenthesized: "NEW(customer)",
    recid: "RECID(customer) <> ?",
    rowid: "ROWID(customer) <> ?",
    error: "ERROR(customer)",
    record_length: "RECORD-LENGTH(customer) > 0",
    row_state: "ROW-STATE(customer) = 0",
    rejected: "REJECTED(customer)",
    data_source_modified: "DATA-SOURCE-MODIFIED(customer)",
    buffer_group_id: "BUFFER-GROUP-ID(customer) = 0",
    buffer_group_name: "BUFFER-GROUP-NAME(customer) = \"\"",
    buffer_partition_id: "BUFFER-PARTITION-ID(customer) = 0",
    buffer_tenant_id: "BUFFER-TENANT-ID(customer) = 0",
    buffer_tenant_name: "BUFFER-TENANT-NAME(customer) = \"\"",
    can_find_first: "CAN-FIND(FIRST customer)",
    can_find_where: "CAN-FIND(customer WHERE TRUE)",
}

/// `NEW record` takes a database-qualified record name too.
#[test]
fn without_a_schema_new_accepts_a_database_qualified_table_name() {
    let found = findings(CONFIG, None, "IF NEW sports.customer THEN RETURN.\n");
    assert!(found.is_empty(), "got:\n{}", found.join("\n"));
}

/// `NEW record` is a record test, not a value: a local buffer it names is
/// read, and a schema field read after it still resolves.
#[test]
fn new_record_names_a_local_buffer_and_keeps_field_checks() {
    let source = "\
DEFINE BUFFER b-cust FOR customer.
FIND FIRST b-cust NO-LOCK NO-ERROR.
IF NEW b-cust THEN MESSAGE b-cust.name b-cust.no-such-field.
";
    let found = findings(CONFIG, Some(CUSTOMER_SCHEMA), source);
    assert_eq!(found, ["3:47 LINT0003 unknown field `no-such-field`"]);
}

/// `NEW record` at the end of a statement stops at the period: the next
/// statement is not part of the record name.
#[test]
fn new_record_at_the_end_of_a_statement_keeps_the_next_statement() {
    let source = "\
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
FIND FIRST customer NO-LOCK NO-ERROR.
IF AVAILABLE customer THEN DO:
  l = NEW customer.
  MESSAGE customer.name customer.no-such-field l.
END.
";
    let found = findings(CONFIG, Some(CUSTOMER_SCHEMA), source);
    assert_eq!(found, ["5:34 LINT0003 unknown field `no-such-field`"]);
}
