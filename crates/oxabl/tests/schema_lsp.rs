//! `oxabl/schema` and `oxabl/schemaTable` through the real `oxabl lsp`: the
//! configured `.df` files as a tree of databases, tables, fields, indexes and
//! sequences, each with the place of its name in its file.
//!
//! All fixtures are synthetic.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"shop\"\n\
                      [workspace.schema]\n\
                      files = [\"db/sales.df\", \"db/stock.df\", \"db/cut.df\", \"db/gone.df\"]\n";

const SALES: &str = r#"ADD TABLE "Customer"
  AREA "Data"
  LABEL "Client"
  DESCRIPTION "People who buy things"
  DUMP-NAME "cust"

ADD FIELD "CustNum" OF "Customer" AS integer
  FORMAT ">>>>9"
  INITIAL "0"
  LABEL "Cust Num"
  MANDATORY
  ORDER 10

ADD FIELD "Phone" OF "Customer" AS character
  FORMAT "x(20)"
  INITIAL ""
  LABEL "Phone"
  EXTENT 3
  DESCRIPTION "Home, work and mobile"
  ORDER 20

ADD FIELD "Salesrep" OF "Customer" AS character
  FORMAT "x(8)"
  LABEL "Rep"
  ORDER 30

ADD INDEX "CustNum" ON "Customer"
  AREA "Index"
  UNIQUE
  PRIMARY
  INDEX-FIELD "CustNum" ASCENDING

ADD INDEX "Rep" ON "Customer"
  AREA "Index"
  INDEX-FIELD "Salesrep" ASCENDING
  INDEX-FIELD "CustNum" DESCENDING

ADD TABLE "Salesrep"
  AREA "Data"

ADD FIELD "Salesrep" OF "Salesrep" AS character
  FORMAT "x(8)"
  ORDER 10

ADD TABLE "Item"
  AREA "Data"

ADD FIELD "Price" OF "Item" AS decimal
  FORMAT "->>,>>9.99"
  DECIMALS 2
  ORDER 10

ADD TABLE "Größe"
  AREA "Data"
  LABEL "Größentabelle"

ADD FIELD "Breite-ü" OF "Größe" AS decimal
  LABEL "Breite in Ärmel"
  ORDER 10

ADD INDEX "Maß" ON "Größe"
  INDEX-FIELD "Breite-ü" ASCENDING

ADD SEQUENCE "NextCust"
  INITIAL 1
  INCREMENT 1
  CYCLE-ON-LIMIT no
  MIN-VAL 1
  MAX-VAL ?

.
PSC
cpstream=UTF-8
.
0000001234
"#;

const STOCK: &str = r#"ADD TABLE "Item"
  AREA "Data"
  DESCRIPTION "What the warehouse holds"

ADD FIELD "OnHand" OF "Item" AS integer
  FORMAT "->>>>9"
  ORDER 10

ADD TABLE "Bin"
  AREA "Data"

ADD FIELD "Shelf" OF "Bin" AS character
  ORDER 10

ADD FIELD "Late" OF "Customer" AS logical
  ORDER 99

ADD SEQUENCE "NextBin"
  INITIAL 100
  INCREMENT 5
"#;

/// A dump that breaks off inside a quoted string.
const CUT: &str = r#"ADD TABLE "Ledger"
  AREA "Data"

ADD FIELD "Amount" OF "Ledger" AS decimal
  FORMAT "->>,>>9.99"
  ORDER 10

ADD FIELD "Memo" OF "Ledger" AS character
  LABEL "Memo of the entr"#;

fn tree() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("db")).unwrap();
    for (name, text) in [
        ("oxabl.toml", CONFIG),
        ("db/sales.df", SALES),
        ("db/stock.df", STOCK),
        ("db/cut.df", CUT),
    ] {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

#[track_caller]
fn schema(session: &mut Session) -> Value {
    let answer = session.request("oxabl/schema", json!({}));
    assert!(answer.get("error").is_none(), "{answer}");
    answer["result"].clone()
}

#[track_caller]
fn database<'a>(answer: &'a Value, name: &str) -> &'a Value {
    answer["databases"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["name"] == json!(name))
        .unwrap_or_else(|| panic!("no database `{name}` in {answer:#}"))
}

#[track_caller]
fn named<'a>(list: &'a Value, name: &str) -> &'a Value {
    list.as_array()
        .unwrap_or_else(|| panic!("not a list: {list}"))
        .iter()
        .find(|t| t["name"] == json!(name))
        .unwrap_or_else(|| panic!("no `{name}` in {list:#}"))
}

fn names(list: &Value) -> Vec<String> {
    list.as_array()
        .unwrap()
        .iter()
        .map(|v| v["name"].as_str().unwrap().to_string())
        .collect()
}

/// The 0-based line of the first line of `text` that holds `needle`.
#[track_caller]
fn line_of(text: &str, needle: &str) -> u64 {
    text.lines()
        .position(|line| line.contains(needle))
        .unwrap_or_else(|| panic!("no `{needle}` in the fixture")) as u64
}

/// The node's range starts on the line of `needle`, at the quote of its name.
#[track_caller]
fn assert_on(node: &Value, text: &str, needle: &str) {
    let line = line_of(text, needle);
    let column = text.lines().nth(line as usize).unwrap().find('"').unwrap() as u64;
    assert_eq!(
        (
            node["range"]["start"]["line"].as_u64(),
            node["range"]["start"]["character"].as_u64()
        ),
        (Some(line), Some(column)),
        "`{needle}`: {node:#}"
    );
}

#[test]
fn the_tree_holds_every_table_field_index_and_sequence_of_each_file() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);

    assert_eq!(answer["reason"], Value::Null, "{answer:#}");
    assert_eq!(
        names(&answer["databases"]),
        ["sales", "stock", "cut", "gone"],
        "one database per configured file, in the configured order"
    );

    let sales = database(&answer, "sales");
    assert_eq!(sales["complete"], json!(true), "{sales:#}");
    assert_eq!(sales["problems"], json!([]));
    assert_eq!(
        sales["uri"],
        json!(file_uri(
            &std::fs::canonicalize(dir.path().join("db/sales.df")).unwrap()
        ))
    );
    assert_eq!(
        names(&sales["tables"]),
        ["Customer", "Salesrep", "Item", "Größe"]
    );

    let customer = named(&sales["tables"], "Customer");
    assert_on(customer, SALES, r#"ADD TABLE "Customer""#);
    assert_eq!(customer["declared"], json!(true));
    assert_eq!(customer["label"], json!("Client"));
    assert_eq!(customer["description"], json!("People who buy things"));
    assert_eq!(customer["dumpName"], json!("cust"));
    assert_eq!(names(&customer["fields"]), ["CustNum", "Phone", "Salesrep"]);

    let cust_num = named(&customer["fields"], "CustNum");
    assert_on(cust_num, SALES, r#"ADD FIELD "CustNum""#);
    assert_eq!(cust_num["dataType"], json!("integer"));
    assert_eq!(cust_num["format"], json!(">>>>9"));
    assert_eq!(cust_num["initial"], json!("0"));
    assert_eq!(cust_num["label"], json!("Cust Num"));
    assert_eq!(cust_num["mandatory"], json!(true));
    assert_eq!(cust_num["extent"], Value::Null);

    let phone = named(&customer["fields"], "Phone");
    assert_on(phone, SALES, r#"ADD FIELD "Phone""#);
    assert_eq!(phone["dataType"], json!("character"));
    assert_eq!(phone["extent"], json!(3));
    assert_eq!(phone["mandatory"], json!(false));
    assert_eq!(phone["initial"], json!(""));
    assert_eq!(phone["description"], json!("Home, work and mobile"));

    // A field and a table with one name stay apart.
    let rep_field = named(&customer["fields"], "Salesrep");
    assert_on(rep_field, SALES, r#"ADD FIELD "Salesrep" OF "Customer""#);
    let rep_table = named(&sales["tables"], "Salesrep");
    assert_on(rep_table, SALES, r#"ADD TABLE "Salesrep""#);
    assert_eq!(names(&rep_table["fields"]), ["Salesrep"]);
    assert_on(
        &rep_table["fields"][0],
        SALES,
        r#"ADD FIELD "Salesrep" OF "Salesrep""#,
    );

    assert_eq!(names(&customer["indexes"]), ["CustNum", "Rep"]);
    let primary = named(&customer["indexes"], "CustNum");
    assert_on(primary, SALES, r#"ADD INDEX "CustNum""#);
    assert_eq!(
        (primary["unique"].clone(), primary["primary"].clone()),
        (json!(true), json!(true))
    );
    let rep = named(&customer["indexes"], "Rep");
    assert_on(rep, SALES, r#"ADD INDEX "Rep""#);
    assert_eq!(
        (rep["unique"].clone(), rep["primary"].clone()),
        (json!(false), json!(false))
    );
    assert_eq!(
        rep["fields"],
        json!([
            {"name": "Salesrep", "ascending": true, "abbreviated": false},
            {"name": "CustNum", "ascending": false, "abbreviated": false},
        ])
    );

    let price = named(&named(&sales["tables"], "Item")["fields"], "Price");
    assert_on(price, SALES, r#"ADD FIELD "Price""#);
    assert_eq!(
        (price["dataType"].clone(), price["decimals"].clone()),
        (json!("decimal"), json!(2))
    );

    // Non-ASCII names, with ranges in UTF-16 code units (the default encoding).
    let groesse = named(&sales["tables"], "Größe");
    assert_on(groesse, SALES, r#"ADD TABLE "Größe""#);
    assert_eq!(groesse["label"], json!("Größentabelle"));
    let width = named(&groesse["fields"], "Breite-ü");
    assert_on(width, SALES, r#"ADD FIELD "Breite-ü""#);
    assert_eq!(width["label"], json!("Breite in Ärmel"));
    let line = line_of(SALES, r#"ADD FIELD "Breite-ü""#);
    // `"Breite-ü"` is 10 UTF-16 code units (11 bytes) from character 10.
    assert_eq!(
        width["range"]["end"],
        json!({"line": line, "character": 20}),
        "{width:#}"
    );
    let measure = named(&groesse["indexes"], "Maß");
    assert_on(measure, SALES, r#"ADD INDEX "Maß""#);
    assert_eq!(measure["fields"][0]["name"], json!("Breite-ü"));

    assert_eq!(names(&sales["sequences"]), ["NextCust"]);
    let next = &sales["sequences"][0];
    assert_on(next, SALES, r#"ADD SEQUENCE "NextCust""#);
    assert_eq!(next["initial"], json!(1));
    assert_eq!(next["increment"], json!(1));
    assert_eq!(next["minValue"], json!(1));
    assert_eq!(next["maxValue"], Value::Null, "MAX-VAL ? is no limit");
    assert_eq!(next["cycleOnLimit"], json!(false));

    // A table in both files shows in each, with what that file declares.
    let stock = database(&answer, "stock");
    assert_eq!(stock["complete"], json!(true), "{stock:#}");
    assert_eq!(names(&stock["tables"]), ["Item", "Bin", "Customer"]);
    let item = named(&stock["tables"], "Item");
    assert_on(item, STOCK, r#"ADD TABLE "Item""#);
    assert_eq!(item["description"], json!("What the warehouse holds"));
    assert_eq!(names(&item["fields"]), ["OnHand"]);
    assert_on(&item["fields"][0], STOCK, r#"ADD FIELD "OnHand""#);
    assert_eq!(names(&named(&sales["tables"], "Item")["fields"]), ["Price"]);

    // A field added to a table this file does not declare.
    let extended = named(&stock["tables"], "Customer");
    assert_eq!(extended["declared"], json!(false), "{extended:#}");
    assert_eq!(names(&extended["fields"]), ["Late"]);
    assert_on(extended, STOCK, r#"ADD FIELD "Late""#);
    assert_eq!(names(&stock["sequences"]), ["NextBin"]);
    assert_on(&stock["sequences"][0], STOCK, r#"ADD SEQUENCE "NextBin""#);

    // A file that breaks off: what came before is there, and the answer says
    // that it is not complete.
    let cut = database(&answer, "cut");
    assert_eq!(cut["complete"], json!(false), "{cut:#}");
    let ledger = named(&cut["tables"], "Ledger");
    assert_on(ledger, CUT, r#"ADD TABLE "Ledger""#);
    assert_on(
        named(&ledger["fields"], "Amount"),
        CUT,
        r#"ADD FIELD "Amount""#,
    );
    let problems = cut["problems"].as_array().unwrap();
    assert!(
        problems
            .iter()
            .any(|p| p["severity"] == json!("error") && p["range"]["start"]["line"].is_u64()),
        "{cut:#}"
    );

    // A configured file that is not there.
    let gone = database(&answer, "gone");
    assert_eq!(gone["complete"], json!(false));
    assert_eq!(gone["tables"], json!([]));
    assert!(
        gone["problems"][0]["message"]
            .as_str()
            .unwrap()
            .contains("cannot read"),
        "{gone:#}"
    );
}

#[test]
fn ranges_follow_the_negotiated_position_encoding() {
    let dir = tree();
    let mut session = Session::start_with(
        dir.path(),
        json!({"general": {"positionEncodings": ["utf-8"]}}),
    );
    let answer = schema(&mut session);
    let groesse = named(&database(&answer, "sales")["tables"], "Größe");
    let width = named(&groesse["fields"], "Breite-ü");
    let line = line_of(SALES, r#"ADD FIELD "Breite-ü""#);
    assert_eq!(
        width["range"]["end"],
        json!({"line": line, "character": 21}),
        "{width:#}"
    );
}

#[test]
fn one_table_answers_from_the_database_it_is_asked_in() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let tree = schema(&mut session);
    let stock_uri = database(&tree, "stock")["uri"].clone();
    let sales_uri = database(&tree, "sales")["uri"].clone();

    let answer = session.request(
        "oxabl/schemaTable",
        json!({"database": stock_uri, "table": "ITEM"}),
    );
    let result = &answer["result"];
    assert_eq!(result["database"]["name"], json!("stock"), "{answer:#}");
    assert_eq!(result["database"]["uri"], stock_uri);
    assert_eq!(result["table"]["name"], json!("Item"));
    assert_eq!(names(&result["table"]["fields"]), ["OnHand"]);
    assert_on(&result["table"], STOCK, r#"ADD TABLE "Item""#);
    assert_on(
        &result["table"]["fields"][0],
        STOCK,
        r#"ADD FIELD "OnHand""#,
    );

    let answer = session.request(
        "oxabl/schemaTable",
        json!({"database": sales_uri, "table": "Item"}),
    );
    assert_eq!(names(&answer["result"]["table"]["fields"]), ["Price"]);

    // No such table: no answer, not an error.
    let answer = session.request(
        "oxabl/schemaTable",
        json!({"database": sales_uri, "table": "Bin"}),
    );
    assert_eq!(answer["result"], Value::Null, "{answer:#}");

    // A file that the configuration does not name is an error.
    let elsewhere = file_uri(&dir.path().join("other.df"));
    let answer = session.request(
        "oxabl/schemaTable",
        json!({"database": elsewhere, "table": "Item"}),
    );
    assert!(
        answer["error"]["message"]
            .as_str()
            .is_some_and(|m| m.contains("not a configured .df file")),
        "{answer:#}"
    );

    let answer = session.request("oxabl/schemaTable", json!({"table": "Item"}));
    assert!(answer.get("error").is_some(), "{answer:#}");
}

#[test]
fn a_changed_df_shows_at_the_next_request() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let before = schema(&mut session);
    assert_eq!(
        names(&database(&before, "stock")["tables"]),
        ["Item", "Bin", "Customer"]
    );

    let grown = format!("{STOCK}\nADD TABLE \"Pallet\"\n  AREA \"Data\"\n");
    std::fs::write(dir.path().join("db/stock.df"), &grown).unwrap();
    let after = schema(&mut session);
    let stock = database(&after, "stock");
    assert_eq!(
        names(&stock["tables"]),
        ["Item", "Bin", "Customer", "Pallet"]
    );
    assert_on(
        named(&stock["tables"], "Pallet"),
        &grown,
        r#"ADD TABLE "Pallet""#,
    );
}

#[track_caller]
fn empty_with_reason(root: &Path) -> String {
    let mut session = Session::start(root);
    let answer = schema(&mut session);
    assert_eq!(answer["databases"], json!([]), "{answer:#}");
    let reason = answer["reason"]
        .as_str()
        .unwrap_or_else(|| panic!("no reason: {answer:#}"));
    assert!(!reason.is_empty());
    reason.to_string()
}

#[test]
fn no_schema_configured_gives_an_empty_schema_that_says_why() {
    // No oxabl.toml at all.
    let bare = tempfile::tempdir().unwrap();
    let reason = empty_with_reason(bare.path());
    assert!(reason.contains("no oxabl.toml"), "{reason}");

    // An oxabl.toml with no `[workspace.schema]`.
    let unnamed = tempfile::tempdir().unwrap();
    std::fs::write(
        unnamed.path().join("oxabl.toml"),
        "[workspace]\nname = \"x\"\n",
    )
    .unwrap();
    let reason = empty_with_reason(unnamed.path());
    assert!(reason.contains("names no .df file"), "{reason}");

    // A schema directory with no `.df` in it.
    let empty_dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(empty_dir.path().join("db")).unwrap();
    std::fs::write(
        empty_dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"x\"\n[workspace.schema]\nfiles = [\"db\"]\n",
    )
    .unwrap();
    let reason = empty_with_reason(empty_dir.path());
    assert!(reason.contains("matched no .df files"), "{reason}");

    // An oxabl.toml that cannot be parsed.
    let broken = tempfile::tempdir().unwrap();
    std::fs::write(broken.path().join("oxabl.toml"), "[workspace\n").unwrap();
    let reason = empty_with_reason(broken.path());
    assert!(reason.contains("oxabl.toml cannot be used"), "{reason}");
}

/// A workspace whose `oxabl.toml` names `files`, with each `(path, text)`.
fn workspace(files: &str, texts: &[(&str, &str)]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(
        dir.path().join("oxabl.toml"),
        format!("[workspace]\nname = \"w\"\n[workspace.schema]\nfiles = {files}\n"),
    )
    .unwrap();
    for (name, text) in texts {
        let path = dir.path().join(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
    dir
}

#[track_caller]
fn problem_on<'a>(database: &'a Value, code: &str, text: &str, needle: &str) -> &'a Value {
    let line = line_of(text, needle);
    database["problems"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["code"] == json!(code) && p["range"]["start"]["line"] == json!(line))
        .unwrap_or_else(|| panic!("no {code} on line {line} (`{needle}`): {database:#}"))
}

const DELTA: &str = r#"ADD TABLE "Customer"
  AREA "Data"

ADD FIELD "Name" OF "Customer" AS character
  LABEL "N"

ADD TABLE "Order"
  AREA "Data"

DROP TABLE "Customer"

UPDATE FIELD "Name" OF "Customer"
  LABEL "Changed"

RENAME TABLE "Order" TO "Orders"
"#;

#[test]
fn a_directive_the_loader_does_not_apply_makes_the_file_incomplete() {
    let dir = workspace(r#"["delta.df"]"#, &[("delta.df", DELTA)]);
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);
    let delta = database(&answer, "delta");
    assert_eq!(delta["complete"], json!(false), "{delta:#}");
    for needle in ["DROP TABLE", "UPDATE FIELD", "RENAME TABLE"] {
        let problem = problem_on(delta, "SCHEMA0020", DELTA, needle);
        assert_eq!(problem["severity"], json!("warning"), "{problem:#}");
    }
    // The tree still shows what the `ADD` directives say.
    assert_eq!(names(&delta["tables"]), ["Customer", "Order"]);
}

const LATE: &str = "ADD FIELD \"Late\" OF \"Customer\" AS logical\n  ORDER 99\n\
                    ADD INDEX \"ByLate\" ON \"Customer\"\n  INDEX-FIELD \"Late\" ASCENDING\n";
const NOWHERE: &str = "ADD FIELD \"f\" OF \"Orphan\" AS integer\n";
const BASE: &str = "ADD TABLE \"Customer\"\n  AREA \"Data\"\n";

#[test]
fn a_field_of_a_table_no_earlier_file_declares_is_the_loaders_error() {
    // `late.df` comes before the file that declares Customer, and no file
    // declares Orphan: the loader drops both, and so does the tree.
    let dir = workspace(
        r#"["late.df", "nowhere.df", "base.df", "again.df"]"#,
        &[
            ("late.df", LATE),
            ("nowhere.df", NOWHERE),
            ("base.df", BASE),
            ("again.df", LATE),
        ],
    );
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);

    let late = database(&answer, "late");
    assert_eq!(late["tables"], json!([]), "{late:#}");
    assert_eq!(late["complete"], json!(false));
    let problem = problem_on(late, "SCHEMA0001", LATE, r#"ADD FIELD "Late""#);
    assert_eq!(problem["severity"], json!("error"));
    problem_on(late, "SCHEMA0001", LATE, r#"ADD INDEX "ByLate""#);

    let nowhere = database(&answer, "nowhere");
    assert_eq!(nowhere["tables"], json!([]), "{nowhere:#}");
    assert_eq!(nowhere["complete"], json!(false));
    problem_on(nowhere, "SCHEMA0001", NOWHERE, r#"ADD FIELD "f""#);

    // After the declaring file, the same text is accepted.
    let again = database(&answer, "again");
    assert_eq!(again["complete"], json!(true), "{again:#}");
    let customer = named(&again["tables"], "Customer");
    assert_eq!(customer["declared"], json!(false));
    assert_eq!(names(&customer["fields"]), ["Late"]);
    assert_eq!(names(&customer["indexes"]), ["ByLate"]);

    // The detail answer reads the earlier files for the same decision.
    let uri = again["uri"].clone();
    let answer = session.request(
        "oxabl/schemaTable",
        json!({"database": uri, "table": "customer"}),
    );
    assert_eq!(
        names(&answer["result"]["table"]["fields"]),
        ["Late"],
        "{answer:#}"
    );
    let uri = late["uri"].clone();
    let answer = session.request(
        "oxabl/schemaTable",
        json!({"database": uri, "table": "customer"}),
    );
    assert_eq!(answer["result"], Value::Null, "{answer:#}");
}

const TWICE: &str = r#"ADD TABLE "T"
  AREA "Data"

ADD FIELD "f" OF "T" AS character
  LABEL "first"

ADD FIELD "f" OF "T" AS character
  LABEL "second"

ADD FIELD "g" OF "T" AS character

ADD FIELD "g" OF "T" AS integer

ADD INDEX "i" ON "T"
  INDEX-FIELD "f" ASCENDING

ADD INDEX "i" ON "T"
  INDEX-FIELD "g" ASCENDING
"#;

#[test]
fn a_declaration_twice_in_one_file_follows_the_loaders_rules() {
    let dir = workspace(r#"["twice.df"]"#, &[("twice.df", TWICE)]);
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);
    let twice = database(&answer, "twice");
    let t = named(&twice["tables"], "T");
    assert_eq!(names(&t["fields"]), ["f", "g"]);
    assert_eq!(named(&t["fields"], "f")["label"], json!("second"));
    // The second `f` replaces the first with a warning, on its own line.
    let second_f = last_line_of(TWICE, r#"ADD FIELD "f""#);
    assert!(has_problem(twice, "SCHEMA0011", second_f), "{twice:#}");
    // `g` twice with two types: an error, and no type.
    let g = named(&t["fields"], "g");
    assert_eq!(g.get("dataType"), None, "{g:#}");
    let conflict = problem_on(
        twice,
        "SCHEMA0012",
        TWICE,
        r#"ADD FIELD "g" OF "T" AS integer"#,
    );
    assert_eq!(conflict["severity"], json!("error"));
    assert_eq!(twice["complete"], json!(false));
    let second_i = last_line_of(TWICE, r#"ADD INDEX "i""#);
    assert!(has_problem(twice, "SCHEMA0011", second_i), "{twice:#}");
}

fn last_line_of(text: &str, needle: &str) -> u64 {
    text.lines()
        .enumerate()
        .filter(|(_, line)| line.contains(needle))
        .last()
        .unwrap()
        .0 as u64
}

fn has_problem(database: &Value, code: &str, line: u64) -> bool {
    database["problems"]
        .as_array()
        .unwrap()
        .iter()
        .any(|p| p["code"] == json!(code) && p["range"]["start"]["line"] == json!(line))
}

#[test]
fn a_file_named_twice_is_one_database() {
    let dir = workspace(r#"["db/a.df", "db", "./db/a.df"]"#, &[("db/a.df", BASE)]);
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);
    assert_eq!(names(&answer["databases"]), ["a"], "{answer:#}");
    let warnings = answer["warnings"].as_array().unwrap();
    assert_eq!(warnings.len(), 2, "{answer:#}");
    assert!(
        warnings
            .iter()
            .all(|w| w.as_str().unwrap().contains("named more than once")),
        "{answer:#}"
    );
}

const NAMED: &str = "ADD TABLE \"Customer\"\n  AREA \"Data\"\n\n\
                     ADD FIELD \"Name\" OF \"Customer\" AS character\n";
const EXTRA: &str = "ADD FIELD \"Extra\" OF \"Customer\" AS character\n";
const READS: &str = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                     MESSAGE Customer.Name Customer.Extra.\n";

/// The definitions of the name one character into `needle` in `file`.
fn definitions(session: &mut Session, file: &Path, text: &str, needle: &str) -> Value {
    let at = text.find(needle).unwrap() + 1;
    let line = text[..at].matches('\n').count();
    let character = at - text[..at].rfind('\n').map_or(0, |n| n + 1);
    let answer = session.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": file_uri(file)},
            "position": {"line": line, "character": character}
        }),
    );
    assert!(answer.get("error").is_none(), "{answer:#}");
    answer["result"].clone()
}

#[test]
fn a_file_named_twice_is_read_at_its_last_place_by_the_tree_and_the_analysis() {
    // `db` names the file again. Read once, at its last place, the file that
    // adds `Extra` comes after the one that declares `Customer`, before or
    // after the directory: the tree shows `Extra` and the analysis knows it.
    for (files, base, ext, order) in [
        (r#"["db/b.df", "db"]"#, "a.df", "b.df", ["a", "b"]),
        (
            r#"["db", "db/a_ext.df"]"#,
            "z_base.df",
            "a_ext.df",
            ["z_base", "a_ext"],
        ),
    ] {
        let base_path = format!("db/{base}");
        let ext_path = format!("db/{ext}");
        let dir = workspace(
            files,
            &[(&base_path, NAMED), (&ext_path, EXTRA), ("main.p", READS)],
        );
        let mut session = Session::start(dir.path());
        let answer = schema(&mut session);
        assert_eq!(names(&answer["databases"]), order, "{answer:#}");
        let extended = database(&answer, order[1]);
        assert_eq!(extended["complete"], json!(true), "{extended:#}");
        let customer = named(&extended["tables"], "Customer");
        assert_eq!(names(&customer["fields"]), ["Extra"]);
        assert!(
            answer["warnings"][0]
                .as_str()
                .is_some_and(|w| w.contains(ext) && w.contains("at its last place")),
            "{answer:#}"
        );

        let main = dir.path().join("main.p");
        session.open(&main, READS);
        for needle in ["Name Customer", "Extra."] {
            let found = definitions(&mut session, &main, READS, needle);
            assert!(
                found.as_array().is_some_and(|l| l.len() == 1),
                "`{needle}` resolves with {files}: {found:#}"
            );
        }
    }
}

#[test]
fn an_extended_table_is_placed_at_its_first_add() {
    const INDEX_FIRST: &str = "ADD INDEX \"ByName\" ON \"Customer\"\n  INDEX-FIELD \"Name\" ASCENDING\n\n\
                               ADD FIELD \"z\" OF \"Customer\" AS integer\n";
    let dir = workspace(
        r#"["a.df", "ext.df"]"#,
        &[("a.df", NAMED), ("ext.df", INDEX_FIRST)],
    );
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);
    let customer = named(&database(&answer, "ext")["tables"], "Customer");
    assert_eq!(customer["declared"], json!(false), "{customer:#}");
    assert_on(customer, INDEX_FIRST, r#"ADD INDEX "ByName""#);
}

#[test]
fn the_answer_names_the_directories_to_watch() {
    let dir = workspace(r#"[]"#, &[("db/a.df", BASE)]);
    std::fs::create_dir(dir.path().join("empty")).unwrap();
    let outside = tempfile::tempdir().unwrap();
    std::fs::write(
        dir.path().join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"w\"\n[workspace.schema]\nfiles = [\"db/a.df\", \"empty\", {:?}]\n",
            outside.path()
        ),
    )
    .unwrap();
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);
    let watched: Vec<std::path::PathBuf> = answer["directories"]
        .as_array()
        .unwrap_or_else(|| panic!("no directories: {answer:#}"))
        .iter()
        .map(|d| std::fs::canonicalize(d.as_str().unwrap()).unwrap())
        .collect();
    for wanted in [
        dir.path().join("db"),
        dir.path().join("empty"),
        outside.path().to_path_buf(),
    ] {
        let wanted = std::fs::canonicalize(&wanted).unwrap();
        assert!(watched.contains(&wanted), "{wanted:?} not in {watched:?}");
    }
}

#[test]
fn a_schema_directory_without_a_df_is_a_warning_beside_the_databases() {
    let dir = workspace(r#"["db/a.df", "empty"]"#, &[("db/a.df", BASE)]);
    std::fs::create_dir(dir.path().join("empty")).unwrap();
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);
    assert_eq!(names(&answer["databases"]), ["a"]);
    assert_eq!(answer["reason"], Value::Null);
    assert!(
        answer["warnings"][0]
            .as_str()
            .is_some_and(|w| w.contains("matched no .df files")),
        "{answer:#}"
    );
}

#[test]
fn a_path_that_is_not_utf8_is_a_problem_not_a_broken_answer() {
    use std::os::unix::ffi::OsStrExt;
    let dir = workspace(r#"["db"]"#, &[]);
    std::fs::create_dir(dir.path().join("db")).unwrap();
    let name = std::ffi::OsStr::from_bytes(b"caf\xe9.df");
    std::fs::write(dir.path().join("db").join(name), BASE).unwrap();
    let mut session = Session::start(dir.path());
    let answer = schema(&mut session);
    let database = &answer["databases"][0];
    assert_eq!(database["complete"], json!(false), "{answer:#}");
    assert_eq!(names(&database["tables"]), ["Customer"]);
    let problem = &database["problems"][0];
    assert!(
        problem["message"]
            .as_str()
            .is_some_and(|m| m.contains("not UTF-8")),
        "{answer:#}"
    );
    assert_eq!(problem["code"], json!("SCHEMA0021"), "{answer:#}");
}

/// Make `path` a named pipe: a read of it waits for a writer.
fn fifo(path: &Path) {
    let _ = std::fs::remove_file(path);
    let made = std::process::Command::new("mkfifo")
        .arg(path)
        .status()
        .unwrap();
    assert!(made.success());
}

/// Write `text` to the named pipe `path` once a reader holds it open.
fn feed(path: &Path, text: &str) -> bool {
    use std::io::Write;
    use std::os::unix::fs::OpenOptionsExt;
    const O_NONBLOCK: i32 = 0o4000;
    match std::fs::OpenOptions::new()
        .write(true)
        .custom_flags(O_NONBLOCK)
        .open(path)
    {
        Ok(mut writer) => {
            let _ = writer.write_all(text.as_bytes());
            true
        }
        Err(_) => false,
    }
}

#[test]
fn one_table_reads_only_its_own_file() {
    let dir = workspace(
        r#"["db/slow.df", "db/sales.df"]"#,
        &[("db/slow.df", BASE), ("db/sales.df", SALES)],
    );
    let mut session = Session::start(dir.path());
    let tree = schema(&mut session);
    let sales_uri = database(&tree, "sales")["uri"].clone();
    // A read of the other file would now wait for ever.
    fifo(&dir.path().join("db/slow.df"));
    let answer = session.request(
        "oxabl/schemaTable",
        json!({"database": sales_uri, "table": "Item"}),
    );
    assert_eq!(
        names(&answer["result"]["table"]["fields"]),
        ["Price"],
        "{answer:#}"
    );
    // Let anything still waiting on the pipe finish.
    feed(&dir.path().join("db/slow.df"), "");
}

#[test]
fn a_newer_tree_request_replaces_an_older_one() {
    // The older request waits on the first file: it stops before the next.
    replaced_while_reading(r#"["db/slow.df", "db/sales.df"]"#, &["slow", "sales"]);
}

#[test]
fn a_tree_request_replaced_during_its_last_file_sends_no_tree() {
    replaced_while_reading(r#"["db/sales.df", "db/slow.df"]"#, &["sales", "slow"]);
}

/// Two tree requests while `db/slow.df` is a pipe no one has written yet:
/// the first answers `ContentModified`, the second the tree.
fn replaced_while_reading(files: &str, order: &[&str]) {
    let dir = workspace(files, &[("db/slow.df", BASE), ("db/sales.df", SALES)]);
    let mut session = Session::start(dir.path());
    schema(&mut session);
    let pipe = dir.path().join("db/slow.df");
    fifo(&pipe);
    // Both requests wait on the pipe; the first is replaced by the second.
    let first = session.next_request_id();
    session.send(json!({"jsonrpc": "2.0", "id": first, "method": "oxabl/schema", "params": {}}));
    std::thread::sleep(std::time::Duration::from_millis(300));
    let second = first + 1;
    session.send(json!({"jsonrpc": "2.0", "id": second, "method": "oxabl/schema", "params": {}}));
    std::thread::sleep(std::time::Duration::from_millis(300));
    let (mut old, mut new) = (None, None);
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
    while (old.is_none() || new.is_none()) && std::time::Instant::now() < deadline {
        feed(&pipe, BASE);
        while let Some(message) = session.try_next(std::time::Duration::from_millis(100)) {
            if message["id"] == json!(first) {
                old = Some(message);
            } else if message["id"] == json!(second) {
                new = Some(message);
            }
        }
    }
    let old = old.expect("no answer to the first request");
    let new = new.expect("no answer to the second request");
    assert_eq!(old["error"]["code"], json!(-32801), "{old:#}");
    assert_eq!(names(&new["result"]["databases"]), order, "{new:#}");
}
