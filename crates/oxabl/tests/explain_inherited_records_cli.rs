//! A statement in a subclass on a buffer or temp-table that a superclass
//! defines has the database effect of the table behind it. The tests run
//! `oxabl explain` and `oxabl check` over a class hierarchy of three files and
//! read the effects of one method.

use std::path::Path;
use std::process::Command;

use serde_json::Value;

const SCHEMA: &str = "ADD TABLE \"customer\"\nADD FIELD \"name\" OF \"customer\" AS character\nADD FIELD \"id\" OF \"customer\" AS integer\nADD INDEX \"id\" ON \"customer\"\n  UNIQUE\n  PRIMARY\n  INDEX-FIELD \"id\" ASCENDING\nADD TABLE \"item\"\nADD FIELD \"sku\" OF \"item\" AS character\n";

const ROOT: &str = "\
CLASS pkg.Root:
    DEFINE PROTECTED TEMP-TABLE ttRoot NO-UNDO FIELD num AS INTEGER.
    DEFINE PROTECTED BUFFER rootCust_ FOR customer.
    DEFINE PRIVATE BUFFER secret_ FOR item.
    DEFINE PRIVATE TEMP-TABLE ttSecret NO-UNDO FIELD num AS INTEGER.
END CLASS.
";

const BASE: &str = "\
USING pkg.*.
CLASS pkg.Base INHERITS Root:
    DEFINE PROTECTED BUFFER rootTt_ FOR ttRoot.
    DEFINE PROTECTED BUFFER baseItem_ FOR item.
    DEFINE PROTECTED BUFFER hidesTt_ FOR ttSecret.
END CLASS.
";

fn workspace(files: &[(&str, &str)]) -> tempfile::TempDir {
    workspace_with("[workspace.sources]\ninclude_paths = [\".\"]\n", files)
}

/// A workspace whose `oxabl.toml` holds `sources` besides the schema.
fn workspace_with(sources: &str, files: &[(&str, &str)]) -> tempfile::TempDir {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(
        tmp.path().join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"fixture\"\n{sources}[workspace.schema]\nfiles = [\"schema.df\"]\n"
        ),
    )
    .unwrap();
    std::fs::write(tmp.path().join("schema.df"), SCHEMA).unwrap();
    for (name, text) in files {
        let path = tmp.path().join(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
    tmp
}

fn oxabl(dir: &Path, args: &[&str]) -> Value {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(args)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{args:?}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).unwrap()
}

/// One direct effect of the method `Run`.
#[derive(Debug, PartialEq, Eq)]
struct Effect {
    line: u64,
    table: String,
    lock: String,
    reason: Option<String>,
}

fn run_effects(dir: &Path, file: &str) -> Vec<Effect> {
    let value = oxabl(dir, &["explain", file, "--json"]);
    let unit = value["units"]
        .as_array()
        .unwrap()
        .iter()
        .find(|u| u["name"] == "Run")
        .expect("a unit called Run");
    unit["effects"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| Effect {
            line: e["line"].as_u64().unwrap(),
            table: e["table"].as_str().unwrap().to_string(),
            lock: e["lock"].as_str().unwrap().to_string(),
            reason: e["reason"].as_str().map(str::to_string),
        })
        .collect()
}

fn table_at(effects: &[Effect], line: u64) -> &str {
    &effects
        .iter()
        .find(|e| e.line == line)
        .unwrap_or_else(|| panic!("no effect at line {line}: {effects:#?}"))
        .table
}

/// A subclass of `pkg.Base` whose method `Run` holds `statements` from line 4,
/// after one line for each line of `header_extra`.
fn sub(header_extra: &str, statements: &[&str]) -> String {
    let mut text = format!(
        "USING pkg.*.\nCLASS app.Sub INHERITS Base:\n{header_extra}    METHOD PUBLIC VOID Run():\n"
    );
    for s in statements {
        text.push_str("        ");
        text.push_str(s);
        text.push('\n');
    }
    text.push_str("    END METHOD.\nEND CLASS.\n");
    text
}

#[test]
fn records_of_a_superclass_and_a_grand_superclass_are_attributed() {
    let text = sub(
        "",
        &[
            "FIND FIRST rootCust_ NO-LOCK NO-ERROR.",
            "FOR EACH rootTt_: END.",
            "FOR EACH baseItem_ NO-LOCK: END.",
            "FOR EACH ttRoot: END.",
            "IF CAN-FIND(FIRST rootCust_ NO-LOCK) THEN RETURN.",
        ],
    );
    let tmp = workspace(&[
        ("pkg/Root.cls", ROOT),
        ("pkg/Base.cls", BASE),
        ("app/Sub.cls", &text),
    ]);
    let effects = run_effects(tmp.path(), "app/Sub.cls");
    assert_eq!(table_at(&effects, 4), "customer", "{effects:#?}");
    assert_eq!(table_at(&effects, 5), "tt:ttRoot", "{effects:#?}");
    assert_eq!(table_at(&effects, 6), "item", "{effects:#?}");
    assert_eq!(table_at(&effects, 7), "tt:ttRoot", "{effects:#?}");
    assert_eq!(table_at(&effects, 8), "customer", "{effects:#?}");
    let customer = effects.iter().find(|e| e.line == 4).unwrap();
    assert_eq!(customer.lock, "no-lock");
    assert_eq!(
        customer.reason.as_deref().map(|r| r.starts_with("primary")),
        Some(true)
    );
    assert_eq!(effects.iter().filter(|e| e.table == "customer").count(), 2);
}

#[test]
fn the_summary_of_the_method_names_the_inherited_tables() {
    let text = sub("", &["FIND FIRST rootCust_ NO-LOCK NO-ERROR."]);
    let tmp = workspace(&[
        ("pkg/Root.cls", ROOT),
        ("pkg/Base.cls", BASE),
        ("app/Sub.cls", &text),
    ]);
    let value = oxabl(tmp.path(), &["explain", "app/Sub.cls", "--json"]);
    let unit = value["units"]
        .as_array()
        .unwrap()
        .iter()
        .find(|u| u["name"] == "Run")
        .unwrap();
    assert_eq!(unit["summary"]["complete"], true);
    let tables: Vec<&str> = unit["summary"]["effects"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| e["table"].as_str().unwrap())
        .collect();
    assert_eq!(tables, ["customer"]);
}

#[test]
fn private_members_of_a_superclass_stay_unattributed() {
    let text = sub(
        "",
        &[
            "FOR EACH secret_ NO-LOCK: END.",
            "FOR EACH ttSecret: END.",
            "FOR EACH hidesTt_: END.",
        ],
    );
    let tmp = workspace(&[
        ("pkg/Root.cls", ROOT),
        ("pkg/Base.cls", BASE),
        ("app/Sub.cls", &text),
    ]);
    let effects = run_effects(tmp.path(), "app/Sub.cls");
    // A buffer `FOR` a table the subclass cannot reach is no table either.
    for (line, table) in [(4, "?:secret_"), (5, "?:ttSecret"), (6, "?:ttSecret")] {
        assert_eq!(table_at(&effects, line), table, "{effects:#?}");
        let effect = effects.iter().find(|e| e.line == line).unwrap();
        assert_eq!(effect.reason.as_deref(), Some("unknown:unknown-table"));
    }
}

#[test]
fn a_definition_of_the_subclass_hides_an_inherited_one() {
    let text = sub(
        "    DEFINE BUFFER baseItem_ FOR customer.\n    DEFINE TEMP-TABLE rootCust_ NO-UNDO FIELD num AS INTEGER.\n",
        &[
            "FOR EACH baseItem_ NO-LOCK: END.",
            "FOR EACH rootCust_: END.",
        ],
    );
    let tmp = workspace(&[
        ("pkg/Root.cls", ROOT),
        ("pkg/Base.cls", BASE),
        ("app/Sub.cls", &text),
    ]);
    let effects = run_effects(tmp.path(), "app/Sub.cls");
    // Two declarations come first, so the statements start at line 6.
    assert_eq!(table_at(&effects, 6), "customer", "{effects:#?}");
    assert_eq!(table_at(&effects, 7), "tt:rootCust_", "{effects:#?}");
}

#[test]
fn an_unreadable_superclass_leaves_its_records_unattributed() {
    let text = sub("", &["FIND FIRST rootCust_ NO-LOCK NO-ERROR."]);
    let tmp = workspace(&[("pkg/Base.cls", BASE), ("app/Sub.cls", &text)]);
    // `pkg.Root`, the superclass of `pkg.Base`, is in no file.
    let effects = run_effects(tmp.path(), "app/Sub.cls");
    assert_eq!(table_at(&effects, 4), "?:rootCust_", "{effects:#?}");
    assert_eq!(effects[0].reason.as_deref(), Some("unknown:unknown-table"));
}

#[test]
fn inherited_records_are_no_undefined_symbol() {
    let text = sub(
        "",
        &[
            "FIND FIRST rootCust_ NO-LOCK NO-ERROR.",
            "FOR EACH rootTt_: MESSAGE rootTt_.num. END.",
            "FOR EACH baseItem_ NO-LOCK: MESSAGE baseItem_.sku. END.",
            "MESSAGE rootCust_.name ttRoot.num.",
            "CREATE ttRoot.",
        ],
    );
    let tmp = workspace(&[
        ("pkg/Root.cls", ROOT),
        ("pkg/Base.cls", BASE),
        ("app/Sub.cls", &text),
    ]);
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .args(["check", "app/Sub.cls", "--json", "--no-format"])
        .output()
        .unwrap();
    let report = String::from_utf8_lossy(&output.stdout);
    assert!(!report.contains("LINT0001"), "{report}");
    assert!(
        output.status.success(),
        "{report}{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// The one effect of `Run` at line 4 of a subclass of `parent` with `statement`.
fn effect_of(files: &[(&str, &str)], parent: &str, statement: &str) -> Effect {
    let text = format!(
        "USING pkg.*.\nCLASS app.Sub INHERITS {parent}:\n    METHOD PUBLIC VOID Run():\n        {statement}\n    END METHOD.\nEND CLASS.\n"
    );
    let mut all = files.to_vec();
    all.push(("app/Sub.cls", &text));
    let tmp = workspace(&all);
    let mut effects = run_effects(tmp.path(), "app/Sub.cls");
    assert_eq!(effects.len(), 1, "{effects:#?}");
    effects.remove(0)
}

const FIND_CUSTOMER: &str = "FIND FIRST customer NO-LOCK NO-ERROR.";

#[test]
fn a_schema_name_is_unknown_below_a_class_that_is_not_read() {
    // `pkg.Root` is in no file: it may define a temp-table `customer`.
    let missing = effect_of(&[("pkg/Base.cls", BASE)], "Base", FIND_CUSTOMER);
    assert_eq!(missing.table, "?:customer", "{missing:#?}");
    assert_eq!(missing.reason.as_deref(), Some("unknown:unknown-table"));

    // A class the AVM supplies is not read either.
    let shipped = effect_of(
        &[(
            "pkg/Base.cls",
            "CLASS pkg.Base INHERITS Progress.Lang.AppError:\nEND CLASS.\n",
        )],
        "Base",
        FIND_CUSTOMER,
    );
    assert_eq!(shipped.table, "?:customer", "{shipped:#?}");

    // The control: a chain that is read to its root keeps the schema table.
    let read = effect_of(
        &[("pkg/Root.cls", ROOT), ("pkg/Base.cls", BASE)],
        "Base",
        FIND_CUSTOMER,
    );
    assert_eq!(read.table, "customer", "{read:#?}");
}

#[test]
fn without_a_search_path_a_superclass_is_not_missed() {
    let text = sub("", &[FIND_CUSTOMER]);
    let tmp = workspace_with("", &[("app/Sub.cls", &text)]);
    let effects = run_effects(tmp.path(), "app/Sub.cls");
    assert_eq!(table_at(&effects, 4), "customer", "{effects:#?}");
}

#[test]
fn a_buffer_is_bound_in_the_class_that_declares_it() {
    let top = "CLASS pkg.Top:\n    DEFINE PROTECTED BUFFER gb FOR customer.\nEND CLASS.\n";
    // A temp-table of a class below `pkg.Top` does not hide the table `gb` is for.
    let below = "USING pkg.*.\nCLASS pkg.Mid INHERITS Top:\n    DEFINE PROTECTED TEMP-TABLE customer NO-UNDO FIELD z AS INTEGER.\nEND CLASS.\n";
    let files = [("pkg/Top.cls", top), ("pkg/Mid.cls", below)];
    let buffer = effect_of(&files, "Mid", "FIND FIRST gb NO-LOCK NO-ERROR.");
    assert_eq!(buffer.table, "customer", "{buffer:#?}");
    // The temp-table is what its own name means.
    let own = effect_of(&files, "Mid", "FIND FIRST customer NO-ERROR.");
    assert_eq!(own.table, "tt:customer", "{own:#?}");

    // One of a class above it does.
    let above = "CLASS pkg.Up:\n    DEFINE PROTECTED TEMP-TABLE customer NO-UNDO FIELD z AS INTEGER.\nEND CLASS.\n";
    let top = "USING pkg.*.\nCLASS pkg.Top2 INHERITS Up:\n    DEFINE PROTECTED BUFFER gb FOR customer.\nEND CLASS.\n";
    let hidden = effect_of(
        &[("pkg/Up.cls", above), ("pkg/Top2.cls", top)],
        "Top2",
        "FIND FIRST gb NO-ERROR.",
    );
    assert_eq!(hidden.table, "tt:customer", "{hidden:#?}");
}

#[test]
fn a_private_buffer_does_not_hide_the_table_another_buffer_is_for() {
    let root = "CLASS pkg.Root:\n    DEFINE PRIVATE BUFFER customer FOR customer.\n    DEFINE PROTECTED BUFFER bCust FOR customer.\nEND CLASS.\n";
    let found = effect_of(
        &[("pkg/Root.cls", root)],
        "Root",
        "FIND FIRST bCust NO-LOCK NO-ERROR.",
    );
    assert_eq!(found.table, "customer", "{found:#?}");
    // The name of the private buffer itself is not the subclass's to use.
    let named = effect_of(&[("pkg/Root.cls", root)], "Root", FIND_CUSTOMER);
    assert_eq!(named.table, "?:customer", "{named:#?}");
}

#[test]
fn an_inherited_buffer_is_no_target_of_a_for() {
    let root = "CLASS pkg.Root:\n    DEFINE PROTECTED BUFFER item FOR customer.\nEND CLASS.\n";
    let text = sub(
        "    DEFINE BUFFER x FOR item.\n",
        &["FIND FIRST x NO-LOCK NO-ERROR."],
    );
    let text = text.replace("INHERITS Base", "INHERITS Root");
    let tmp = workspace(&[("pkg/Root.cls", root), ("app/Sub.cls", &text)]);
    let effects = run_effects(tmp.path(), "app/Sub.cls");
    // `FOR item` means the table `item`, as it does where no superclass exists.
    assert_eq!(table_at(&effects, 5), "item", "{effects:#?}");
}

#[test]
fn a_lock_rule_judges_an_inherited_database_buffer() {
    let text = sub("", &["FIND FIRST rootCust_.", "FIND FIRST ttRoot."]);
    let tmp = workspace(&[
        ("pkg/Root.cls", ROOT),
        ("pkg/Base.cls", BASE),
        ("app/Sub.cls", &text),
    ]);
    let mut config = std::fs::read_to_string(tmp.path().join("oxabl.toml")).unwrap();
    config.push_str("[workspace.lint]\nenable = [\"strict.locks\"]\n");
    std::fs::write(tmp.path().join("oxabl.toml"), config).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .args(["check", "app/Sub.cls", "--json", "--no-format"])
        .output()
        .unwrap();
    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    let lines: Vec<u64> = report["diagnostics"]
        .as_array()
        .unwrap_or_else(|| panic!("{report}"))
        .iter()
        .filter(|f| f["code"] == "LK001")
        .map(|f| f["start"]["line"].as_u64().unwrap())
        .collect();
    // The read of the database table takes the default lock; the temp-table
    // read takes none.
    assert_eq!(lines, [4], "{report}");
}
