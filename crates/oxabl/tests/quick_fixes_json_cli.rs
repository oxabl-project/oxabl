//! Quick fixes in the JSON output of `oxabl check` (the NO-UNDO fix).
//!
//! Black-box: every case drives the `oxabl` binary and reads its JSON. Every
//! run here is a run without `--fix`, so each run also proves that the files
//! stay byte for byte as the test wrote them. All fixtures are synthetic ABL.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde_json::{Value, json};
use tempfile::TempDir;

const CONF_TX: &str =
    "[workspace]\nname = \"fixes\"\n\n[workspace.lint]\nenable = [\"strict.transactions\"]\n";
const CONF_TX_INCLUDE: &str = "[workspace]\nname = \"fixes\"\n\n[workspace.sources]\ninclude_paths = [\".\"]\n\n[workspace.lint]\nenable = [\"strict.transactions\"]\n";
const CONF_DEFAULT: &str = "[workspace]\nname = \"fixes\"\n";
const CONF_TX_SCHEMA: &str = "[workspace]\nname = \"fixes\"\n\n[workspace.schema]\nfiles = [\"schema.df\"]\n\n[workspace.lint]\nenable = [\"strict.transactions\"]\n";
const SCHEMA_DF: &str = "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\nADD FIELD \"Name\" OF \"Customer\" AS character\nADD TABLE \"Order\"\nADD FIELD \"OrderNum\" OF \"Order\" AS integer\nADD FIELD \"CustNum\" OF \"Order\" AS integer\n";

const J1_TEXT: &str = "DEFINE VARIABLE iCount AS INTEGER.\nDISPLAY iCount.\n";

struct Run {
    exit: Option<i32>,
    stdout: String,
    stderr: String,
}

impl Run {
    fn json(&self) -> Value {
        serde_json::from_str(&self.stdout)
            .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{}", self.stdout))
    }
}

struct Project {
    dir: TempDir,
}

impl Project {
    fn new(config: &str) -> Self {
        let dir = TempDir::new().unwrap();
        let project = Project { dir };
        project.write("oxabl.toml", config.as_bytes());
        project
    }

    fn root(&self) -> &Path {
        self.dir.path()
    }

    fn write(&self, rel: &str, bytes: impl AsRef<[u8]>) {
        let path = self.root().join(rel);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }

    fn read(&self, rel: &str) -> String {
        fs::read_to_string(self.root().join(rel)).unwrap()
    }

    /// Every file of the project, by relative path.
    fn snapshot(&self) -> BTreeMap<PathBuf, Vec<u8>> {
        fn walk(base: &Path, dir: &Path, out: &mut BTreeMap<PathBuf, Vec<u8>>) {
            for entry in fs::read_dir(dir).unwrap() {
                let path = entry.unwrap().path();
                if path.is_dir() {
                    walk(base, &path, out);
                } else {
                    out.insert(
                        path.strip_prefix(base).unwrap().to_path_buf(),
                        fs::read(&path).unwrap(),
                    );
                }
            }
        }
        let mut out = BTreeMap::new();
        walk(self.root(), self.root(), &mut out);
        out
    }

    fn run_raw(&self, args: &[&str]) -> Run {
        let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .current_dir(self.root())
            .arg("check")
            .args(args)
            .output()
            .unwrap();
        Run {
            exit: output.status.code(),
            stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        }
    }

    /// Run `oxabl check <args>` in the project. A run without `--fix` must
    /// leave every file unchanged, so each run checks that.
    fn check(&self, args: &[&str]) -> Run {
        let before = self.snapshot();
        let run = self.run_raw(args);
        // Without `--fix`, `oxabl check` never writes a file.
        let after = self.snapshot();
        let changed: Vec<_> = before
            .iter()
            .filter(|(p, b)| after.get(*p) != Some(*b))
            .map(|(p, _)| p)
            .collect();
        assert!(
            changed.is_empty() && before.len() == after.len(),
            "a run without --fix changed files {changed:?}\nstderr: {}",
            run.stderr
        );
        run
    }

    fn check_json(&self, args: &[&str]) -> Value {
        let mut all = vec!["--json", "--no-format"];
        all.extend_from_slice(args);
        let run = self.check(&all);
        assert!(
            run.exit == Some(0) || run.exit == Some(1),
            "unexpected exit {:?}\nstdout: {}\nstderr: {}",
            run.exit,
            run.stdout,
            run.stderr
        );
        run.json()
    }
}

/// The rows of one array of the report; the array must exist.
fn rows<'a>(report: &'a Value, key: &str) -> &'a [Value] {
    report[key]
        .as_array()
        .unwrap_or_else(|| panic!("`{key}` is not an array in:\n{report}"))
}

fn with_code<'a>(report: &'a Value, code: &str) -> Vec<&'a Value> {
    rows(report, "diagnostics")
        .iter()
        .filter(|row| row["code"] == code)
        .collect()
}

fn tx006(report: &Value) -> Vec<&Value> {
    with_code(report, "TX006")
}

/// The row of a path that ends with `name` (a run on `.` spells `./name`).
fn row_for_path<'a>(rows: &[&'a Value], name: &str) -> &'a Value {
    let found: Vec<_> = rows
        .iter()
        .copied()
        .filter(|row| {
            row["path"]
                .as_str()
                .is_some_and(|p| p.trim_start_matches("./") == name)
        })
        .collect();
    assert_eq!(found.len(), 1, "one row for {name} in {rows:?}");
    found[0]
}

/// No row of any array of the report has a `fixes` key.
fn assert_no_fixes_anywhere(report: &Value) {
    for key in ["diagnostics", "include_diagnostics", "preproc"] {
        let Some(array) = report.get(key) else {
            continue;
        };
        for row in array.as_array().unwrap() {
            assert!(
                row.get("fixes").is_none(),
                "`{key}` row has a `fixes` key: {row}"
            );
        }
    }
}

/// The text after the one edit of the one fix of a row, applied to `text`.
fn apply_only_edit(row: &Value, text: &str) -> String {
    let fixes = row["fixes"]
        .as_array()
        .unwrap_or_else(|| panic!("row has no `fixes`: {row}"));
    assert_eq!(fixes.len(), 1, "one fix in {row}");
    let edits = fixes[0]["edits"].as_array().unwrap();
    assert_eq!(edits.len(), 1, "one edit in {row}");
    let start = edits[0]["span"]["start"].as_u64().unwrap() as usize;
    let end = edits[0]["span"]["end"].as_u64().unwrap() as usize;
    let new_text = edits[0]["new_text"].as_str().unwrap();
    format!("{}{}{}", &text[..start], new_text, &text[end..])
}

/// A fix is offered for a plain definition. Cases of "not offered" call this,
/// so a build that never offers a fix cannot pass them.
fn assert_fixes_are_live() {
    let project = Project::new(CONF_TX);
    project.write("main.p", J1_TEXT);
    let report = project.check_json(&["main.p"]);
    let rows = tx006(&report);
    assert_eq!(rows.len(), 1, "{report}");
    assert!(
        rows[0].get("fixes").is_some(),
        "no fix is offered for a plain definition: {report}"
    );
}

/// One file of CONF_TX, checked as `name`.
fn tx_report(name: &str, text: &str) -> Value {
    let project = Project::new(CONF_TX);
    project.write(name, text);
    project.check_json(&[name])
}

fn only_tx006(report: &Value) -> &Value {
    let rows = tx006(report);
    assert_eq!(rows.len(), 1, "one TX006 row in:\n{report}");
    rows[0]
}

fn fix_new_text(row: &Value) -> &str {
    row["fixes"][0]["edits"][0]["new_text"]
        .as_str()
        .unwrap_or_else(|| panic!("no edit text in {row}"))
}

// J1
#[test]
fn j1_the_fix_is_in_the_json_row() {
    let project = Project::new(CONF_TX);
    project.write("main.p", J1_TEXT);
    let run = project.check(&["--json", "--no-format", "main.p"]);
    assert_eq!(run.exit, Some(1), "{}{}", run.stdout, run.stderr);
    let report = run.json();
    assert_eq!(report["version"], 6);
    let row = only_tx006(&report);
    assert_eq!(row["span"], json!({"start": 16, "end": 22}));
    assert_eq!(
        row["fixes"],
        json!([{
            "title": "Add NO-UNDO",
            "preferred": true,
            "edits": [{
                "path": "main.p",
                "span": {"start": 33, "end": 33},
                "start": {"byte": 33, "line": 1, "column": 34},
                "end": {"byte": 33, "line": 1, "column": 34},
                "new_text": " NO-UNDO"
            }]
        }])
    );
    assert_eq!(project.read("main.p"), J1_TEXT);
}

// J2
#[test]
fn j2_no_fixes_key_when_there_is_no_fix() {
    assert_fixes_are_live();
    let report = tx_report(
        "main.p",
        "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\nDISPLAY iCount.\n",
    );
    assert!(tx006(&report).is_empty(), "{report}");
    // The key is absent, not an empty array, on every row of every array.
    assert_no_fixes_anywhere(&report);
}

// J3
#[test]
fn j3_keyword_case_preserve_follows_the_statement() {
    let report = tx_report(
        "main.p",
        "define variable iCount as integer.\ndisplay iCount.\n",
    );
    let row = only_tx006(&report);
    let fix = &row["fixes"][0];
    assert_eq!(fix["title"], "Add NO-UNDO");
    assert_eq!(fix["edits"][0]["span"]["start"], 33);
    assert_eq!(fix["edits"][0]["new_text"], " no-undo");
}

// J4
#[test]
fn j4_keyword_case_from_the_style() {
    let upper = Project::new(&format!(
        "{CONF_TX}\n[workspace.style]\nkeyword_case = \"Uppercase\"\n"
    ));
    upper.write(
        "main.p",
        "define variable iCount as integer.\ndisplay iCount.\n",
    );
    let report = upper.check_json(&["main.p"]);
    assert_eq!(fix_new_text(only_tx006(&report)), " NO-UNDO");

    let lower = Project::new(&format!(
        "{CONF_TX}\n[workspace.style]\nkeyword_case = \"Lowercase\"\n"
    ));
    lower.write("main.p", J1_TEXT);
    let report = lower.check_json(&["main.p"]);
    assert_eq!(fix_new_text(only_tx006(&report)), " no-undo");
}

// J5
#[test]
fn j5_each_definition_form() {
    let cases: &[(&str, &str, &str)] = &[
        (
            "a.p",
            "DEFINE VARIABLE iCount AS INTEGER INITIAL 5.\nDISPLAY iCount.\n",
            "DEFINE VARIABLE iCount AS INTEGER NO-UNDO INITIAL 5.\nDISPLAY iCount.\n",
        ),
        (
            "b.p",
            "DEFINE INPUT PARAMETER pName AS CHARACTER.\nDISPLAY pName.\n",
            "DEFINE INPUT PARAMETER pName AS CHARACTER NO-UNDO.\nDISPLAY pName.\n",
        ),
        (
            "c.p",
            "DEFINE TEMP-TABLE ttItem FIELD ItemNum AS INTEGER.\nFOR EACH ttItem:\n    DISPLAY ttItem.ItemNum.\nEND.\n",
            "DEFINE TEMP-TABLE ttItem NO-UNDO FIELD ItemNum AS INTEGER.\nFOR EACH ttItem:\n    DISPLAY ttItem.ItemNum.\nEND.\n",
        ),
        (
            "d.p",
            "DEFINE VARIABLE iList AS INTEGER EXTENT 3.\nDISPLAY iList[1].\n",
            "DEFINE VARIABLE iList AS INTEGER NO-UNDO EXTENT 3.\nDISPLAY iList[1].\n",
        ),
        (
            "src/app/Totals.cls",
            "CLASS app.Totals:\n    DEFINE PUBLIC PROPERTY Total AS DECIMAL GET. SET.\nEND CLASS.\n",
            "CLASS app.Totals:\n    DEFINE PUBLIC PROPERTY Total AS DECIMAL NO-UNDO GET. SET.\nEND CLASS.\n",
        ),
    ];
    for (name, before, after) in cases {
        let report = tx_report(name, before);
        let row = only_tx006(&report);
        assert_eq!(
            apply_only_edit(row, before),
            *after,
            "{name}: the edit does not give the expected text"
        );

        let fixed = tx_report(name, after);
        assert!(
            tx006(&fixed).is_empty(),
            "{name}: the fixed text still has a TX006 row:\n{fixed}"
        );
    }
}

// J6
#[test]
fn j6_like() {
    let project = Project::new(CONF_TX_SCHEMA);
    project.write("schema.df", SCHEMA_DF);
    let before = "DEFINE VARIABLE cName LIKE Customer.Name.\nDISPLAY cName.\n";
    project.write("main.p", before);
    let report = project.check_json(&["main.p"]);
    let row = only_tx006(&report);
    assert_eq!(
        apply_only_edit(row, before),
        "DEFINE VARIABLE cName LIKE Customer.Name NO-UNDO.\nDISPLAY cName.\n"
    );
}

// J7
#[test]
fn j7_not_offered_for_shared_definitions() {
    assert_fixes_are_live();
    let report = tx_report(
        "main.p",
        "DEFINE NEW SHARED VARIABLE gTotal AS INTEGER.\nDEFINE SHARED TEMP-TABLE ttShared FIELD a AS INTEGER.\nDISPLAY gTotal.\n",
    );
    let rows = tx006(&report);
    for name in ["gTotal", "ttShared"] {
        let row = rows
            .iter()
            .find(|row| row["message"].as_str().unwrap().contains(name))
            .unwrap_or_else(|| panic!("no TX006 row for {name}:\n{report}"));
        assert!(row.get("fixes").is_none(), "{name} has a fix: {row}");
    }
    assert_no_fixes_anywhere(&report);
}

// J8
#[test]
fn j8_not_offered_for_a_temp_table_that_writes_undo() {
    assert_fixes_are_live();
    let report = tx_report(
        "main.p",
        "DEFINE TEMP-TABLE ttA UNDO FIELD a AS INTEGER.\nFOR EACH ttA:\n    DISPLAY ttA.a.\nEND.\n",
    );
    for row in tx006(&report) {
        assert!(row.get("fixes").is_none(), "{row}");
    }
}

// J9
#[test]
fn j9_not_offered_for_a_type_from_the_preprocessor() {
    assert_fixes_are_live();
    let report = tx_report(
        "main.p",
        "&SCOPED-DEFINE T INTEGER\nDEFINE VARIABLE iX AS {&T}.\nDISPLAY iX.\n",
    );
    for row in tx006(&report) {
        assert!(row.get("fixes").is_none(), "{row}");
    }
}

// J10
#[test]
fn j10_not_offered_for_a_definition_the_preprocessor_made() {
    assert_fixes_are_live();
    let report = tx_report(
        "main.p",
        "&SCOPED-DEFINE VARDEF DEFINE VARIABLE iX AS INTEGER.\n{&VARDEF}\nDISPLAY iX.\n",
    );
    assert_no_fixes_anywhere(&report);
}

// J11
#[test]
fn j11_not_offered_with_an_empty_reference_in_the_statement() {
    assert_fixes_are_live();
    let report = tx_report(
        "main.p",
        "DEFINE VARIABLE iX AS INTEGER {&EXTRA}.\nDISPLAY iX.\n",
    );
    for row in tx006(&report) {
        assert!(row.get("fixes").is_none(), "{row}");
    }
}

// J12
#[test]
fn j12_not_offered_for_a_definition_in_an_include() {
    assert_fixes_are_live();
    // With the spec's configuration the include is not found; with an include
    // path it is. A fix is absent in both.
    for (config, resolved) in [(CONF_TX, false), (CONF_TX_INCLUDE, true)] {
        let project = Project::new(config);
        project.write("defs.i", "DEFINE VARIABLE iInc AS INTEGER.\n");
        project.write("main.p", "{defs.i}\nDISPLAY iInc.\n");

        for args in [
            &["."][..],
            &["--include-findings", "."][..],
            &["main.p"][..],
        ] {
            let report = project.check_json(args);
            assert_no_fixes_anywhere(&report);
            if resolved {
                assert!(
                    !rows(&report, "preproc")
                        .iter()
                        .any(|row| row["code"] == "PREPROC007"),
                    "the include did not resolve: {report}"
                );
            }
        }
        assert_eq!(project.read("defs.i"), "DEFINE VARIABLE iInc AS INTEGER.\n");
        assert_eq!(project.read("main.p"), "{defs.i}\nDISPLAY iInc.\n");
    }
}

// J13
#[test]
fn j13_not_offered_for_a_file_that_does_not_parse() {
    assert_fixes_are_live();
    let report = tx_report(
        "main.p",
        "DEFINE VARIABLE iX AS INTEGER.\n@ @ @\nDISPLAY iX.\n",
    );
    assert!(!with_code(&report, "PARSE001").is_empty(), "{report}");
    assert_no_fixes_anywhere(&report);
}

// J14
#[test]
fn j14_not_offered_for_a_p_file_another_checked_file_includes() {
    assert_fixes_are_live();
    let project = Project::new(CONF_TX_INCLUDE);
    project.write(
        "shared.p",
        "DEFINE VARIABLE iShared AS INTEGER.\nDISPLAY iShared.\n",
    );
    project.write("main.p", "{shared.p}\n");
    let report = project.check_json(&["."]);
    assert!(
        !rows(&report, "preproc")
            .iter()
            .any(|row| row["code"] == "PREPROC007"),
        "the include did not resolve: {report}"
    );
    let row = row_for_path(&tx006(&report), "shared.p");
    assert!(row.get("fixes").is_none(), "{row}");
}

// J15
#[test]
fn j15_other_rows_keep_their_shape() {
    let project = Project::new(CONF_DEFAULT);
    project.write("main.p", "DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n");
    project.write("other.p", "MESSAGE \"hi\".\n");
    let report = project.check_json(&["."]);
    let rows = with_code(&report, "LINT0002");
    let row = row_for_path(&rows, "main.p");
    for key in [
        "path", "code", "severity", "source", "message", "span", "start", "end",
    ] {
        assert!(row.get(key).is_some(), "LINT0002 row lacks `{key}`: {row}");
    }
    assert_eq!(row["code"], "LINT0002");
    assert_eq!(row["source"], "lint");
    assert!(
        row["message"].as_str().unwrap().contains("v-total"),
        "{row}"
    );
    for key in ["start", "end"] {
        for part in ["byte", "line", "column"] {
            assert!(row[key][part].is_u64(), "`{key}.{part}` in {row}");
        }
    }
    for part in ["start", "end"] {
        assert!(row["span"][part].is_u64(), "`span.{part}` in {row}");
    }
}

// J16
#[test]
fn j16_a_baseline_does_not_read_fixes() {
    let project = Project::new(CONF_TX);
    project.write("main.p", J1_TEXT);
    let update = project.run_raw(&[
        "--no-format",
        "--baseline",
        "b.json",
        "--update-baseline",
        "main.p",
    ]);
    assert_eq!(update.exit, Some(0), "{}{}", update.stdout, update.stderr);
    let before_fixes = project.read("main.p");
    assert_eq!(before_fixes, J1_TEXT);
    let second = project.check(&["--no-format", "--baseline", "b.json", "main.p"]);
    assert_eq!(second.exit, Some(0), "{}{}", second.stdout, second.stderr);
    assert!(second.stderr.contains("0 new"), "{}", second.stderr);
}
