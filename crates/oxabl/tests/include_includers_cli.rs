//! `oxabl check <dir>`: a finding in an include file lists the programs of the
//! run that include it, in `--json` (`includers`) and in one note line of the
//! text output. The baseline key does not change.
//!
//! All fixtures are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// `(exit, stdout, stderr)` of `oxabl check --no-format <args>` in `dir`.
fn check(dir: &Path, args: &[&str]) -> (Option<i32>, String, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .arg("check")
        .arg("--no-format")
        .args(args)
        .output()
        .unwrap();
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stdout).replace('\\', "/"),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn json(stdout: &str) -> serde_json::Value {
    serde_json::from_str(stdout).unwrap_or_else(|e| panic!("{e}: {stdout}"))
}

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\n\
     include_paths = [\".\", \"src\"]\n[workspace.lint]\nenable = [\"strict\"]\n";

/// An include with a `RUN VALUE` (CL001) and a shared variable (CL005).
const INC: &str =
    "DEFINE NEW SHARED VARIABLE shared-c AS CHARACTER NO-UNDO.\nRUN VALUE(shared-c).\n";

fn tree(programs: &[(&str, &str)]) -> TempDir {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), CONFIG);
    write(&dir.path().join("src/inc.i"), INC);
    for (name, text) in programs {
        write(&dir.path().join("src").join(name), text);
    }
    dir
}

/// The `includers` of the `code` finding in `file`, or `None` when the finding
/// has none.
fn includers(report: &serde_json::Value, file: &str, code: &str) -> Option<serde_json::Value> {
    let row = report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["path"].as_str().unwrap().ends_with(file) && d["code"] == code)
        .unwrap_or_else(|| panic!("no {code} in {file}: {report}"));
    row.get("includers").cloned()
}

fn strings(value: &serde_json::Value) -> Vec<&str> {
    value
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_str().unwrap())
        .collect()
}

#[test]
fn json_lists_the_includers_sorted_with_their_sites() {
    let dir = tree(&[
        ("c.p", "MESSAGE \"c\".\n{inc.i}\n"),
        ("b.p", "{inc.i}\nMESSAGE \"b\".\n"),
    ]);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let report = json(&stdout);

    for code in ["CL001", "CL005"] {
        let found = includers(&report, "src/inc.i", code).expect("includers");
        assert_eq!(found["count"], 2, "{code}: {found}");
        assert_eq!(
            strings(&found["first"]),
            ["./src/b.p:1", "./src/c.p:2"],
            "{code}"
        );
    }
}

#[test]
fn json_lists_at_most_three_and_counts_all() {
    // Created out of order: the list is the three smallest, not the first three.
    let dir = tree(&[
        ("e.p", "{inc.i}\n"),
        ("a.p", "{inc.i}\n"),
        ("d.p", "{inc.i}\n"),
        ("b.p", "{inc.i}\n"),
        ("c.p", "{inc.i}\n"),
    ]);
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    let found = includers(&report, "src/inc.i", "CL001").expect("includers");
    assert_eq!(found["count"], 5, "{stderr}{found}");
    assert_eq!(
        strings(&found["first"]),
        ["./src/a.p:1", "./src/b.p:1", "./src/c.p:1"]
    );
}

#[test]
fn the_list_is_sorted_by_the_path_as_shown_not_by_the_walk_order() {
    // The walk orders `a/x.p` before `a.d/y.p`; the shown paths order the other way.
    let dir = tree(&[
        ("a/x.p", "{inc.i}\n"),
        ("a.d/y.p", "{inc.i}\n"),
        ("b.p", "{inc.i}\n"),
        ("c.p", "{inc.i}\n"),
    ]);
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    let found = includers(&report, "src/inc.i", "CL001").expect("includers");
    assert_eq!(found["count"], 4, "{stderr}{found}");
    assert_eq!(
        strings(&found["first"]),
        ["./src/a.d/y.p:1", "./src/a/x.p:1", "./src/b.p:1"]
    );
}

#[cfg(unix)]
#[test]
fn a_program_that_reaches_one_file_by_two_names_counts_once() {
    let dir = tree(&[("a.p", "{inc.i}\n{zalias.i}\n")]);
    std::os::unix::fs::symlink("inc.i", dir.path().join("src/zalias.i")).unwrap();
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    let found = includers(&report, "src/inc.i", "CL001").expect("includers");
    assert_eq!(found["count"], 1, "{stderr}{found}");
}

#[test]
fn a_program_that_includes_twice_counts_once_at_its_first_site() {
    let dir = tree(&[("a.p", "MESSAGE \"a\".\n{inc.i}\n{inc.i}\n")]);
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    let found = includers(&report, "src/inc.i", "CL001").expect("includers");
    assert_eq!(found["count"], 1, "{stderr}{found}");
    assert_eq!(strings(&found["first"]), ["./src/a.p:2"]);
}

#[test]
fn a_nested_include_names_the_program_and_the_site_that_reaches_it() {
    let dir = tree(&[("a.p", "MESSAGE \"a\".\nMESSAGE \"b\".\n{mid.i}\n")]);
    write(&dir.path().join("src/mid.i"), "{inc.i}\n");
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    let found = includers(&report, "src/inc.i", "CL001").expect("includers");
    assert_eq!(found["count"], 1, "{stderr}{found}");
    assert_eq!(strings(&found["first"]), ["./src/a.p:3"]);
}

#[test]
fn an_include_that_no_checked_program_includes_has_a_count_of_zero() {
    let dir = tree(&[("a.p", "MESSAGE \"a\".\n")]);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let report = json(&stdout);
    let found = includers(&report, "src/inc.i", "CL001").expect("includers");
    assert_eq!(found["count"], 0, "{found}");
    assert_eq!(found["first"].as_array().unwrap().len(), 0);

    let (_, text, _) = check(dir.path(), &["."]);
    assert!(text.contains("warning[CL001]"), "{text}");
    assert!(
        !text.contains("included by"),
        "no note for no includer: {text}"
    );
}

#[test]
fn a_finding_in_a_program_has_no_includers() {
    let dir = tree(&[(
        "a.p",
        "DEFINE VARIABLE x AS CHARACTER NO-UNDO.\nRUN VALUE(x).\n",
    )]);
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    assert!(
        includers(&report, "src/a.p", "CL001").is_none(),
        "{stderr}{report}"
    );
}

#[test]
fn text_adds_one_note_line_after_each_finding_in_an_include() {
    let dir = tree(&[
        ("b.p", "{inc.i}\nMESSAGE \"b\".\n"),
        ("c.p", "MESSAGE \"c\".\n{inc.i}\n"),
    ]);
    let (exit, text, stderr) = check(dir.path(), &["."]);
    assert_eq!(exit, Some(1), "{stderr}");
    let notes: Vec<&str> = text.lines().filter(|l| l.contains("included by")).collect();
    assert_eq!(
        notes, ["  note: included by 2 files, e.g. ./src/b.p:1, ./src/c.p:2"; 2],
        "one per finding: {text}"
    );
    // Each note follows its own finding.
    let lines: Vec<&str> = text.lines().collect();
    for (i, line) in lines.iter().enumerate() {
        if line.contains("included by") {
            assert!(lines[i - 1].starts_with("  help:"), "{text}");
        }
    }
}

/// A baseline that an earlier run wrote: the key of an include finding is the
/// include path, the rule and a hash of its line, with no includers.
const OLD_BASELINE: &str = r#"{
  "version": 2,
  "entries": [
    {
      "path": "src/inc.i",
      "code": "CL001",
      "hash": "f20a6c0752419bb8",
      "occurrence": 0,
      "message": "`RUN VALUE(...)` calls a procedure chosen at run time"
    },
    {
      "path": "src/inc.i",
      "code": "CL005",
      "hash": "b6bc2a4dad7a76a8",
      "occurrence": 0,
      "message": "`NEW SHARED` variable `shared-c` is shared state"
    }
  ]
}
"#;

#[test]
fn an_existing_baseline_still_matches_a_finding_that_has_includers() {
    let dir = tree(&[("b.p", "{inc.i}\n"), ("c.p", "{inc.i}\n")]);
    write(&dir.path().join("old.json"), OLD_BASELINE);

    let (exit, stdout, stderr) = check(dir.path(), &[".", "--baseline", "old.json", "--json"]);
    assert_eq!(exit, Some(0), "{stderr}{stdout}");
    let report = json(&stdout);
    assert_eq!(report["baseline"]["baselined"], 2, "{report}");
    assert_eq!(report["baseline"]["new"], 0, "{report}");
    assert_eq!(report["baseline"]["stale"], 0, "{report}");

    // Without the baseline the same findings are there, with includers.
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    assert_eq!(
        includers(&report, "src/inc.i", "CL001").unwrap()["count"],
        2
    );
}

#[test]
fn a_new_baseline_has_no_includers_and_the_same_version() {
    let dir = tree(&[("b.p", "{inc.i}\n"), ("c.p", "{inc.i}\n")]);
    let (exit, _, stderr) = check(
        dir.path(),
        &[".", "--baseline", "new.json", "--update-baseline"],
    );
    assert_eq!(exit, Some(0), "{stderr}");
    let written = fs::read_to_string(dir.path().join("new.json")).unwrap();
    assert!(!written.contains("includers"), "{written}");
    assert_eq!(json(&written)["version"], 2);
    // The file that an old binary wrote is the file that this one writes.
    assert_eq!(json(&written), json(OLD_BASELINE));
}
