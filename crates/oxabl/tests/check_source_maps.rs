//! `oxabl check` builds the line table of a text once, whatever the number of
//! findings and labels in it. A table built for each finding or label makes
//! the report quadratic: findings times the size of the file.
//!
//! A test build of the binary counts the tables it builds and prints the count
//! when `OXABL_TEST_SOURCE_MAPS` is set. Each test runs one input with few
//! findings and one with many, and wants the same count. All fixtures are
//! synthetic ABL written for this file.

use std::fs;
use std::path::Path;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

/// `count` declarations of one name: each after the first is a `SEM0001`
/// finding with a label at the first.
fn duplicates(count: usize) -> String {
    let mut source = String::from("/* padding so a table costs more than a finding */\n");
    for i in 0..200 {
        source.push_str(&format!("/* line {i} of a long comment block */\n"));
    }
    for _ in 0..count {
        source.push_str("DEFINE VARIABLE dup-name AS INTEGER NO-UNDO.\n");
    }
    source.push_str("MESSAGE dup-name.\n");
    source
}

/// Run `oxabl check` in `dir` with `args`: the stdout and the number of line
/// tables the run built.
fn check(dir: &Path, args: &[&str]) -> (String, usize) {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .env("OXABL_TEST_SOURCE_MAPS", "1")
        .args(["check", "--no-format", "-j", "1"])
        .args(args)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    let built = stderr
        .lines()
        .find_map(|line| line.strip_prefix("oxabl-test: source maps built: "))
        .unwrap_or_else(|| panic!("the test build printed no count; stderr:\n{stderr}"))
        .parse()
        .unwrap();
    (String::from_utf8_lossy(&out.stdout).into_owned(), built)
}

fn json(stdout: &str) -> Value {
    serde_json::from_str(stdout).unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{stdout}"))
}

/// The findings of `key` that have a related place with a line.
fn with_labels(report: &Value, key: &str) -> usize {
    report[key]
        .as_array()
        .unwrap_or_else(|| panic!("no `{key}` array in {report}"))
        .iter()
        .filter(|d| {
            d["related"]
                .as_array()
                .is_some_and(|r| r.iter().any(|r| r["start"]["line"].is_u64()))
        })
        .count()
}

/// The runs of `args` over a root file of `few` and of `many` duplicates.
fn root_counts(few: usize, many: usize, args: &[&str]) -> [(String, usize); 2] {
    [few, many].map(|count| {
        let tmp = TempDir::new().unwrap();
        fs::write(tmp.path().join("dups.p"), duplicates(count)).unwrap();
        check(tmp.path(), &[args, &["dups.p"]].concat())
    })
}

/// The runs of `args` over a root that includes a file of `few` and of `many`
/// duplicates.
fn include_counts(few: usize, many: usize, args: &[&str]) -> [(String, usize); 2] {
    [few, many].map(|count| {
        let tmp = TempDir::new().unwrap();
        fs::write(tmp.path().join("dups.i"), duplicates(count)).unwrap();
        fs::write(
            tmp.path().join("main.p"),
            "{dups.i}\n{dups.i}\nMESSAGE \"done\".\n",
        )
        .unwrap();
        check(
            tmp.path(),
            &[args, &["--include-findings", "-I", ".", "main.p"]].concat(),
        )
    })
}

#[test]
fn json_report_of_a_file_builds_its_tables_once() {
    let [(few_out, few), (many_out, many)] = root_counts(5, 300, &["--json"]);
    assert_eq!(with_labels(&json(&few_out), "diagnostics"), 4);
    assert_eq!(with_labels(&json(&many_out), "diagnostics"), 299);
    assert_eq!(
        few, many,
        "299 findings built {many} line tables, 4 findings built {few}"
    );
}

#[test]
fn text_report_of_a_file_builds_its_tables_once() {
    let [(few_out, few), (many_out, many)] = root_counts(5, 300, &[]);
    assert_eq!(few_out.matches("SEM0001").count(), 4);
    assert_eq!(many_out.matches("SEM0001").count(), 299);
    assert_eq!(
        few, many,
        "299 findings built {many} line tables, 4 findings built {few}"
    );
}

#[test]
fn json_report_of_include_findings_builds_each_table_once() {
    let [(few_out, few), (many_out, many)] = include_counts(5, 300, &["--json"]);
    let few_found = with_labels(&json(&few_out), "include_diagnostics");
    let many_found = with_labels(&json(&many_out), "include_diagnostics");
    assert!(
        few_found > 0 && many_found > few_found * 10,
        "include findings with labels: {few_found} and {many_found}\n{many_out}"
    );
    assert_eq!(
        few, many,
        "{many_found} include findings built {many} line tables, {few_found} built {few}"
    );
}

#[test]
fn text_report_of_include_findings_builds_each_table_once() {
    let [(few_out, few), (many_out, many)] = include_counts(5, 300, &[]);
    let few_found = few_out.matches("reached through").count();
    let many_found = many_out.matches("reached through").count();
    assert!(
        few_found > 0 && many_found > few_found * 10,
        "include findings: {few_found} and {many_found}\n{many_out}"
    );
    assert_eq!(
        few, many,
        "{many_found} include findings built {many} line tables, {few_found} built {few}"
    );
}

#[test]
fn parse_errors_in_an_include_build_its_table_once() {
    // The parse stops reporting after 50 errors.
    let [(few_out, few), (many_out, many)] = [2, 40].map(|count| {
        let tmp = TempDir::new().unwrap();
        let bad = "DEFINE VARIABLE AS INTEGER NO-UNDO.\n".repeat(count);
        fs::write(tmp.path().join("bad.i"), bad).unwrap();
        fs::write(tmp.path().join("main.p"), "{bad.i}\nMESSAGE \"done\".\n").unwrap();
        check(tmp.path(), &["--json", "-I", ".", "main.p"])
    });
    let at_site = |stdout: &str| {
        json(stdout)["diagnostics"]
            .as_array()
            .expect("diagnostics array")
            .iter()
            .filter(|d| {
                d["message"]
                    .as_str()
                    .is_some_and(|m| m.contains("(in bad.i:"))
            })
            .count()
    };
    assert_eq!(at_site(&few_out), 2);
    assert_eq!(at_site(&many_out), 40);
    assert_eq!(
        few, many,
        "40 parse errors built {many} line tables, 2 built {few}"
    );
}

/// A directory walk where `src/inc.i` has `count` findings of a context-free
/// rule and `src/main.p` includes it, so each finding lists its includers.
fn includers_counts(few: usize, many: usize, args: &[&str]) -> [(String, usize); 2] {
    [few, many].map(|count| {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("oxabl.toml"),
            "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\", \"src\"]\n\
             [workspace.lint]\nenable = [\"strict\"]\n",
        )
        .unwrap();
        fs::create_dir(tmp.path().join("src")).unwrap();
        let mut inc = String::from("DEFINE NEW SHARED VARIABLE shared-c AS CHARACTER NO-UNDO.\n");
        for _ in 0..count {
            inc.push_str("RUN VALUE(shared-c).\n");
        }
        fs::write(tmp.path().join("src/inc.i"), inc).unwrap();
        fs::write(tmp.path().join("src/main.p"), "{inc.i}\n").unwrap();
        check(tmp.path(), &[args, &["src"]].concat())
    })
}

#[test]
fn json_report_of_an_include_with_includers_builds_its_tables_once() {
    let [(few_out, few), (many_out, many)] = includers_counts(5, 300, &["--json"]);
    let found = |out: &str| {
        json(out)["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|d| d["code"] == "CL001" && d["includers"]["count"] == 1)
            .count()
    };
    assert_eq!((found(&few_out), found(&many_out)), (5, 300));
    assert_eq!(
        few, many,
        "300 findings with includers built {many} line tables, 5 built {few}"
    );
}

#[test]
fn text_report_of_an_include_with_includers_builds_its_tables_once() {
    let [(few_out, few), (many_out, many)] = includers_counts(5, 300, &[]);
    assert_eq!(few_out.matches("note: included by 1 file").count(), 5 + 1);
    // One more note for the `CL005` finding of the shared variable.
    assert_eq!(
        many_out.matches("note: included by 1 file").count(),
        300 + 1
    );
    assert_eq!(
        few, many,
        "300 findings with includers built {many} line tables, 5 built {few}"
    );
}

/// A file with an unresolvable include (a preprocessor finding) and one other
/// finding, so the text report, the preprocessor rows and the finding rows all
/// need the line table.
fn preproc_counts(args: &[&str]) -> (String, usize) {
    let tmp = TempDir::new().unwrap();
    fs::write(
        tmp.path().join("p.p"),
        "{missing.i}\nDEFINE VARIABLE a AS INTEGER NO-UNDO.\nDEFINE VARIABLE a AS INTEGER NO-UNDO.\nMESSAGE a.\n",
    )
    .unwrap();
    check(tmp.path(), &[args, &["p.p"]].concat())
}

#[test]
fn a_file_with_a_preprocessor_finding_builds_one_table() {
    let (out, built) = preproc_counts(&[]);
    assert!(out.contains("SEM0001"), "no finding in:\n{out}");
    assert_eq!(built, 1, "text report built {built} line tables");
    let (out, built) = preproc_counts(&["--json"]);
    assert!(out.contains("PREPROC007"), "no preprocessor row in:\n{out}");
    assert_eq!(built, 1, "JSON report built {built} line tables");
}

/// A directory of three roots that include one file of `count` duplicate
/// declarations: each root reports every finding of the include.
fn shared_include_counts(count: usize, args: &[&str]) -> (String, usize) {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("dups.i"), duplicates(count)).unwrap();
    for root in ["a.p", "b.p", "c.p"] {
        fs::write(tmp.path().join(root), "{dups.i}\nMESSAGE \"done\".\n").unwrap();
    }
    check(
        tmp.path(),
        &[args, &["--include-findings", "-I", ".", "."]].concat(),
    )
}

#[test]
fn roots_that_share_an_include_build_its_table_once_each_run() {
    for args in [&["--json"][..], &[][..]] {
        let (_, few) = shared_include_counts(5, args);
        let (out, many) = shared_include_counts(300, args);
        assert!(
            out.matches("already declared").count() >= 3 * 299,
            "few findings in:\n{out}"
        );
        assert_eq!(
            few, many,
            "{args:?}: 300 findings in a shared include built {many} line tables, 5 built {few}"
        );
    }
}
