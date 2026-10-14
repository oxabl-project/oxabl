//! `oxabl check --include-findings`: findings that sit in an include, reported
//! for the compilation unit of each root that expands it.
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
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn json(stdout: &str) -> serde_json::Value {
    serde_json::from_str(stdout).unwrap_or_else(|e| panic!("{e}: {stdout}"))
}

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

const DECL: &str = "DEFINE VARIABLE used-v AS INTEGER NO-UNDO.\n\
                    DEFINE VARIABLE spare-v AS INTEGER NO-UNDO.\n";

fn tree(files: &[(&str, &str)]) -> TempDir {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), CONFIG);
    for (name, contents) in files {
        write(&dir.path().join(name), contents);
    }
    dir
}

fn rows(report: &serde_json::Value) -> Vec<&serde_json::Value> {
    report["include_diagnostics"]
        .as_array()
        .map(|rows| rows.iter().collect())
        .unwrap_or_default()
}

fn norm(path: &serde_json::Value) -> String {
    path.as_str().unwrap().replace('\\', "/")
}

#[test]
fn a_plain_run_does_not_report_findings_in_includes() {
    let dir = tree(&[("decl.i", DECL), ("prog.p", "{decl.i}\nMESSAGE used-v.\n")]);
    let (exit, stdout, stderr) = check(dir.path(), &["prog.p", "--json"]);
    assert_eq!(exit, Some(0), "{stdout}{stderr}");
    let report = json(&stdout);
    assert!(report.get("include_diagnostics").is_none(), "{report}");
    assert_eq!(report["diagnostics"].as_array().unwrap().len(), 0);
}

#[test]
fn the_flag_adds_rows_with_the_root_and_the_chain() {
    let dir = tree(&[("decl.i", DECL), ("prog.p", "{decl.i}\nMESSAGE used-v.\n")]);
    let (exit, stdout, stderr) = check(dir.path(), &["prog.p", "--json", "--include-findings"]);
    assert_eq!(exit, Some(1), "{stdout}{stderr}");
    let report = json(&stdout);
    let rows = rows(&report);
    assert_eq!(rows.len(), 1, "{report}");
    let row = rows[0];
    assert!(norm(&row["path"]).ends_with("decl.i"));
    assert_eq!(norm(&row["root"]), "prog.p");
    assert_eq!(row["scope"], "compilation-unit");
    assert_eq!(row["code"], "LINT0002");
    assert_eq!(row["message"], "unused variable `spare-v`");
    assert_eq!(row["start"]["line"], 2);
    assert_eq!(row["start"]["column"], 17);
    let chain = &row["expansions"][0]["chain"];
    assert_eq!(chain.as_array().unwrap().len(), 1);
    assert!(norm(&chain[0]["path"]).ends_with("decl.i"));
    assert_eq!(chain[0]["line"], 1);
    assert_eq!(chain[0]["column"], 1);
    assert_eq!(row["expansions"][0]["occurrences"], 1);
}

#[test]
fn text_output_names_the_root_and_does_not_claim_the_workspace() {
    let dir = tree(&[("decl.i", DECL), ("prog.p", "{decl.i}\nMESSAGE used-v.\n")]);
    let (exit, stdout, stderr) = check(dir.path(), &["prog.p", "--include-findings"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let out = stdout.replace('\\', "/");
    assert!(
        out.contains("decl.i:2:17: warning[LINT0002]: unused variable `spare-v`"),
        "{out}"
    );
    assert!(out.contains("in the compilation unit of prog.p"), "{out}");
    assert!(out.contains("not a workspace-wide result"), "{out}");
    assert!(out.contains("reached through prog.p:1:1"), "{out}");
}

#[test]
fn each_root_gets_its_own_answer() {
    let dir = tree(&[
        ("decl.i", DECL),
        ("reads.p", "{decl.i}\nMESSAGE used-v spare-v.\n"),
        ("ignores.p", "{decl.i}\nMESSAGE used-v.\n"),
    ]);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json", "--include-findings"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let report = json(&stdout);
    let rows = rows(&report);
    assert_eq!(rows.len(), 1, "{report}");
    assert_eq!(norm(&rows[0]["root"]), "./ignores.p");
}

#[test]
fn a_nested_include_reports_its_whole_chain() {
    let dir = tree(&[
        ("inner.i", "DEFINE VARIABLE deep-v AS INTEGER NO-UNDO.\n"),
        ("outer.i", "/* outer */\n{inner.i}\n"),
        ("prog.p", "{outer.i}\n"),
    ]);
    let (_, stdout, stderr) = check(dir.path(), &["prog.p", "--json", "--include-findings"]);
    let report = json(&stdout);
    let rows = rows(&report);
    assert_eq!(rows.len(), 1, "{stderr}{report}");
    assert!(norm(&rows[0]["path"]).ends_with("inner.i"));
    let chain = rows[0]["expansions"][0]["chain"].as_array().unwrap();
    assert_eq!(chain.len(), 2);
    assert!(norm(&chain[0]["path"]).ends_with("outer.i"));
    assert_eq!(
        (chain[0]["line"].as_u64(), chain[0]["column"].as_u64()),
        (Some(1), Some(1))
    );
    assert!(norm(&chain[1]["path"]).ends_with("inner.i"));
    assert_eq!(
        (chain[1]["line"].as_u64(), chain[1]["column"].as_u64()),
        (Some(2), Some(1))
    );
}

#[test]
fn repeated_expansion_is_one_row_listing_each_expansion() {
    let dir = tree(&[
        ("d.i", "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n"),
        (
            "prog.p",
            "PROCEDURE a:\n{d.i}\nEND PROCEDURE.\nPROCEDURE b:\n{d.i}\nEND PROCEDURE.\n",
        ),
    ]);
    let (_, stdout, _) = check(dir.path(), &["prog.p", "--json", "--include-findings"]);
    let report = json(&stdout);
    let rows = rows(&report);
    assert_eq!(rows.len(), 1, "{report}");
    let expansions = rows[0]["expansions"].as_array().unwrap();
    assert_eq!(expansions.len(), 2);
    assert_eq!(expansions[1]["occurrence"], 2);
    assert_eq!(expansions[1]["occurrences"], 2);

    let (_, text, _) = check(dir.path(), &["prog.p", "--include-findings"]);
    assert!(
        text.contains("(expansion 1 of 2)") && text.contains("(expansion 2 of 2)"),
        "{text}"
    );
}

#[test]
fn root_origin_findings_are_unchanged_by_the_flag() {
    let dir = tree(&[
        ("decl.i", DECL),
        (
            "prog.p",
            "{decl.i}\nDEFINE VARIABLE own-v AS INTEGER NO-UNDO.\nMESSAGE used-v.\n",
        ),
    ]);
    let (plain_exit, plain, _) = check(dir.path(), &["prog.p", "--json"]);
    let (flag_exit, flagged, _) = check(dir.path(), &["prog.p", "--json", "--include-findings"]);
    assert_eq!((plain_exit, flag_exit), (Some(1), Some(1)));
    assert_eq!(json(&plain)["diagnostics"], json(&flagged)["diagnostics"]);
    assert_eq!(json(&flagged)["diagnostics"].as_array().unwrap().len(), 1);
}

#[test]
fn the_flag_does_not_combine_with_a_baseline() {
    let dir = tree(&[("prog.p", "MESSAGE 1.\n")]);
    write(
        &dir.path().join("base.json"),
        "{\"version\":2,\"entries\":[]}",
    );
    let (exit, _, stderr) = check(
        dir.path(),
        &["prog.p", "--include-findings", "--baseline", "base.json"],
    );
    assert_eq!(exit, Some(2), "{stderr}");
}
