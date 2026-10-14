//! `oxabl check <dir>` and the context-free strict rules in include files.
//!
//! A walk checks `.i` files too, but only for rules whose finding does not
//! depend on the includer, and reports each finding once, in the include.
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

/// `(path, code)` of every finding in a `--json` report, `/`-separated.
fn findings(report: &serde_json::Value) -> Vec<(String, String)> {
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| {
            (
                d["path"].as_str().unwrap().replace('\\', "/"),
                d["code"].as_str().unwrap().to_string(),
            )
        })
        .collect()
}

/// Includes resolve through include paths only, so the tree names its own.
const SOURCES: &str = "[workspace.sources]\ninclude_paths = [\".\", \"src\"]\n";

const STRICT_ALL: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\", \"src\"]\n\
     [workspace.lint]\nenable = [\"strict\"]\n";

/// An include with a `RUN VALUE` and a shared variable that two programs read.
const INC: &str =
    "DEFINE NEW SHARED VARIABLE shared-c AS CHARACTER NO-UNDO.\nRUN VALUE(shared-c).\n";

fn tree(config: &str) -> TempDir {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), config);
    write(&dir.path().join("src/inc.i"), INC);
    write(&dir.path().join("src/b.p"), "{inc.i}\nMESSAGE \"b\".\n");
    write(&dir.path().join("src/c.p"), "{inc.i}\nMESSAGE \"c\".\n");
    dir
}

fn codes_at(report: &serde_json::Value, file: &str) -> Vec<String> {
    findings(report)
        .into_iter()
        .filter(|(p, _)| p.ends_with(file))
        .map(|(_, c)| c)
        .collect()
}

#[test]
fn a_strict_finding_in_an_include_is_reported_once_in_the_include() {
    let dir = tree(STRICT_ALL);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    assert!(
        !stderr.contains("PREPROC007"),
        "the include resolves: {stderr}"
    );
    let report = json(&stdout);

    let mut in_include = codes_at(&report, "src/inc.i");
    in_include.sort();
    assert!(
        in_include.contains(&"CL001".to_string()) && in_include.contains(&"CL005".to_string()),
        "{in_include:?}"
    );
    assert_eq!(
        in_include.iter().filter(|c| *c == "CL001").count(),
        1,
        "once, however many programs include it: {in_include:?}"
    );
    assert!(codes_at(&report, "b.p").is_empty(), "{report}");
    assert!(codes_at(&report, "c.p").is_empty(), "{report}");
    assert_eq!(report["files_checked"], 2, "roots only");
    assert_eq!(report["include_files_checked"], 1);
}

#[test]
fn an_include_is_checked_for_context_free_rules_only() {
    let dir = tree(STRICT_ALL);
    // An unresolvable `RUN` target and a syntax error: findings that an include
    // fragment shows when it is read without its includer.
    write(
        &dir.path().join("src/frag.i"),
        "RUN missing-program.p.\n@ @ @\nRUN VALUE(c).\n",
    );
    let (_, explicit, _) = check(dir.path(), &["src/frag.i", "--json"]);
    let explicit = json(&explicit);
    let explicit_codes = codes_at(&explicit, "frag.i");
    assert!(
        ["PARSE001", "LINT0001", "CL001"]
            .iter()
            .all(|c| explicit_codes.iter().any(|e| e == c)),
        "the control run shows the noise: {explicit_codes:?}"
    );
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    assert_eq!(codes_at(&report, "src/frag.i"), ["CL001"], "{report}");
}

#[test]
fn nothing_is_checked_in_an_include_when_no_context_free_rule_is_on() {
    let dir = tree(&format!("[workspace]\nname = \"p\"\n{SOURCES}"));
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(0), "{stderr}");
    let report = json(&stdout);
    assert!(report.get("include_files_checked").is_none(), "{report}");
    assert!(findings(&report).is_empty());
}

#[test]
fn the_override_that_enables_a_group_decides_which_includes_are_checked() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &format!(
            "[workspace]\nname = \"p\"\n{SOURCES}\
             [[workspace.lint.overrides]]\nfiles = [\"strict\"]\nenable = [\"strict.calls\"]\n"
        ),
    );
    write(&dir.path().join("strict/s.i"), "RUN VALUE(c).\n");
    write(&dir.path().join("legacy/l.i"), "RUN VALUE(c).\n");
    write(&dir.path().join("strict/a.p"), "MESSAGE \"a\".\n");
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    assert_eq!(codes_at(&report, "strict/s.i"), ["CL001"], "{report}");
    assert!(codes_at(&report, "legacy/l.i").is_empty(), "{report}");
    assert_eq!(report["include_files_checked"], 1);
}

#[test]
fn a_nested_include_is_reported_in_its_own_file_only() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), STRICT_ALL);
    write(&dir.path().join("outer.i"), "{inner.i}\n");
    write(&dir.path().join("inner.i"), "RUN VALUE(c).\n");
    write(&dir.path().join("a.p"), "{outer.i}\n");
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert!(
        !stderr.contains("PREPROC007"),
        "the includes resolve: {stderr}"
    );
    let report = json(&stdout);
    let found = findings(&report);
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].0.ends_with("inner.i"), "{found:?}");
    assert_eq!(found[0].1, "CL001");
    assert_eq!(report["include_files_checked"], 2);
}

#[test]
fn naming_one_program_does_not_check_its_includes_but_naming_the_include_does() {
    let dir = tree(STRICT_ALL);
    let (_, stdout, _) = check(dir.path(), &["src/b.p", "--json"]);
    assert!(findings(&json(&stdout)).is_empty(), "{stdout}");

    let (exit, stdout, _) = check(dir.path(), &["src/inc.i", "--json"]);
    assert_eq!(exit, Some(1));
    assert!(
        codes_at(&json(&stdout), "inc.i").contains(&"CL001".to_string()),
        "{stdout}"
    );
}

#[test]
fn no_lint_skips_the_include_walk() {
    let dir = tree(STRICT_ALL);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json", "--no-lint"]);
    assert_eq!(exit, Some(0), "{stderr}");
    let report = json(&stdout);
    assert!(report.get("include_files_checked").is_none(), "{report}");
}

#[test]
fn the_text_summary_counts_the_include_files() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), STRICT_ALL);
    write(&dir.path().join("a.p"), "MESSAGE \"a\".\n");
    write(&dir.path().join("ok.i"), "MESSAGE \"fine\".\n");
    let (exit, stdout, stderr) = check(dir.path(), &["."]);
    assert_eq!(exit, Some(0), "{stderr}");
    assert!(
        stdout.contains("checked 1 file and 1 include file: no findings"),
        "{stdout}"
    );
}

#[test]
fn an_include_finding_is_baselined_under_the_include_path() {
    let dir = tree(STRICT_ALL);
    let root = dir.path();
    let (exit, _, stderr) = check(root, &[".", "--baseline", "b.json", "--update-baseline"]);
    assert_eq!(exit, Some(0), "{stderr}");
    let doc = json(&fs::read_to_string(root.join("b.json")).unwrap());
    let paths: Vec<_> = doc["entries"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| e["path"].as_str().unwrap())
        .collect();
    assert!(!paths.is_empty());
    assert!(
        paths.iter().all(|p| *p == "src/inc.i"),
        "keyed by the include: {paths:?}"
    );

    // A new program that includes the file adds nothing; a keyword case change
    // in the include keeps the match.
    write(&root.join("src/d.p"), "{inc.i}\n");
    write(
        &root.join("src/inc.i"),
        "define new shared variable shared-c as character no-undo.\nrun value(shared-c).\n",
    );
    let (exit, stdout, stderr) = check(root, &[".", "--baseline", "b.json", "--json"]);
    assert_eq!(exit, Some(0), "{stderr}");
    let report = json(&stdout);
    assert_eq!(report["baseline"]["new"], 0, "{report}");
    assert_eq!(report["baseline"]["stale"], 0);
}

/// A parametrised fragment: the includer supplies the modifier and `NO-UNDO`.
const PARAMETRISED: &str = "\
DEFINE {&mod} VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE y AS INTEGER {&nu}.
&IF DEFINED(want-run) &THEN
RUN VALUE(y).
&ENDIF
DEFINE VARIABLE z AS INTEGER.
";

#[test]
fn a_finding_that_depends_on_the_includer_has_the_generic_help() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), STRICT_ALL);
    write(&dir.path().join("src/inc.i"), PARAMETRISED);
    write(
        &dir.path().join("src/a.p"),
        "{inc.i &mod=\"NEW SHARED\" &nu=\"NO-UNDO\" &want-run=1}\nMESSAGE x y.\n",
    );
    let (_, stdout, stderr) = check(dir.path(), &["src", "--json"]);
    let report = json(&stdout);
    let rows: Vec<(u64, String)> = report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["path"].as_str().unwrap().ends_with("inc.i"))
        .map(|d| {
            (
                d["start"]["line"].as_u64().unwrap(),
                d["help"].as_str().unwrap_or_default().to_string(),
            )
        })
        .collect();
    // `y` depends on `{&nu}`, which the fragment alone leaves empty; `z` is
    // plain. The compiled `x` (NEW SHARED) and `RUN VALUE` are not seen.
    assert_eq!(rows.len(), 2, "{rows:?}\n{stderr}");
    assert_eq!(rows[0].0, 2);
    assert!(rows[0].1.contains("including file"), "{}", rows[0].1);
    assert!(!rows[0].1.contains("producer"), "{}", rows[0].1);
    assert_eq!(rows[1], (6, "Add `NO-UNDO` to the definition.".to_string()));
}

#[test]
fn a_directory_of_only_includes_is_checked() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), STRICT_ALL);
    write(&dir.path().join("inc/a.i"), INC);
    let (exit, stdout, stderr) = check(dir.path(), &["inc", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    assert!(!stderr.contains("no ABL files"), "{stderr}");
    let report = json(&stdout);
    assert_eq!(report["files_checked"], 0);
    assert_eq!(report["include_files_checked"], 1);
    assert!(codes_at(&report, "inc/a.i").contains(&"CL001".to_string()));

    // The baseline can record it.
    let (exit, _, stderr) = check(
        dir.path(),
        &["inc", "--baseline", "base.json", "--update-baseline"],
    );
    assert_eq!(exit, Some(0), "{stderr}");
    let (exit, _, stderr) = check(dir.path(), &["inc", "--baseline", "base.json"]);
    assert_eq!(exit, Some(0), "{stderr}");
}

#[test]
fn a_directory_with_nothing_to_check_is_still_a_usage_error() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n",
    );
    fs::create_dir_all(dir.path().join("empty")).unwrap();
    write(&dir.path().join("inc/a.i"), INC);
    // Empty, and include-only with no context-free rule on.
    for target in ["empty", "inc"] {
        let (exit, _, stderr) = check(dir.path(), &[target]);
        assert_eq!(exit, Some(2), "{target}: {stderr}");
        assert!(stderr.contains("no ABL files"), "{target}: {stderr}");
    }
    // Include-only with lint off.
    write(&dir.path().join("oxabl.toml"), STRICT_ALL);
    let (exit, _, stderr) = check(dir.path(), &["inc", "--no-lint"]);
    assert_eq!(exit, Some(2), "{stderr}");
}

#[test]
fn the_includer_note_marks_only_what_an_includer_can_change() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), STRICT_ALL);
    write(&dir.path().join("src/name.i"), "\"x\"");
    write(&dir.path().join("src/dep.i"), "\"x\"{&suffix}");
    write(
        &dir.path().join("src/guard.i"),
        "&IF DEFINED(guard-once) = 0 &THEN\n&GLOBAL-DEFINE guard-once 1\n\
         RUN VALUE(c).\n&ENDIF\n",
    );
    write(
        &dir.path().join("src/inc.i"),
        "&SCOPED-DEFINE pfx {1}\n\
         RUN VALUE(c + {name.i}).\n\
         RUN VALUE(c + {dep.i}).\n\
         DEFINE {1} SHARED VARIABLE s AS INTEGER.\n\
         DEFINE {&mod} VARIABLE m AS INTEGER.\n\
         DEFINE VARIABLE p AS INTEGER.\n",
    );
    let (_, stdout, stderr) = check(dir.path(), &["src", "--json"]);
    let report = json(&stdout);
    let rows = |file: &str| -> Vec<(u64, String, String)> {
        report["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|d| d["path"].as_str().unwrap().ends_with(file))
            .filter(|d| ["CL001", "TX006"].contains(&d["code"].as_str().unwrap()))
            .map(|d| {
                (
                    d["start"]["line"].as_u64().unwrap(),
                    d["code"].as_str().unwrap().to_string(),
                    d["help"].as_str().unwrap_or_default().to_string(),
                )
            })
            .collect()
    };
    let note = |help: &str| help.contains("depends on the including file");
    let inc = rows("inc.i");
    assert_eq!(inc.len(), 5, "{inc:?}\n{stderr}");

    // The include that cannot change expands the same everywhere.
    assert_eq!((inc[0].0, note(&inc[0].2)), (2, false), "{inc:?}");
    assert_eq!((inc[1].0, note(&inc[1].2)), (3, true), "{inc:?}");
    // A literal SHARED keeps the producer and consumers help; a reference in
    // the slot says that an includer may share it, and never the plain help.
    assert_eq!(inc[2].0, 4);
    assert!(inc[2].2.contains("producer") && note(&inc[2].2), "{inc:?}");
    assert_eq!(inc[3].0, 5);
    assert!(
        inc[3]
            .2
            .contains("If an includer makes this definition `SHARED`")
            && note(&inc[3].2),
        "{inc:?}"
    );
    assert_eq!(inc[4].0, 6);
    assert_eq!(inc[4].2, "Add `NO-UNDO` to the definition.");

    // An include guard is not a dependence.
    let guard = rows("guard.i");
    assert_eq!(guard.len(), 1, "{guard:?}");
    assert!(!note(&guard[0].2), "{guard:?}");
}
