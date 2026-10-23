//! `oxabl check --format sarif` and inline `oxabl-ignore` suppressions.
//!
//! All fixtures are synthetic ABL. The SARIF check is structural: it asserts
//! the required members and enumerations of SARIF 2.1.0 that the output uses.

use std::fs;
use std::path::Path;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// Run `oxabl check --no-format <args>` in `dir`; `(exit, stdout, stderr)`.
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

fn workspace(source: &str) -> TempDir {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n",
    );
    write(&dir.path().join("src/a.p"), source);
    dir
}

const UNUSED: &str = "DEFINE VARIABLE v-old AS INTEGER NO-UNDO.\nMESSAGE \"a\".\n";

fn sarif(dir: &Path, extra: &[&str]) -> (Option<i32>, Value) {
    let mut args = vec!["--format", "sarif", "src"];
    args.extend_from_slice(extra);
    let (code, stdout, stderr) = check(dir, &args);
    let doc = serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("{e}: {stdout}\n{stderr}"));
    (code, doc)
}

fn results(doc: &Value) -> &Vec<Value> {
    doc["runs"][0]["results"].as_array().unwrap()
}

fn expect_str<'a>(v: &'a Value, what: &str) -> &'a str {
    v.as_str()
        .unwrap_or_else(|| panic!("{what} is not a string: {v}"))
}

/// The members of SARIF 2.1.0 that this output relies on.
fn assert_sarif_shape(doc: &Value) {
    assert_eq!(doc["version"], "2.1.0");
    let runs = doc["runs"].as_array().expect("runs");
    assert_eq!(runs.len(), 1);
    let driver = &runs[0]["tool"]["driver"];
    assert_eq!(driver["name"], "oxabl");
    expect_str(&driver["version"], "driver.version");
    expect_str(&driver["informationUri"], "driver.informationUri");
    for rule in driver["rules"].as_array().expect("rules") {
        expect_str(&rule["id"], "rule.id");
        expect_str(&rule["shortDescription"]["text"], "shortDescription.text");
        let level = expect_str(&rule["defaultConfiguration"]["level"], "level");
        assert!(["none", "note", "warning", "error"].contains(&level));
        assert!(rule["properties"]["tags"].is_array());
    }
    for result in runs[0]["results"].as_array().expect("results") {
        expect_str(&result["ruleId"], "ruleId");
        let level = expect_str(&result["level"], "level");
        assert!(["none", "note", "warning", "error"].contains(&level));
        expect_str(&result["message"]["text"], "message.text");
        for loc in result["locations"].as_array().expect("locations") {
            let phys = &loc["physicalLocation"];
            let uri = expect_str(&phys["artifactLocation"]["uri"], "uri");
            assert!(!uri.starts_with('/') && !uri.contains('\\'), "{uri}");
            if let Some(region) = phys.get("region") {
                assert!(region["startLine"].as_u64().unwrap() >= 1);
                assert!(region["startColumn"].as_u64().unwrap() >= 1);
            }
        }
        for (k, v) in result["partialFingerprints"].as_object().expect("fp") {
            assert!(!k.is_empty());
            assert!(v.is_string());
        }
        if let Some(state) = result.get("baselineState") {
            let state = expect_str(state, "baselineState");
            assert!(["new", "unchanged", "updated", "absent"].contains(&state));
        }
        for s in result["suppressions"].as_array().into_iter().flatten() {
            assert!(["inSource", "external"].contains(&expect_str(&s["kind"], "kind")));
        }
    }
}

fn fingerprint(result: &Value) -> String {
    let fps = result["partialFingerprints"].as_object().unwrap();
    assert_eq!(fps.len(), 1);
    fps.values().next().unwrap().as_str().unwrap().to_string()
}

#[test]
fn sarif_log_describes_the_driver_and_every_registry_rule() {
    let dir = workspace(UNUSED);
    let (code, doc) = sarif(dir.path(), &[]);
    assert_eq!(code, Some(1));
    assert_sarif_shape(&doc);
    let rules = doc["runs"][0]["tool"]["driver"]["rules"]
        .as_array()
        .unwrap();
    let unused = rules.iter().find(|r| r["id"] == "LINT0002").unwrap();
    assert_eq!(unused["name"], "unused-variable");
    assert_eq!(unused["defaultConfiguration"]["level"], "warning");
    assert!(
        unused["properties"]["tags"]
            .as_array()
            .unwrap()
            .contains(&Value::from("default"))
    );
    assert!(unused["help"]["text"].is_string());
    let strict = rules.iter().find(|r| r["id"] == "CL001").unwrap();
    assert!(
        strict["properties"]["tags"]
            .as_array()
            .unwrap()
            .contains(&Value::from("strict.calls"))
    );
}

#[test]
fn a_result_has_a_relative_uri_a_one_based_region_and_a_fingerprint() {
    let dir = workspace(UNUSED);
    let (_, doc) = sarif(dir.path(), &[]);
    let found = results(&doc);
    assert_eq!(found.len(), 1);
    let r = &found[0];
    assert_eq!(r["ruleId"], "LINT0002");
    assert_eq!(r["level"], "warning");
    let phys = &r["locations"][0]["physicalLocation"];
    assert_eq!(phys["artifactLocation"]["uri"], "src/a.p");
    assert_eq!(phys["region"]["startLine"], 1);
    assert!(phys["region"]["startColumn"].as_u64().unwrap() >= 1);
    assert!(r.get("baselineState").is_none());
    assert!(r.get("suppressions").is_none());
}

#[test]
fn a_fingerprint_does_not_depend_on_the_line_number() {
    let before = workspace(UNUSED);
    let after = workspace(&format!("/* header */\n\n{UNUSED}"));
    let (_, a) = sarif(before.path(), &[]);
    let (_, b) = sarif(after.path(), &[]);
    assert_ne!(
        results(&a)[0]["locations"][0]["physicalLocation"]["region"]["startLine"],
        results(&b)[0]["locations"][0]["physicalLocation"]["region"]["startLine"],
    );
    assert_eq!(fingerprint(&results(&a)[0]), fingerprint(&results(&b)[0]));
}

#[test]
fn a_fingerprint_differs_by_rule_path_and_statement() {
    let dir = workspace(UNUSED);
    write(
        &dir.path().join("src/b.p"),
        "DEFINE VARIABLE v-other AS INTEGER NO-UNDO.\n",
    );
    write(&dir.path().join("src/c.p"), UNUSED);
    let (_, doc) = sarif(dir.path(), &[]);
    let mut seen: Vec<String> = results(&doc).iter().map(fingerprint).collect();
    assert_eq!(seen.len(), 3);
    seen.sort();
    seen.dedup();
    assert_eq!(seen.len(), 3);
}

#[test]
fn sarif_output_does_not_change_the_exit_code() {
    let dirty = workspace(UNUSED);
    let (plain, _, _) = check(dirty.path(), &["src"]);
    let (with_sarif, _) = sarif(dirty.path(), &[]);
    assert_eq!(plain, Some(1));
    assert_eq!(with_sarif, plain);

    let clean = workspace("MESSAGE \"a\".\n");
    let (code, doc) = sarif(clean.path(), &[]);
    assert_eq!(code, Some(0));
    assert_sarif_shape(&doc);
    assert!(results(&doc).is_empty());
}

#[test]
fn baseline_state_marks_new_and_unchanged_results() {
    let dir = workspace(UNUSED);
    let base = dir.path().join("base.json");
    let base = base.to_str().unwrap();
    let (code, _, _) = check(
        dir.path(),
        &["src", "--baseline", base, "--update-baseline"],
    );
    assert_eq!(code, Some(0));
    write(
        &dir.path().join("src/b.p"),
        "DEFINE VARIABLE v-other AS INTEGER NO-UNDO.\n",
    );
    let (code, doc) = sarif(dir.path(), &["--baseline", base]);
    assert_eq!(code, Some(1));
    assert_sarif_shape(&doc);
    let mut states: Vec<(String, String)> = results(&doc)
        .iter()
        .map(|r| {
            let uri = r["locations"][0]["physicalLocation"]["artifactLocation"]["uri"]
                .as_str()
                .unwrap()
                .to_string();
            (uri, r["baselineState"].as_str().unwrap().to_string())
        })
        .collect();
    states.sort();
    assert_eq!(
        states,
        vec![
            ("src/a.p".to_string(), "unchanged".to_string()),
            ("src/b.p".to_string(), "new".to_string()),
        ]
    );
}

#[test]
fn a_suppression_with_a_reason_hides_the_finding_and_passes_the_gate() {
    let dir = workspace(&format!(
        "/* oxabl-ignore LINT0002: kept for the report layout */\n{UNUSED}"
    ));
    let (code, stdout, _) = check(dir.path(), &["src"]);
    assert_eq!(code, Some(0), "{stdout}");
    assert!(stdout.contains("no findings"), "{stdout}");
}

#[test]
fn a_trailing_suppression_covers_its_own_line() {
    let dir = workspace(
        "DEFINE VARIABLE v-old AS INTEGER NO-UNDO. // oxabl-ignore LINT0002: reason\nMESSAGE \"a\".\n",
    );
    let (code, _, _) = check(dir.path(), &["src"]);
    assert_eq!(code, Some(0));
}

#[test]
fn a_suppression_does_not_reach_another_line_or_rule() {
    let dir = workspace(&format!(
        "/* oxabl-ignore LINT0002: reason */\nMESSAGE \"b\".\n{UNUSED}"
    ));
    let (code, stdout, _) = check(dir.path(), &["src"]);
    assert_eq!(code, Some(1));
    assert!(stdout.contains("LINT0002"), "{stdout}");
    assert!(stdout.contains("LINT0008"), "the unused comment: {stdout}");
}

#[test]
fn a_suppressed_finding_stays_visible_in_sarif_with_its_reason() {
    let dir = workspace(&format!(
        "/* oxabl-ignore LINT0002: kept for the report layout */\n{UNUSED}"
    ));
    let (code, doc) = sarif(dir.path(), &[]);
    assert_eq!(code, Some(0));
    assert_sarif_shape(&doc);
    let found = results(&doc);
    assert_eq!(found.len(), 1);
    assert_eq!(found[0]["ruleId"], "LINT0002");
    assert_eq!(found[0]["suppressions"][0]["kind"], "inSource");
    assert_eq!(
        found[0]["suppressions"][0]["justification"],
        "kept for the report layout"
    );
}

#[test]
fn json_counts_suppressed_findings() {
    let dir = workspace(&format!("// oxabl-ignore LINT0002: reason\n{UNUSED}"));
    let (code, stdout, _) = check(dir.path(), &["--json", "src"]);
    assert_eq!(code, Some(0));
    let doc: Value = serde_json::from_str(&stdout).unwrap();
    assert_eq!(doc["suppressed"], 1);
    assert_eq!(doc["diagnostics"].as_array().unwrap().len(), 0);
}

#[test]
fn a_suppression_with_no_reason_does_not_suppress_and_is_reported() {
    let dir = workspace(&format!("// oxabl-ignore LINT0002\n{UNUSED}"));
    let (code, stdout, _) = check(dir.path(), &["--json", "src"]);
    assert_eq!(code, Some(1));
    let doc: Value = serde_json::from_str(&stdout).unwrap();
    let codes: Vec<&str> = doc["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["code"].as_str().unwrap())
        .collect();
    assert!(codes.contains(&"LINT0002"), "{codes:?}");
    assert!(codes.contains(&"LINT0007"), "{codes:?}");
    assert!(doc.get("suppressed").is_none());
}

#[test]
fn a_suppression_of_an_unknown_code_does_not_suppress_and_is_reported() {
    let dir = workspace(&format!("// oxabl-ignore NOPE999: reason\n{UNUSED}"));
    let (code, doc) = sarif(dir.path(), &[]);
    assert_eq!(code, Some(1));
    let ids: Vec<&str> = results(&doc)
        .iter()
        .map(|r| r["ruleId"].as_str().unwrap())
        .collect();
    assert!(
        ids.contains(&"LINT0002") && ids.contains(&"LINT0007"),
        "{ids:?}"
    );
    let invalid = results(&doc)
        .iter()
        .find(|r| r["ruleId"] == "LINT0007")
        .unwrap();
    assert!(
        invalid["message"]["text"]
            .as_str()
            .unwrap()
            .contains("NOPE999")
    );
    assert!(
        results(&doc)
            .iter()
            .all(|r| r.get("suppressions").is_none())
    );
}

#[test]
fn an_unused_suppression_is_reported_at_info_level() {
    let dir = workspace("// oxabl-ignore LINT0002: stale\nMESSAGE \"a\".\n");
    let (code, doc) = sarif(dir.path(), &[]);
    assert_eq!(code, Some(1));
    let found = results(&doc);
    assert_eq!(found.len(), 1);
    assert_eq!(found[0]["ruleId"], "LINT0008");
    assert_eq!(found[0]["level"], "note");
}

#[test]
fn a_suppression_of_a_rule_that_is_off_is_not_unused() {
    let dir = workspace("// oxabl-ignore CL001: needs the strict group\nMESSAGE \"a\".\n");
    let (code, _, stdout_err) = check(dir.path(), &["src"]);
    assert_eq!(code, Some(0), "{stdout_err}");
}

#[test]
fn format_json_matches_the_json_flag_and_the_two_conflict() {
    let dir = workspace(UNUSED);
    let (_, a, _) = check(dir.path(), &["--json", "src"]);
    let (_, b, _) = check(dir.path(), &["--format", "json", "src"]);
    assert_eq!(a, b);
    let (code, _, _) = check(dir.path(), &["--json", "--format", "sarif", "src"]);
    assert_eq!(code, Some(2));
}

#[test]
fn text_is_the_default_format() {
    let dir = workspace(UNUSED);
    let (code, stdout, _) = check(dir.path(), &["--format", "text", "src"]);
    assert_eq!(code, Some(1));
    assert!(stdout.contains("LINT0002"), "{stdout}");
}

#[test]
fn include_findings_are_sarif_results_in_the_include_file() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    );
    write(
        &dir.path().join("decl.i"),
        "DEFINE VARIABLE spare-v AS INTEGER NO-UNDO.\n",
    );
    write(&dir.path().join("prog.p"), "{decl.i}\nMESSAGE \"a\".\n");
    let (code, stdout, stderr) = check(
        dir.path(),
        &["--format", "sarif", "prog.p", "--include-findings"],
    );
    let doc: Value =
        serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("{e}: {stdout}\n{stderr}"));
    assert_eq!(code, Some(1));
    assert_sarif_shape(&doc);
    let found = results(&doc);
    assert_eq!(found.len(), 1, "{doc}");
    let location = &found[0]["locations"][0]["physicalLocation"];
    assert_eq!(location["artifactLocation"]["uri"], "decl.i");
    assert_eq!(location["region"]["startLine"], 1);
    assert_eq!(found[0]["ruleId"], "LINT0002");
}
