//! `oxabl check` and the `security` rule group.
//!
//! The group is off until enabled, a sink in an include is reported once in the
//! include, and severities follow the rule defaults. All fixtures are
//! synthetic ABL.

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

/// `(path, code, severity)` of every finding in a `--json` report.
fn findings(report: &serde_json::Value) -> Vec<(String, String, String)> {
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| {
            (
                d["path"].as_str().unwrap().replace('\\', "/"),
                d["code"].as_str().unwrap().to_string(),
                d["severity"].as_str().unwrap().to_string(),
            )
        })
        .collect()
}

fn config(lint: &str) -> String {
    format!(
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\", \"src\"]\n{lint}"
    )
}

const SINKS: &str = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE \" + c).
OUTPUT TO VALUE(c).
";

#[test]
fn the_group_is_off_by_default_and_strict_does_not_enable_it() {
    for lint in ["", "[workspace.lint]\nenable = [\"strict\"]\n"] {
        let dir = TempDir::new().unwrap();
        write(&dir.path().join("oxabl.toml"), &config(lint));
        write(&dir.path().join("a.p"), SINKS);
        let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
        let report = json(&stdout);
        assert!(
            findings(&report)
                .iter()
                .all(|(_, c, _)| !c.starts_with("ABLSEC")),
            "{report} {stderr}"
        );
    }
}

#[test]
fn enabling_the_group_reports_each_sink_at_its_default_severity() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("a.p"), SINKS);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let mut sec: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c.starts_with("ABLSEC"))
        .map(|(_, c, s)| (c, s))
        .collect();
    sec.sort();
    assert_eq!(
        sec,
        [
            ("ABLSEC001".to_string(), "error".to_string()),
            ("ABLSEC005".to_string(), "warning".to_string()),
        ]
    );
}

#[test]
fn a_rule_can_be_turned_off_inside_the_group() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\nABLSEC005 = \"off\"\n"),
    );
    write(&dir.path().join("a.p"), SINKS);
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let codes: Vec<_> = findings(&json(&stdout)).into_iter().map(|f| f.1).collect();
    assert!(codes.contains(&"ABLSEC001".to_string()), "{codes:?}");
    assert!(!codes.contains(&"ABLSEC005".to_string()), "{codes:?}");
}

#[test]
fn a_sink_in_an_include_is_reported_once_in_the_include() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("src/run.i"), "RUN VALUE(c).\n");
    write(
        &dir.path().join("src/b.p"),
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n{run.i}\n",
    );
    write(
        &dir.path().join("src/d.p"),
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n{run.i}\n",
    );
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let found: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c == "ABLSEC004")
        .collect();
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].0.ends_with("src/run.i"), "{found:?}");
}

#[test]
fn a_sink_from_a_define_is_reported_where_the_define_is_used() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(
        &dir.path().join("a.p"),
        "&GLOBAL-DEFINE PROG cProg\nDEFINE VARIABLE cProg AS CHARACTER NO-UNDO.\nRUN VALUE({&PROG}).\n",
    );
    write(
        &dir.path().join("safe.p"),
        "&GLOBAL-DEFINE PROG \"post.p\"\nRUN VALUE({&PROG}).\n",
    );
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let found: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c == "ABLSEC004")
        .collect();
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].0.ends_with("a.p"), "{found:?}");
}

#[test]
fn an_oxabl_ignore_comment_suppresses_a_security_finding() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(
        &dir.path().join("a.p"),
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE hQ AS HANDLE NO-UNDO.\n\
         // oxabl-ignore ABLSEC001: c comes from a fixed list\n\
         hQ:QUERY-PREPARE(c).\n\
         hQ:QUERY-PREPARE(c).\n",
    );
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    let lines: Vec<_> = report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["code"] == "ABLSEC001")
        .map(|d| d["start"]["line"].as_u64().unwrap())
        .collect();
    assert_eq!(lines, [5], "{report}");
}

#[test]
fn sarif_carries_the_security_severity_of_each_security_rule() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("a.p"), SINKS);
    let (_, stdout, _) = check(dir.path(), &[".", "--format", "sarif"]);
    let doc = json(&stdout);
    let rules = doc["runs"][0]["tool"]["driver"]["rules"]
        .as_array()
        .unwrap();
    let severity = |id: &str| {
        rules
            .iter()
            .find(|r| r["id"] == id)
            .unwrap_or_else(|| panic!("{id} is listed"))["properties"]["security-severity"]
            .clone()
    };
    assert_eq!(severity("ABLSEC001"), "8.0");
    assert_eq!(severity("ABLSEC002"), "9.5");
    assert_eq!(severity("ABLSEC003"), "9.5");
    assert_eq!(severity("ABLSEC004"), "9.5");
    assert_eq!(severity("ABLSEC005"), "8.0");
    let results: Vec<_> = doc["runs"][0]["results"]
        .as_array()
        .unwrap()
        .iter()
        .map(|r| r["ruleId"].as_str().unwrap().to_string())
        .filter(|id| id.starts_with("ABLSEC"))
        .collect();
    assert!(results.contains(&"ABLSEC001".to_string()), "{results:?}");
}

const MORE_SINKS: &str = "\
DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.
oWriter:Write(c).
oResp:SetHeader(\"Location\", c).
cPassword = \"hunter2hunter2\".
cHash = MD5-DIGEST(cPassword).
pcError = ERROR-STATUS:GET-MESSAGE(1).
oSer:Deserialize(c).
hDoc:LOAD(\"file\", c, FALSE).
SET-SIZE(m) = n.
DISABLE TRIGGERS FOR LOAD OF Customer.
";

#[test]
fn the_remaining_sink_rules_report_at_their_default_severities() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("a.p"), MORE_SINKS);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let mut sec: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c.starts_with("ABLSEC"))
        .map(|(_, c, s)| (c, s))
        .collect();
    sec.sort();
    let want = [
        ("ABLSEC006", "warning"),
        ("ABLSEC007", "warning"),
        ("ABLSEC008", "error"),
        ("ABLSEC009", "warning"),
        ("ABLSEC013", "warning"),
        ("ABLSEC014", "warning"),
        ("ABLSEC015", "info"),
        ("ABLSEC016", "info"),
        ("ABLSEC017", "warning"),
    ];
    let want: Vec<_> = want
        .iter()
        .map(|(c, s)| (c.to_string(), s.to_string()))
        .collect();
    assert_eq!(sec, want);
}

#[test]
fn a_hard_coded_secret_is_not_in_the_message_of_a_report() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("a.p"), MORE_SINKS);
    for args in [&[".", "--json"][..], &[".", "--format", "sarif"]] {
        let (_, stdout, stderr) = check(dir.path(), args);
        assert!(!stdout.contains("hunter2"), "{args:?}: {stdout}");
        assert!(!stderr.contains("hunter2"), "{args:?}: {stderr}");
    }
}

#[test]
fn an_override_lists_the_programs_where_integrity_statements_are_expected() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config(
            "[workspace.lint]\nenable = [\"security\"]\n\
             [[workspace.lint.overrides]]\nfiles = [\"utils/**\"]\nABLSEC017 = \"off\"\n",
        ),
    );
    let load = "DISABLE TRIGGERS FOR LOAD OF Customer.\n";
    write(&dir.path().join("utils/load.p"), load);
    write(&dir.path().join("app/run.p"), load);
    let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    let found: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c == "ABLSEC017")
        .collect();
    assert_eq!(found.len(), 1, "{found:?} {stderr}");
    assert!(found[0].0.ends_with("app/run.p"), "{found:?}");
}

#[test]
fn sarif_carries_the_security_severity_of_the_remaining_rules() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("a.p"), MORE_SINKS);
    let (_, stdout, _) = check(dir.path(), &[".", "--format", "sarif"]);
    let doc = json(&stdout);
    let rules = doc["runs"][0]["tool"]["driver"]["rules"]
        .as_array()
        .unwrap();
    let severity = |id: &str| {
        rules
            .iter()
            .find(|r| r["id"] == id)
            .unwrap_or_else(|| panic!("{id} is listed"))["properties"]["security-severity"]
            .clone()
    };
    for (id, want) in [
        ("ABLSEC006", "8.0"),
        ("ABLSEC007", "5.5"),
        ("ABLSEC008", "8.0"),
        ("ABLSEC009", "8.0"),
        ("ABLSEC013", "5.5"),
        ("ABLSEC014", "8.0"),
        ("ABLSEC015", "3.0"),
        ("ABLSEC016", "5.5"),
        ("ABLSEC017", "5.5"),
    ] {
        assert_eq!(severity(id), want, "{id}");
    }
}

const TAINTED: &str = "\
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFixed AS CHARACTER NO-UNDO.
cWhere = \"FOR EACH Customer WHERE Customer.Name = '\" + pcName + \"'\".
hQ:QUERY-PREPARE(cWhere).
cFixed = \"FOR EACH Customer\".
hQ:QUERY-PREPARE(cFixed).
";

#[test]
fn taint_mode_reports_only_what_untrusted_input_reaches_with_its_path() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config(
            "[workspace.lint]\nenable = [\"security\"]\n\
             [workspace.lint.security]\nmode = \"taint\"\n",
        ),
    );
    write(&dir.path().join("a.p"), TAINTED);
    let (_, stdout, _) = check(dir.path(), &[".", "--format", "sarif"]);
    let doc = json(&stdout);
    let results: Vec<&serde_json::Value> = doc["runs"][0]["results"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|r| r["ruleId"] == "ABLSEC001")
        .collect();
    assert_eq!(results.len(), 1, "{results:?}");
    let r = results[0];
    assert_eq!(
        r["locations"][0]["physicalLocation"]["region"]["startLine"],
        5
    );
    let text = r["message"]["text"].as_str().unwrap();
    assert!(
        text.contains("input parameter `pcName` → `cWhere`"),
        "{text}"
    );
    let steps: Vec<u64> = r["codeFlows"][0]["threadFlows"][0]["locations"]
        .as_array()
        .unwrap()
        .iter()
        .map(|l| {
            l["location"]["physicalLocation"]["region"]["startLine"]
                .as_u64()
                .unwrap()
        })
        .collect();
    assert_eq!(steps, [1, 4, 5]);

    // The default mode is stage 1: both sinks.
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let codes: Vec<String> = findings(&json(&stdout))
        .into_iter()
        .map(|(_, code, _)| code)
        .filter(|c| c == "ABLSEC001")
        .collect();
    assert_eq!(codes.len(), 2);
}

/// A two-file web app with an entry-point file: a handler that runs a
/// program with a dynamic query.
fn reach_app(dir: &Path, reach: &str) {
    write(
        &dir.join("oxabl.toml"),
        &config(&format!(
            "[workspace.lint]\nenable = [\"security\"]\n\
             [workspace.lint.security.reach]\nentry-points = \"entries.json\"\n{reach}"
        )),
    );
    write(
        &dir.join("entries.json"),
        r#"{"version": 1, "entries": [
            {"kind": "REST", "name": "GET /orders", "program": "web/orders.p"},
            {"kind": "REST", "name": "GET /gone", "program": "web/gone.p"}
        ]}"#,
    );
    write(
        &dir.join("src/web/orders.p"),
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = get-value(\"q\").\nRUN lib/find.p (INPUT c).\n",
    );
    write(
        &dir.join("src/lib/find.p"),
        "DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\nDEFINE VARIABLE hQ AS HANDLE NO-UNDO.\n\
         hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE \" + pc).\n",
    );
    write(
        &dir.join("src/batch/job.p"),
        "DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\nOS-COMMAND SILENT VALUE(pc).\n",
    );
}

#[test]
fn a_security_finding_says_how_the_entry_points_reach_it() {
    let dir = TempDir::new().unwrap();
    reach_app(dir.path(), "lower-unreachable = true\n");
    let (_, stdout, stderr) = check(dir.path(), &["."]);
    assert!(
        stdout.contains("(reachable from REST GET /orders)"),
        "{stdout}"
    );
    assert!(
        stdout.contains("(unreachable from the entry points)"),
        "{stdout}"
    );
    assert!(
        stderr.contains("entry point `REST GET /gone`: its target was not found"),
        "{stderr}"
    );

    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let report = json(&stdout);
    let find = |code: &str| {
        report["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .find(|d| d["code"] == code)
            .cloned()
            .unwrap_or_else(|| panic!("{code}: {report}"))
    };
    let q = find("ABLSEC001");
    assert_eq!(q["reach"]["status"], "reachable");
    assert_eq!(q["reach"]["entries"][0]["label"], "REST GET /orders");
    let job = find("ABLSEC003");
    assert_eq!(job["reach"]["status"], "unreachable");
    assert_eq!(job["severity"], "warning", "lowered from error");

    let (_, stdout, _) = check(dir.path(), &[".", "--format", "sarif"]);
    let log = json(&stdout);
    let results = log["runs"][0]["results"].as_array().unwrap();
    let q = results.iter().find(|r| r["ruleId"] == "ABLSEC001").unwrap();
    assert_eq!(
        q["properties"]["reachability"]["entries"][0]["label"],
        "REST GET /orders"
    );
    let related = &q["relatedLocations"][0]["physicalLocation"];
    assert_eq!(related["artifactLocation"]["uri"], "src/web/orders.p");
    assert_eq!(related["region"]["startLine"], 3);
}

const IDENTITY: &str = "\
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
cCompany = get-value(\"company\").
FIND FIRST Customer NO-LOCK WHERE Customer.Company = cCompany NO-ERROR.
FOR EACH Order NO-LOCK WHERE Order.Company = cCompany:
END.
";

#[test]
fn client_identity_is_one_finding_with_its_other_decisions_related() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config(
            "[workspace.lint]\nenable = [\"security\"]\n[workspace.lint.security]\nmode = \"taint\"\n",
        ),
    );
    write(&dir.path().join("a.p"), IDENTITY);
    let (_, stdout, _) = check(dir.path(), &[".", "--format", "sarif"]);
    let log = json(&stdout);
    let results: Vec<_> = log["runs"][0]["results"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|r| r["ruleId"] == "ABLSEC010")
        .collect();
    assert_eq!(results.len(), 1, "{log}");
    let r = results[0];
    assert_eq!(
        r["locations"][0]["physicalLocation"]["region"]["startLine"],
        3
    );
    let related = &r["relatedLocations"][0]["physicalLocation"];
    assert_eq!(related["artifactLocation"]["uri"], "a.p");
    assert_eq!(related["region"]["startLine"], 4);
}

#[test]
fn client_identity_is_silent_in_sink_mode_and_its_baseline_survives_a_new_decision() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("a.p"), IDENTITY);
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    assert!(
        findings(&json(&stdout))
            .iter()
            .all(|(_, c, _)| c != "ABLSEC010"),
        "{stdout}"
    );

    write(
        &dir.path().join("oxabl.toml"),
        &config(
            "[workspace.lint]\nenable = [\"security\"]\n[workspace.lint.security]\nmode = \"taint\"\n",
        ),
    );
    let (exit, _, stderr) = check(
        dir.path(),
        &[".", "--baseline", "b.json", "--update-baseline"],
    );
    assert_eq!(exit, Some(0), "{stderr}");
    write(
        &dir.path().join("a.p"),
        &format!("{IDENTITY}IF cCompany = \"\" THEN RETURN.\n"),
    );
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--baseline", "b.json", "--json"]);
    assert_eq!(exit, Some(0), "{stdout}{stderr}");
}
