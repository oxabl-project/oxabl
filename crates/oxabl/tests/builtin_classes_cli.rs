//! Built-in Progress classes through `oxabl check`: a built-in type name is no
//! undefined symbol, a member of a built-in class has the documented type, a
//! class that inherits a built-in class types its own calls, the opt-in
//! `CL006` and `CL007` rules report a call that no overload takes and a member
//! the class does not have, and a fact oxabl does not know gives no finding.
//! Every case runs the built binary over a temporary workspace and reads the
//! complete list of findings of one code for one file.

#![cfg(unix)]

#[path = "builtin_classes_ws/mod.rs"]
mod ws;

use std::io::Read;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use serde_json::Value;
use ws::{
    CLOSED, CONFIG, CONFIG_NO_SOURCES, CONFIG_STRICT, Finding, SURE, Workspace, calls_findings,
    calls_program, errors_lint0004, errors_program, finding, lint0004_of, mismatch, sure_lint0004,
    types_program, types_rows,
};

pub const CONFIG_NO_SOURCES_STRICT: &str = "[workspace]\nname = \"builtins\"\n\
    [workspace.lint]\nenable = [\"strict.calls\"]\n";

struct Report {
    source: String,
    diagnostics: Vec<Value>,
}

impl Report {
    /// The findings of `code`, sorted by position.
    fn findings(&self, code: &str) -> Vec<Finding> {
        let mut found: Vec<(u64, u64, Finding)> = self
            .diagnostics
            .iter()
            .filter(|d| d["code"] == code)
            .map(|d| {
                let start = d["span"]["start"].as_u64().unwrap();
                let end = d["span"]["end"].as_u64().unwrap();
                let line = d["start"]["line"].as_u64().unwrap();
                let text = self.source.lines().nth(line as usize - 1).unwrap().trim();
                (
                    line,
                    start,
                    (
                        text.to_string(),
                        d["message"].as_str().unwrap().to_string(),
                        self.source[start as usize..end as usize].to_string(),
                    ),
                )
            })
            .collect();
        found.sort_by_key(|(line, start, _)| (*line, *start));
        found.into_iter().map(|(.., finding)| finding).collect()
    }

    fn span_texts(&self, code: &str) -> Vec<String> {
        self.findings(code)
            .into_iter()
            .map(|(.., span)| span)
            .collect()
    }
}

/// `oxabl check --json --no-format file` in `ws`, killed after `limit`. Fails
/// when the file was not checked, when a member name after a colon is reported
/// as an undefined symbol, or when a type name is.
#[track_caller]
fn check_within(ws: &Workspace, file: &str, limit: Duration) -> Report {
    let mut child = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["check", "--json", "--no-format", file])
        .current_dir(&ws.root)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let out = std::thread::spawn(move || {
        let mut text = String::new();
        stdout.read_to_string(&mut text).ok();
        text
    });
    let err = std::thread::spawn(move || {
        let mut text = String::new();
        stderr.read_to_string(&mut text).ok();
        text
    });
    let deadline = Instant::now() + limit;
    loop {
        if child.try_wait().unwrap().is_some() {
            break;
        }
        if Instant::now() >= deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("`oxabl check {file}` did not end within {limit:?}");
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    let stdout = out.join().unwrap();
    let stderr = err.join().unwrap();
    let json: Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{stdout}\nstderr:\n{stderr}"));
    let failures = json["failures"].as_array().expect("failures array");
    assert!(
        failures.is_empty(),
        "`{file}` was not checked: {failures:?}"
    );
    let report = Report {
        source: std::fs::read_to_string(ws.path(file)).unwrap(),
        diagnostics: json["diagnostics"]
            .as_array()
            .expect("diagnostics array")
            .clone(),
    };
    assert_no_member_lint0001(&report);
    assert_no_type_lint0001(&report);
    report
}

#[track_caller]
fn check(ws: &Workspace, file: &str) -> Report {
    check_within(ws, file, Duration::from_secs(120))
}

/// No `LINT0001` names a word that follows a colon in the source: a member is
/// never an undefined symbol.
#[track_caller]
fn assert_no_member_lint0001(report: &Report) {
    let bytes = report.source.as_bytes();
    let mut members = Vec::new();
    for (at, byte) in bytes.iter().enumerate() {
        if *byte == b':' && bytes.get(at + 1).is_some_and(|b| b.is_ascii_alphabetic()) {
            let word: String = report.source[at + 1..]
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_')
                .collect();
            members.push(word);
        }
    }
    let wrong: Vec<_> = report
        .span_texts("LINT0001")
        .into_iter()
        .filter(|name| members.iter().any(|m| m.eq_ignore_ascii_case(name)))
        .collect();
    assert!(
        wrong.is_empty(),
        "LINT0001 names members as undefined symbols: {wrong:?}"
    );
}

/// No `LINT0001` names a type that the source writes after `AS`, `NEW` or
/// `USING`, or as the qualified receiver of a static member.
#[track_caller]
fn assert_no_type_lint0001(report: &Report) {
    let mut types: Vec<String> = Vec::new();
    for line in report.source.lines() {
        let words: Vec<&str> = line.split_whitespace().collect();
        for pair in words.windows(2) {
            if ["AS", "NEW", "USING"].contains(&pair[0].to_ascii_uppercase().as_str()) {
                types.push(type_word(pair[1]));
            }
        }
        for word in &words {
            if let Some(at) = word.find(':')
                && word[..at].contains('.')
            {
                types.push(type_word(&word[..at]));
            }
        }
    }
    let wrong: Vec<_> = report
        .span_texts("LINT0001")
        .into_iter()
        .filter(|name| {
            let name = type_word(name);
            types.iter().any(|t| t.eq_ignore_ascii_case(&name))
        })
        .collect();
    assert!(
        wrong.is_empty(),
        "LINT0001 names types as undefined symbols: {wrong:?}"
    );
}

/// A type as the source spells it, with the closing punctuation and the
/// generic arguments cut off.
fn type_word(word: &str) -> String {
    let word = word.split('<').next().unwrap_or(word);
    word.trim_end_matches(['.', '*', ')', ',']).to_string()
}

fn show(findings: &[Finding]) -> String {
    if findings.is_empty() {
        return "  (none)".to_string();
    }
    findings
        .iter()
        .map(|(line, message, span)| format!("  {line}  |  {message}  |  span `{span}`"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// The complete list of the findings of `code` equals `expected`.
#[track_caller]
fn assert_findings(report: &Report, code: &str, expected: Vec<Finding>) {
    let actual = report.findings(code);
    assert!(
        actual == expected,
        "{code} findings differ.\nexpected:\n{}\nactual:\n{}",
        show(&expected),
        show(&actual)
    );
}

#[track_caller]
fn assert_lint0004(report: &Report, expected: Vec<Finding>) {
    assert_findings(report, "LINT0004", expected);
}

/// Write `text` as `name`, check it and compare its `LINT0004` list.
#[track_caller]
fn assert_program(ws: &Workspace, name: &str, text: &str, expected: Vec<Finding>) {
    ws.write(name, text);
    assert_lint0004(&check(ws, name), expected);
}

// ---------------------------------------------------------------------------
// A. Names
// ---------------------------------------------------------------------------

const NAMES: &str = "USING Progress.Lang.*.\n\
    USING OpenEdge.Core.String.\n\
    DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE oBare AS AppError NO-UNDO.\n\
    DEFINE VARIABLE oStr AS OpenEdge.Core.String NO-UNDO.\n\
    DEFINE VARIABLE oShort AS String NO-UNDO.\n\
    oErr = NEW Progress.Lang.AppError().\n\
    oBare = NEW AppError(\"x\").\n\
    oStr = NEW OpenEdge.Core.String().\n\
    oShort = NEW String(\"y\").\n\
    MESSAGE oErr oBare oStr oShort.\n";

#[test]
fn a_built_in_type_name_is_no_undefined_symbol() {
    for config in [Some(CONFIG), Some(CONFIG_NO_SOURCES), None] {
        let ws = Workspace::new(config);
        ws.write("names.p", NAMES);
        let report = check(&ws, "names.p");
        assert_findings(&report, "LINT0001", vec![]);
    }
}

// ---------------------------------------------------------------------------
// B. Types of built-in members
// ---------------------------------------------------------------------------

#[test]
fn built_in_member_types() {
    let ws = Workspace::new(Some(CONFIG));
    assert_program(
        &ws,
        "types.p",
        &types_program(),
        lint0004_of(&types_rows(), true),
    );
}

#[test]
fn built_in_member_types_with_no_search_path() {
    let ws = Workspace::new(Some(CONFIG_NO_SOURCES));
    assert_program(
        &ws,
        "types.p",
        &types_program(),
        lint0004_of(&types_rows(), false),
    );
}

#[test]
fn a_user_class_that_inherits_a_built_in_class() {
    let ws = Workspace::new(Some(CONFIG));
    assert_program(&ws, "errors.p", &errors_program(), errors_lint0004());
}

#[test]
fn a_class_that_inherits_a_built_in_class_types_its_own_calls() {
    let want = vec![
        finding(
            "l = THIS-OBJECT:GetMessage(1).",
            mismatch("CHARACTER", "LOGICAL"),
            "l",
        ),
        finding(
            "l = SUPER:GetMessage(1).",
            mismatch("CHARACTER", "LOGICAL"),
            "l",
        ),
        finding("l = GetMessage(1).", mismatch("CHARACTER", "LOGICAL"), "l"),
        finding("l = NumMessages.", mismatch("INTEGER", "LOGICAL"), "l"),
    ];
    for config in [CONFIG, CONFIG_NO_SOURCES] {
        let ws = Workspace::new(Some(config));
        let report = check(&ws, "app/ErrorUser.cls");
        assert_lint0004(&report, want.clone());
        // `NumMessagez` is a name that no level has.
        assert_findings(&report, "LINT0001", vec![]);

        let report = check(&ws, "app/OrderError.cls");
        assert_lint0004(&report, vec![]);
        assert_findings(&report, "LINT0001", vec![]);
    }
}

// ---------------------------------------------------------------------------
// C. Fail-closed cases
// ---------------------------------------------------------------------------

#[test]
fn facts_the_catalog_does_not_have_give_no_finding() {
    let ws = Workspace::new(Some(CONFIG));
    ws.write("closed.p", CLOSED);
    let report = check(&ws, "closed.p");
    assert_lint0004(&report, vec![]);
    assert_findings(&report, "LINT0001", vec![]);
}

const UNSURE: &str = "USING acme.*.\n\
    USING Progress.Lang.*.\n\
    DEFINE VARIABLE oE AS AppError NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    l = oE:GetMessage(1).\n\
    MESSAGE oE l.\n";

const ACME_APP_ERROR: &str = "CLASS acme.AppError:\n\
    METHOD PUBLIC INTEGER GetMessage(INPUT p AS INTEGER):\n\
    RETURN p.\n\
    END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_bare_name_that_an_unread_import_may_supply_is_not_typed() {
    let one = |from: &str| {
        vec![finding(
            "l = oE:GetMessage(1).",
            mismatch(from, "LOGICAL"),
            "l",
        )]
    };

    let ws = Workspace::new(Some(CONFIG_NO_SOURCES));
    assert_program(&ws, "unsure.p", UNSURE, vec![]);

    let ws = Workspace::new(Some(CONFIG));
    assert_program(&ws, "unsure.p", UNSURE, one("CHARACTER"));

    ws.write("acme/AppError.cls", ACME_APP_ERROR);
    assert_program(&ws, "unsure.p", UNSURE, one("INTEGER"));
}

#[test]
fn a_sure_built_in_name_is_typed_with_no_search_path() {
    for config in [Some(CONFIG_NO_SOURCES), None] {
        let ws = Workspace::new(config);
        assert_program(&ws, "sure.p", SURE, sure_lint0004());
    }
}

#[test]
fn a_workspace_copy_of_an_openedge_class_wins() {
    let ws = Workspace::new(Some(CONFIG));
    ws.write(
        "OpenEdge/Core/String.cls",
        "CLASS OpenEdge.Core.String:\n\
         DEFINE PUBLIC PROPERTY Value AS CHARACTER NO-UNDO GET. SET.\n\
         END CLASS.\n",
    );
    assert_program(
        &ws,
        "copy.p",
        "DEFINE VARIABLE oStr AS OpenEdge.Core.String NO-UNDO.\n\
         DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
         i = oStr:Value.\n\
         MESSAGE oStr i.\n",
        vec![finding(
            "i = oStr:Value.",
            mismatch("CHARACTER", "INTEGER"),
            "i",
        )],
    );
}

// ---------------------------------------------------------------------------
// F. Facts the compiler settles (OpenEdge 12.8.9)
// ---------------------------------------------------------------------------

const SHADOW: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
    l = oErr:Extra.\n\
    l = oErr:GetMessage(1).\n\
    i = NEW Progress.Lang.AppError().\n\
    MESSAGE oErr:Extra.\n\
    MESSAGE oErr:GetMessage(1).\n\
    MESSAGE oErr l i.\n";

/// A workspace file `Progress/Lang/AppError.cls` on the include path does not
/// replace the built-in class: the type is the built-in class.
#[test]
fn a_workspace_file_with_a_progress_name_does_not_replace_the_built_in_class() {
    let shadow = "CLASS Progress.Lang.AppError:\n\
         DEFINE PUBLIC PROPERTY Extra AS INTEGER NO-UNDO GET. SET.\n\
         END CLASS.\n";

    let ws = Workspace::new(Some(CONFIG));
    ws.write("Progress/Lang/AppError.cls", shadow);
    assert_program(
        &ws,
        "shadow.p",
        SHADOW,
        vec![
            finding(
                "l = oErr:GetMessage(1).",
                mismatch("CHARACTER", "LOGICAL"),
                "l",
            ),
            finding(
                "i = NEW Progress.Lang.AppError().",
                mismatch("Progress.Lang.AppError", "INTEGER"),
                "i",
            ),
        ],
    );

    // The built-in class has no `Extra`, so the opt-in rule names it.
    let ws = Workspace::new(Some(CONFIG_STRICT));
    ws.write("Progress/Lang/AppError.cls", shadow);
    ws.write("shadow.p", SHADOW);
    let report = check(&ws, "shadow.p");
    assert_findings(&report, "CL006", vec![]);
    assert_findings(
        &report,
        "CL007",
        vec![finding(
            "MESSAGE oErr:Extra.",
            ws::no_member("Progress.Lang.AppError", "Extra"),
            "Extra",
        )],
    );
}

const BARE: &str = "DEFINE VARIABLE oE AS AppError NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    l = oE:GetMessage(1).\n\
    MESSAGE oE l.\n";

/// A bare name with a class in the unnamed package and no `USING` binds the
/// workspace class; `USING Progress.Lang.*` binds the built-in class.
#[test]
fn an_unnamed_package_class_wins_a_bare_name_unless_the_import_names_progress_lang() {
    let one = |from: &str| {
        vec![finding(
            "l = oE:GetMessage(1).",
            mismatch(from, "LOGICAL"),
            "l",
        )]
    };
    let ws = Workspace::new(Some(CONFIG));
    ws.write(
        "AppError.cls",
        "CLASS AppError:\n\
         METHOD PUBLIC INTEGER GetMessage(INPUT p AS INTEGER):\n\
         RETURN p.\n\
         END METHOD.\n\
         END CLASS.\n",
    );
    assert_program(&ws, "bare1.p", BARE, one("INTEGER"));
    assert_program(
        &ws,
        "bare2.p",
        &format!("USING Progress.Lang.*.\n{BARE}"),
        one("CHARACTER"),
    );

    // With no workspace class the implicit `Progress.Lang` import supplies it.
    std::fs::remove_file(ws.path("AppError.cls")).unwrap();
    assert_program(&ws, "bare1.p", BARE, one("CHARACTER"));
}

const IFACE: &str = "DEFINE VARIABLE oS AS app.IShape NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
    l = oS:ToString().\n\
    c = oS:ToString().\n\
    i = oS:Equals(oS).\n\
    l = oS:GetClass():TypeName.\n\
    DO ON ERROR UNDO, LEAVE:\n\
    \x20   i = 1.\n\
    \x20   CATCH e AS Progress.Lang.Error:\n\
    \x20       c = e:ToString().\n\
    \x20       l = e:ToString().\n\
    \x20       i = e:Equals(e).\n\
    \x20       l = e:GetClass():TypeName.\n\
    \x20       MESSAGE e:Bogus().\n\
    \x20   END CATCH.\n\
    END.\n\
    MESSAGE oS l i c.\n";

/// A variable `AS` an interface has the members of `Progress.Lang.Object`.
#[test]
fn a_variable_of_an_interface_type_has_the_members_of_progress_lang_object() {
    let bad =
        |line: &str, from: &str, to: &str, span: &str| finding(line, mismatch(from, to), span);
    let ws = Workspace::new(Some(CONFIG));
    assert_program(
        &ws,
        "iface.p",
        IFACE,
        vec![
            bad("l = oS:ToString().", "CHARACTER", "LOGICAL", "l"),
            bad("i = oS:Equals(oS).", "LOGICAL", "INTEGER", "i"),
            bad("l = oS:GetClass():TypeName.", "CHARACTER", "LOGICAL", "l"),
            bad("l = e:ToString().", "CHARACTER", "LOGICAL", "l"),
            bad("i = e:Equals(e).", "LOGICAL", "INTEGER", "i"),
            bad("l = e:GetClass():TypeName.", "CHARACTER", "LOGICAL", "l"),
        ],
    );

    // No object method is an absent member; a name that no level has is.
    let ws = Workspace::new(Some(CONFIG_STRICT));
    ws.write("iface.p", IFACE);
    let report = check(&ws, "iface.p");
    assert_findings(&report, "CL006", vec![]);
    assert_findings(
        &report,
        "CL007",
        vec![finding(
            "MESSAGE e:Bogus().",
            ws::no_member("Progress.Lang.Error", "Bogus"),
            "Bogus",
        )],
    );
}

// ---------------------------------------------------------------------------
// D. CL006 and CL007
// ---------------------------------------------------------------------------

#[test]
fn calls_that_no_overload_takes_and_unknown_members() {
    let ws = Workspace::new(Some(CONFIG_STRICT));
    ws.write("calls.p", &calls_program(true));
    let report = check(&ws, "calls.p");
    let (cl006, cl007) = calls_findings(true);
    assert_findings(&report, "CL006", cl006);
    assert_findings(&report, "CL007", cl007);
}

#[test]
fn the_member_rules_are_off_by_default() {
    let ws = Workspace::new(Some(CONFIG));
    ws.write("calls.p", &calls_program(true));
    let report = check(&ws, "calls.p");
    assert_findings(&report, "CL006", vec![]);
    assert_findings(&report, "CL007", vec![]);
}

#[test]
fn the_member_rules_need_no_search_path() {
    let ws = Workspace::new(Some(CONFIG_NO_SOURCES_STRICT));
    ws.write("calls.p", &calls_program(false));
    let report = check(&ws, "calls.p");
    let (cl006, cl007) = calls_findings(false);
    assert_findings(&report, "CL006", cl006);
    assert_findings(&report, "CL007", cl007);
}

// ---------------------------------------------------------------------------
// E. Existing rules that could get a new false positive
// ---------------------------------------------------------------------------

#[test]
fn an_argument_of_a_built_in_call_is_used_and_read() {
    let ws = Workspace::new(Some(CONFIG));
    ws.write(
        "args.p",
        "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
         DEFINE VARIABLE iIdx AS INTEGER NO-UNDO.\n\
         iIdx = 1.\n\
         MESSAGE oErr:GetMessage(iIdx).\n",
    );
    let report = check(&ws, "args.p");
    assert_findings(&report, "LINT0002", vec![]);
    assert_findings(&report, "LINT0006", vec![]);
}

#[test]
fn signature_mismatch_reads_built_in_member_types() {
    let ws = Workspace::new(Some(CONFIG_STRICT));
    ws.write(
        "run.p",
        "DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.\n\
         DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
         RUN takes-handle (oJson:Has(\"a\")).\n\
         RUN takes-handle (oErr:GetMessage(1)).\n\
         PROCEDURE takes-handle:\n\
         \x20   DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.\n\
         END PROCEDURE.\n",
    );
    let report = check(&ws, "run.p");
    let found = report.findings("CL003");
    assert_eq!(
        found.len(),
        1,
        "exactly one CL003 finding, not {}:\n{}",
        found.len(),
        show(&found)
    );
    assert_eq!(found[0].0, "RUN takes-handle (oJson:Has(\"a\")).");
    assert_eq!(
        found[0].1,
        "argument 1 of `RUN takes-handle` is LOGICAL, but the parameter is HANDLE"
    );
}

#[test]
fn a_class_that_overrides_an_object_method_gives_no_finding_alone() {
    let ws = Workspace::new(Some(CONFIG));
    for file in ["app/Named.cls", "app/OrderError.cls"] {
        let report = check(&ws, file);
        assert_lint0004(&report, vec![]);
    }
}
