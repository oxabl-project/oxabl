//! The types of class members through `oxabl check`: a property or a method
//! read through a variable, a chain, `NEW`, `CAST`, `THIS-OBJECT` or `SUPER`
//! has the type the class declares, an overload is picked only when the
//! arguments pick one, and a fact oxabl does not know gives no finding. Every
//! case runs the built binary over a copy of the workspace of
//! `class_members_ws` and reads the complete `LINT0004` list of one file.

#![cfg(unix)]

#[path = "class_members_ws/mod.rs"]
mod ws;

use std::io::Read;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use serde_json::Value;
use ws::{
    BASIC_ROWS, CHAIN_ROWS, EXTENT_ROWS, INHERITED_ROWS, INTERFACE_ROWS, METHOD_ROWS,
    NEW_CAST_ROWS, PROPERTY_ROWS, Row, Workspace, basic_program, expected, mismatch,
};

/// One finding: the trimmed text of its line, its message and the text of its span.
type Finding = (String, String, String);

struct Report {
    source: String,
    diagnostics: Vec<Value>,
    failures: Vec<Value>,
}

/// The `LINT0004` list that a case wants, as owned strings.
fn list(rows: &[(&str, String, &str)]) -> Vec<Finding> {
    rows.iter()
        .map(|(line, message, span)| (line.to_string(), message.clone(), span.to_string()))
        .collect()
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

    fn lint0004(&self) -> Vec<Finding> {
        self.findings("LINT0004")
    }

    fn span_texts(&self, code: &str) -> Vec<String> {
        self.findings(code)
            .into_iter()
            .map(|(.., span)| span)
            .collect()
    }
}

/// `oxabl check --json --no-format file` in `ws`, killed after `limit`.
/// Fails when the file was not checked, or when a member name after a colon
/// is reported as an undefined symbol.
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
    let report: Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{stdout}\nstderr:\n{stderr}"));
    let failures = report["failures"]
        .as_array()
        .expect("failures array")
        .clone();
    let report = Report {
        source: std::fs::read_to_string(ws.path(file)).unwrap(),
        diagnostics: report["diagnostics"]
            .as_array()
            .expect("diagnostics array")
            .clone(),
        failures,
    };
    assert!(
        report.failures.is_empty(),
        "`{file}` was not checked: {:?}",
        report.failures
    );
    assert_no_member_lint0001(&report);
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

#[track_caller]
fn assert_findings(report: &Report, expected: Vec<Finding>) {
    let actual = report.lint0004();
    assert!(
        actual == expected,
        "LINT0004 findings differ.\nexpected:\n{}\nactual:\n{}",
        show(&expected),
        show(&actual)
    );
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

/// Write `text` as `name`, check it and compare its `LINT0004` list.
#[track_caller]
fn assert_program(ws: &Workspace, name: &str, text: &str, expected: Vec<Finding>) {
    ws.write(name, text);
    assert_findings(&check(ws, name), expected);
}

#[track_caller]
fn assert_topics(topics: &[&[Row]]) {
    let ws = Workspace::new();
    assert_program(&ws, "basic.p", &basic_program(topics), expected(topics));
}

// ---------------------------------------------------------------------------
// A. A mismatch through a member is reported, a correct assignment is not
// ---------------------------------------------------------------------------

#[test]
fn a_property_is_read_and_assigned_with_its_declared_type() {
    assert_topics(&[PROPERTY_ROWS]);
}

#[test]
fn a_method_call_has_the_return_type() {
    assert_topics(&[METHOD_ROWS]);
}

#[test]
fn a_chain_through_properties_and_method_returns_is_followed() {
    assert_topics(&[CHAIN_ROWS]);
}

#[test]
fn new_and_cast_receivers_resolve() {
    assert_topics(&[NEW_CAST_ROWS]);
}

#[test]
fn members_inherited_across_files_resolve() {
    assert_topics(&[INHERITED_ROWS]);
}

#[test]
fn interface_members_and_inherited_interface_members_resolve() {
    assert_topics(&[INTERFACE_ROWS]);
}

#[test]
fn extent_on_a_property_and_on_a_return_type_is_kept() {
    assert_topics(&[EXTENT_ROWS]);
}

#[test]
fn every_row_of_basic_p_together() {
    assert_topics(BASIC_ROWS);
}

// ---------------------------------------------------------------------------
// B. THIS-OBJECT, SUPER and access
// ---------------------------------------------------------------------------

#[test]
fn this_object_and_super_see_the_members_the_class_can_see() {
    let ws = Workspace::new();
    let report = check(&ws, "app/Employee.cls");
    assert_findings(
        &report,
        list(&[
            (
                "l = THIS-OBJECT:Salary.",
                mismatch("DECIMAL", "LOGICAL"),
                "l",
            ),
            ("l = THIS-OBJECT:Age.", mismatch("INTEGER", "LOGICAL"), "l"),
            (
                "l = THIS-OBJECT:Level.",
                mismatch("INTEGER", "LOGICAL"),
                "l",
            ),
            ("l = SUPER:Greet().", mismatch("CHARACTER", "LOGICAL"), "l"),
            (
                "l = THIS-OBJECT:Greet().",
                mismatch("CHARACTER", "LOGICAL"),
                "l",
            ),
            ("l = THIS-OBJECT.", mismatch("app.employee", "LOGICAL"), "l"),
        ]),
    );
    let unused = report.span_texts("LINT0006");
    assert!(
        !unused.iter().any(|name| name == "l" || name == "d"),
        "`l` and `d` are read by the MESSAGE: {unused:?}"
    );
    let undefined = report.span_texts("LINT0001");
    for name in ["Secret", "Salary", "Age", "Level", "Greet"] {
        assert!(
            !undefined.iter().any(|n| n == name),
            "LINT0001 on `{name}`: {undefined:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// C. Overloads
// ---------------------------------------------------------------------------

const OVERLOADS: &str = r#"DEFINE VARIABLE oSc AS app.Scorer NO-UNDO.
DEFINE VARIABLE oSub AS app.SubScorer NO-UNDO.
DEFINE VARIABLE oP AS app.Person NO-UNDO.
DEFINE VARIABLE oE AS app.Employee NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE i64 AS INT64 NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
l = oSc:Score(1).
l = oSc:Score("a").
l = oSc:Score(?).
l = oSc:Score(1, 2).
l = oSc:Score(1, 2, 3).
l = oSc:Score(i).
l = oSc:Widen(i).
l = oSc:Widen(i64).
i = oSc:Same(?).
l = oSc:Fill(c).
l = oSc:Pick(oP).
l = oSc:Pick(oE).
l = oSc:Pick(?).
l = oSc:Who(1):Age.
l = oSc:Who2(1):Age.
l = oSub:Score(1/1/2026).
l = oSub:Score(1).
l = oSub:Score(?).
MESSAGE l i i64 c oSc oSub oP oE.
"#;

#[test]
fn an_overload_is_picked_only_when_the_arguments_pick_one() {
    let ws = Workspace::new();
    ws.write("overloads.p", OVERLOADS);
    let report = check(&ws, "overloads.p");
    // `Who(1):Age` may later give this finding, never another one.
    let allowed = (
        "l = oSc:Who(1):Age.".to_string(),
        mismatch("INTEGER", "LOGICAL"),
        "l".to_string(),
    );
    let actual: Vec<Finding> = report
        .lint0004()
        .into_iter()
        .filter(|finding| *finding != allowed)
        .collect();
    let expected = list(&[
        ("l = oSc:Score(1).", mismatch("INTEGER", "LOGICAL"), "l"),
        (
            "l = oSc:Score(\"a\").",
            mismatch("CHARACTER", "LOGICAL"),
            "l",
        ),
        ("l = oSc:Score(1, 2).", mismatch("DECIMAL", "LOGICAL"), "l"),
        ("l = oSc:Score(i).", mismatch("INTEGER", "LOGICAL"), "l"),
        ("l = oSc:Widen(i64).", mismatch("INT64", "LOGICAL"), "l"),
        ("i = oSc:Same(?).", mismatch("LOGICAL", "INTEGER"), "i"),
        ("l = oSc:Pick(oP).", mismatch("INTEGER", "LOGICAL"), "l"),
        ("l = oSc:Pick(oE).", mismatch("INTEGER", "LOGICAL"), "l"),
        ("l = oSc:Who2(1):Age.", mismatch("INTEGER", "LOGICAL"), "l"),
        (
            "l = oSub:Score(1/1/2026).",
            mismatch("DATE", "LOGICAL"),
            "l",
        ),
        ("l = oSub:Score(1).", mismatch("INTEGER", "LOGICAL"), "l"),
    ]);
    assert!(
        actual == expected,
        "LINT0004 findings differ.\nexpected:\n{}\nactual:\n{}",
        show(&expected),
        show(&actual)
    );
}

#[test]
fn unqualified_and_this_object_calls_agree() {
    let ws = Workspace::new();
    assert_findings(
        &check(&ws, "app/SubScorer.cls"),
        list(&[
            ("l = Score(1).", mismatch("INTEGER", "LOGICAL"), "l"),
            (
                "l = THIS-OBJECT:Score(1).",
                mismatch("INTEGER", "LOGICAL"),
                "l",
            ),
            (
                "l = THIS-OBJECT:Score(1/1/2026).",
                mismatch("DATE", "LOGICAL"),
                "l",
            ),
        ]),
    );
}

const MODE_CLASS: &str = r#"CLASS app.Md:
    METHOD PUBLIC INTEGER F(INPUT p AS DECIMAL):
        RETURN 0.
    END METHOD.
    METHOD PUBLIC CHARACTER F(OUTPUT p AS INTEGER):
        p = 1.
        RETURN "".
    END METHOD.
END CLASS.
"#;

/// The compiler counts the mode written at the call: `F(OUTPUT d)` picks the
/// `OUTPUT INTEGER` overload, `F(d)` the `INPUT DECIMAL` one. Both lines are
/// correct code.
#[test]
fn the_mode_written_at_the_call_takes_part_in_the_pick() {
    let ws = Workspace::new();
    ws.write("app/Md.cls", MODE_CLASS);
    assert_program(
        &ws,
        "modes.p",
        r#"DEFINE VARIABLE o AS app.Md NO-UNDO.
DEFINE VARIABLE d AS DECIMAL NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
o = NEW app.Md().
c = o:F(OUTPUT d).
i = o:F(d).
MESSAGE c i d.
"#,
        Vec::new(),
    );
}

const STATIC_CLASS: &str = r#"CLASS app.St:
    METHOD PUBLIC STATIC INTEGER M(i AS INT64):
        RETURN 0.
    END METHOD.
    METHOD PUBLIC CHARACTER M(i AS INTEGER):
        RETURN "".
    END METHOD.
    METHOD PUBLIC STATIC VOID SRun():
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        i = M(1).
        MESSAGE i.
    END METHOD.
END CLASS.
"#;

/// The compiler picks the instance overload and then rejects the call
/// (error 14422); it does not keep the static candidates. oxabl gives no type
/// finding on a call that the compiler rejects.
#[test]
fn an_unqualified_call_in_a_static_method_gives_no_type_finding() {
    let ws = Workspace::new();
    ws.write("app/St.cls", STATIC_CLASS);
    assert_findings(&check(&ws, "app/St.cls"), Vec::new());
}

/// `app.St` with the overloads of `STATIC_CLASS` and `member` in place of
/// `SRun`; `member` holds the call `i = M(1).`.
fn static_context_class(member: &str) -> String {
    format!(
        r#"CLASS app.St:
    METHOD PUBLIC STATIC INTEGER M(i AS INT64):
        RETURN 0.
    END METHOD.
    METHOD PUBLIC CHARACTER M(i AS INTEGER):
        RETURN "".
    END METHOD.
{member}END CLASS.
"#
    )
}

const STATIC_CONSTRUCTOR: &str = r#"    CONSTRUCTOR STATIC St():
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        i = M(1).
        MESSAGE i.
    END CONSTRUCTOR.
"#;

const STATIC_PROPERTY: &str = r#"    DEFINE PUBLIC STATIC PROPERTY P AS INTEGER NO-UNDO
    GET():
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        i = M(1).
        MESSAGE i.
        RETURN 0.
    END GET.
"#;

const INSTANCE_PROPERTY: &str = r#"    DEFINE PUBLIC PROPERTY P AS INTEGER NO-UNDO
    GET():
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        i = M(1).
        MESSAGE i.
        RETURN 0.
    END GET.
"#;

/// Same call as in a static method: the compiler rejects it (error 14422), so
/// oxabl gives no type finding in a static constructor.
#[test]
fn an_unqualified_call_in_a_static_constructor_gives_no_type_finding() {
    let ws = Workspace::new();
    ws.write("app/St.cls", &static_context_class(STATIC_CONSTRUCTOR));
    assert_findings(&check(&ws, "app/St.cls"), Vec::new());
}

/// Same call in the `GET` of a static property: no type finding.
#[test]
fn an_unqualified_call_in_a_static_property_getter_gives_no_type_finding() {
    let ws = Workspace::new();
    ws.write("app/St.cls", &static_context_class(STATIC_PROPERTY));
    assert_findings(&check(&ws, "app/St.cls"), Vec::new());
}

/// Control: in the `GET` of an instance property the call is legal and picks
/// the instance overload, which returns CHARACTER.
#[test]
fn an_unqualified_call_in_an_instance_property_getter_is_typed() {
    let ws = Workspace::new();
    ws.write("app/St.cls", &static_context_class(INSTANCE_PROPERTY));
    assert_findings(
        &check(&ws, "app/St.cls"),
        list(&[("i = M(1).", mismatch("CHARACTER", "INTEGER"), "i")]),
    );
}

const ARITHMETIC_CLASS: &str = r#"CLASS app.Ar:
    METHOD PUBLIC CHARACTER M(p AS INTEGER):
        RETURN "".
    END METHOD.
    METHOD PUBLIC INTEGER M(p AS INT64):
        RETURN 0.
    END METHOD.
END CLASS.
"#;

/// INTEGER arithmetic is INTEGER for the pick, so `M(i + 1)` picks
/// `M(INTEGER)`, which returns CHARACTER.
#[test]
fn integer_arithmetic_as_an_argument_picks_the_integer_overload() {
    let ws = Workspace::new();
    ws.write("app/Ar.cls", ARITHMETIC_CLASS);
    assert_program(
        &ws,
        "arith.p",
        r#"DEFINE VARIABLE o AS app.Ar NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
o = NEW app.Ar().
c = o:M(i + 1).
i = o:M(i + 1).
MESSAGE c i.
"#,
        list(&[("i = o:M(i + 1).", mismatch("CHARACTER", "INTEGER"), "i")]),
    );
}

const CONFIG_NO_SOURCES: &str = "[workspace]\nname = \"members\"\n";

#[test]
fn a_class_of_the_analysed_file_needs_no_index() {
    let wanted = || {
        list(&[
            ("l = Rank(1).", mismatch("INTEGER", "LOGICAL"), "l"),
            (
                "l = THIS-OBJECT:Rank(\"a\").",
                mismatch("CHARACTER", "LOGICAL"),
                "l",
            ),
        ])
    };
    for config in [ws::CONFIG, CONFIG_NO_SOURCES] {
        let ws = Workspace::new();
        ws.write("oxabl.toml", config);
        let report = check(&ws, "app/LocalScore.cls");
        assert_findings(&report, wanted());
        for code in ["LINT0002", "LINT0006"] {
            assert!(
                !report.span_texts(code).iter().any(|name| name == "n"),
                "{code} on `n`, which is read as an argument ({config:?})"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// D. Static members and enums
// ---------------------------------------------------------------------------

#[test]
fn static_members_and_enum_members_resolve_through_the_type_name() {
    let ws = Workspace::new();
    assert_program(
        &ws,
        "static.p",
        r#"DEFINE VARIABLE l AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE eShade AS app.Shade NO-UNDO.
l = app.Person:Count.
i = app.Person:Count.
l = app.Person:Create("x"):Age.
eShade = app.Shade:Dark.
i = app.Shade:Dark.
MESSAGE l i eShade.
"#,
        list(&[
            ("l = app.Person:Count.", mismatch("INTEGER", "LOGICAL"), "l"),
            (
                "l = app.Person:Create(\"x\"):Age.",
                mismatch("INTEGER", "LOGICAL"),
                "l",
            ),
            ("i = app.Shade:Dark.", mismatch("app.shade", "INTEGER"), "i"),
        ]),
    );
}

// ---------------------------------------------------------------------------
// E. USING imports and package names
// ---------------------------------------------------------------------------

#[test]
fn using_brings_a_class_name_in_scope() {
    let ws = Workspace::new();
    assert_program(
        &ws,
        "using.p",
        r#"USING app.*.
DEFINE VARIABLE oP AS Person NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
l = Person:Count.
l = oP:Age.
MESSAGE oP l.
"#,
        list(&[
            ("l = Person:Count.", mismatch("INTEGER", "LOGICAL"), "l"),
            ("l = oP:Age.", mismatch("INTEGER", "LOGICAL"), "l"),
        ]),
    );
}

#[test]
fn a_type_name_in_a_class_file_follows_that_files_own_using() {
    // `Person` in app/Holder.cls is app.Person; the program's `USING other.*.`
    // would make it other.Person, whose Age is LOGICAL and gives no finding.
    let ws = Workspace::new();
    assert_program(
        &ws,
        "imports.p",
        r#"USING other.*.
DEFINE VARIABLE oH AS app.Holder NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
l = oH:Owner:Age.
MESSAGE oH l.
"#,
        list(&[("l = oH:Owner:Age.", mismatch("INTEGER", "LOGICAL"), "l")]),
    );
}

// ---------------------------------------------------------------------------
// F. Fail closed
// ---------------------------------------------------------------------------

const CLOSED: &str = r#"DEFINE VARIABLE oP AS app.Person NO-UNDO.
DEFINE VARIABLE oX AS app.Missing NO-UNDO.
DEFINE VARIABLE oNamed AS app.Named NO-UNDO.
DEFINE VARIABLE oSc AS app.Scorer NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
l = oX:Name.
l = oX:Greet().
i = h:NAME.
i = SESSION:DATE-FORMAT.
i = DYNAMIC-INVOKE(oP, "Greet").
l = oP:NoSuch.
l = oP:Secret.
l = oP:Level.
l = oP:Count.
l = app.Person:Age.
l = oSc:Score(undefinedVar).
MESSAGE oP oX oNamed oSc h l i.
"#;

#[test]
fn an_unknown_class_member_access_or_argument_gives_no_finding() {
    let ws = Workspace::new();
    assert_program(&ws, "closed.p", CLOSED, Vec::new());
    let report = check(&ws, "closed.p");
    let undefined = report.span_texts("LINT0001");
    for name in [
        "Name",
        "Greet",
        "NAME",
        "DATE-FORMAT",
        "NoSuch",
        "Secret",
        "Level",
        "Count",
        "Age",
    ] {
        assert!(
            !undefined.iter().any(|n| n == name),
            "LINT0001 on `{name}`: {undefined:?}"
        );
    }
}

#[test]
fn the_methods_of_progress_lang_object_have_their_types() {
    // Every class inherits `ToString` from `Progress.Lang.Object`; an
    // `OVERRIDE` keeps its type.
    let ws = Workspace::new();
    assert_program(
        &ws,
        "object.p",
        r#"DEFINE VARIABLE oP AS app.Person NO-UNDO.
DEFINE VARIABLE oNamed AS app.Named NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
l = oNamed:ToString().
l = oP:ToString().
MESSAGE oP oNamed l.
"#,
        list(&[
            (
                "l = oNamed:ToString().",
                mismatch("CHARACTER", "LOGICAL"),
                "l",
            ),
            ("l = oP:ToString().", mismatch("CHARACTER", "LOGICAL"), "l"),
        ]),
    );
    let report = check(&ws, "object.p");
    assert!(
        !report
            .span_texts("LINT0001")
            .iter()
            .any(|n| n == "ToString"),
        "{:?}",
        report.span_texts("LINT0001")
    );
}

#[test]
fn without_an_index_no_receiver_class_resolves() {
    let ws = Workspace::new();
    ws.write("oxabl.toml", CONFIG_NO_SOURCES);
    assert_program(&ws, "basic.p", &basic_program(BASIC_ROWS), Vec::new());
}

#[test]
fn a_class_that_two_search_paths_supply_gives_no_finding() {
    let ws = Workspace::new();
    ws.write(
        "oxabl.toml",
        "[workspace]\nname = \"members\"\n\
         [workspace.sources]\ninclude_paths = [\".\", \"alt\"]\n",
    );
    let dup =
        "CLASS app.Dup:\n    DEFINE PUBLIC PROPERTY V AS CHARACTER NO-UNDO GET. SET.\nEND CLASS.\n";
    ws.write("app/Dup.cls", dup);
    ws.write("alt/app/Dup.cls", dup);
    assert_program(
        &ws,
        "dup.p",
        r#"DEFINE VARIABLE oD AS app.Dup NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
i = oD:V.
MESSAGE oD i.
"#,
        Vec::new(),
    );
}

// ---------------------------------------------------------------------------
// G. Ancestors oxabl cannot read
// ---------------------------------------------------------------------------

#[test]
fn a_method_of_a_class_with_an_unread_ancestor_is_unknown_but_a_property_is_not() {
    let ws = Workspace::new();
    assert_program(
        &ws,
        "opaque.p",
        r#"DEFINE VARIABLE oErr AS app.OrphanErr NO-UNDO.
DEFINE VARIABLE oBad AS app.Bad NO-UNDO.
DEFINE VARIABLE oOnBad AS app.OnBad NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
l = oErr:Code.
l = oErr:Describe().
l = oBad:X.
l = oOnBad:Y.
l = oOnBad:Z().
MESSAGE oErr oBad oOnBad l.
"#,
        list(&[
            ("l = oErr:Code.", mismatch("INTEGER", "LOGICAL"), "l"),
            ("l = oOnBad:Y.", mismatch("INTEGER", "LOGICAL"), "l"),
        ]),
    );
}

#[test]
fn a_class_with_a_built_in_ancestor_has_a_complete_surface() {
    // The catalog has every member of `Progress.Lang.AppError` and its
    // ancestors, so the methods of the class have their types.
    let ws = Workspace::new();
    assert_program(
        &ws,
        "builtin_parent.p",
        r#"DEFINE VARIABLE oErr AS app.AppErr NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
l = oErr:Code.
l = oErr:Describe().
MESSAGE oErr l.
"#,
        list(&[
            ("l = oErr:Code.", mismatch("INTEGER", "LOGICAL"), "l"),
            (
                "l = oErr:Describe().",
                mismatch("CHARACTER", "LOGICAL"),
                "l",
            ),
        ]),
    );
}

// ---------------------------------------------------------------------------
// H. A cycle in inheritance
// ---------------------------------------------------------------------------

#[test]
fn a_cycle_in_inheritance_ends_and_gives_no_finding() {
    let ws = Workspace::new();
    ws.write(
        "cycle.p",
        r#"DEFINE VARIABLE oC AS app.CycA NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
l = oC:A.
l = oC:B.
l = oC:Nope.
MESSAGE oC l.
"#,
    );
    let report = check_within(&ws, "cycle.p", Duration::from_secs(60));
    assert_findings(&report, Vec::new());
}

// ---------------------------------------------------------------------------
// I. Includes that hold part of a class
// ---------------------------------------------------------------------------

#[test]
fn a_member_declared_in_an_include_is_known_unless_an_include_is_missing() {
    let ws = Workspace::new();
    assert_program(
        &ws,
        "includes.p",
        r#"DEFINE VARIABLE oO AS app.Order NO-UNDO.
DEFINE VARIABLE oB AS app.Broken NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
l = oO:Total.
l = oO:Code().
l = oB:Code().
MESSAGE oO oB l.
"#,
        list(&[
            ("l = oO:Total.", mismatch("DECIMAL", "LOGICAL"), "l"),
            ("l = oO:Code().", mismatch("CHARACTER", "LOGICAL"), "l"),
        ]),
    );
}

#[test]
fn this_object_sees_a_property_declared_in_an_include() {
    let ws = Workspace::new();
    assert_findings(
        &check(&ws, "app/Invoice.cls"),
        list(&[("l = THIS-OBJECT:Due.", mismatch("DATE", "LOGICAL"), "l")]),
    );
}

#[test]
fn a_class_with_a_missing_include_makes_its_own_methods_unknown() {
    let ws = Workspace::new();
    assert_findings(&check(&ws, "app/Partial.cls"), Vec::new());
}

// ---------------------------------------------------------------------------
// J. Existing rules: no new false positive
// ---------------------------------------------------------------------------

#[test]
fn assignability_follows_inheritance_and_stays_silent_when_a_parent_is_unknown() {
    let ws = Workspace::new();
    assert_program(
        &ws,
        "lattice.p",
        r#"DEFINE VARIABLE oFac AS app.Factory NO-UNDO.
DEFINE VARIABLE oTop AS app.Top NO-UNDO.
DEFINE VARIABLE oP AS app.Person NO-UNDO.
DEFINE VARIABLE oE AS app.Employee NO-UNDO.
DEFINE VARIABLE oS AS app.IShape NO-UNDO.
DEFINE VARIABLE oNamed AS app.INamed NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
oTop = NEW app.Bottom().
oTop = oFac:MakeBottom().
oP = oFac:MakeEmployee().
oS = oFac:MakeSquare().
oNamed = oFac:MakeSquare().
oE = oP:Partner().
oP = oP:Partner().
l = oFac:MakeBottom().
MESSAGE oFac oTop oP oE oS oNamed l.
"#,
        list(&[
            (
                "oE = oP:Partner().",
                mismatch("app.person", "app.employee"),
                "oE",
            ),
            (
                "l = oFac:MakeBottom().",
                mismatch("app.bottom", "LOGICAL"),
                "l",
            ),
        ]),
    );
}

#[test]
fn a_member_argument_of_run_is_checked_against_the_parameter() {
    let ws = Workspace::new();
    ws.write(
        "oxabl.toml",
        "[workspace]\nname = \"members\"\n\
         [workspace.sources]\ninclude_paths = [\".\"]\n\
         [workspace.lint]\nenable = [\"strict.calls\"]\n",
    );
    ws.write(
        "calls.p",
        r#"DEFINE VARIABLE oP AS app.Person NO-UNDO.
RUN takes-dec (INPUT oP:Window).
RUN takes-dec (INPUT oP:Age).
PROCEDURE takes-dec:
    DEFINE INPUT PARAMETER p AS DECIMAL NO-UNDO.
    MESSAGE p.
END PROCEDURE.
"#,
    );
    let report = check(&ws, "calls.p");
    let found: Vec<(String, String)> = report
        .findings("CL003")
        .into_iter()
        .map(|(line, message, _)| (line, message))
        .collect();
    assert_eq!(
        found,
        vec![(
            "RUN takes-dec (INPUT oP:Window).".to_string(),
            "argument 1 of `RUN takes-dec` is HANDLE, but the parameter is DECIMAL".to_string()
        )],
        "CL003 findings"
    );
}

const STATIC_VARIABLE_CLASS: &str = r#"CLASS Box:
    DEFINE PUBLIC STATIC VARIABLE xS AS INTEGER NO-UNDO.
    DEFINE PUBLIC STATIC PROPERTY pS AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC VOID Run():
        DEFINE VARIABLE c AS CHARACTER NO-UNDO.
        DEFINE VARIABLE o AS Box NO-UNDO.
        c = Box:xS.
        c = o:xS.
        c = Box:pS.
        c = o:pS.
        MESSAGE c.
    END METHOD.
END CLASS.
"#;

/// A `STATIC` data member is typed as a `STATIC` property is: read through
/// its type it has the declared type; read through an instance it reaches a
/// member of the wrong kind, so it has no type and no finding. The class that
/// declares it and another file (through the workspace index) agree.
#[test]
fn a_static_variable_is_typed_through_its_type_and_not_through_an_instance() {
    let ws = Workspace::new();
    ws.write("Box.cls", STATIC_VARIABLE_CLASS);
    assert_findings(
        &check(&ws, "Box.cls"),
        list(&[
            ("c = Box:xS.", mismatch("INTEGER", "CHARACTER"), "c"),
            ("c = Box:pS.", mismatch("INTEGER", "CHARACTER"), "c"),
        ]),
    );
    ws.write(
        "app/Sv.cls",
        "CLASS app.Sv:\n    DEFINE PUBLIC STATIC VARIABLE xS AS INTEGER NO-UNDO.\nEND CLASS.\n",
    );
    assert_program(
        &ws,
        "sv_user.p",
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nDEFINE VARIABLE o AS app.Sv NO-UNDO.\nc = app.Sv:xS.\nc = o:xS.\nMESSAGE c.\n",
        list(&[("c = app.Sv:xS.", mismatch("INTEGER", "CHARACTER"), "c")]),
    );
}
