//! End-to-end tests for statement forms the parser reads the head of
//! (PROMPT-FOR, COPY-LOB, REPOSITION, CLEAR, ACCUMULATE, COLOR, GET, CLOSE).
//!
//! Each case drives the built binary over a synthetic file and checks what the
//! user sees: a variable the form names is credited (no unused-variable finding,
//! no "could not be fully checked" note), an undefined name in the form is
//! reported, and `oxabl format` leaves the statement stable.

use std::fs;
use std::path::Path;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

struct Form {
    name: &'static str,
    /// Declarations the statement may use. `v-x` is the one the credit test
    /// watches.
    decls: &'static str,
    /// A statement that names `v-x` and nothing else undeclared.
    statement: &'static str,
    /// The same statement with the undefined name `v-nope` where `v-x` was.
    /// `None` when the form has no operand that must be a declared symbol.
    undefined: Option<&'static str>,
    /// Whether `statement` names a value symbol, so that the credit test can
    /// fail when the form is skipped.
    credits: bool,
    /// A statement with a bare name that is not a symbol (a static query, a
    /// frame). It must not be reported as undefined.
    bare: Option<&'static str>,
}

const DECL_X: &str = "DEFINE VARIABLE v-x AS CHARACTER NO-UNDO.\n";
const DECL_STORED: &str =
    "DEFINE VARIABLE v-x AS INTEGER NO-UNDO.\nDEFINE VARIABLE v-st AS INTEGER NO-UNDO.\n";
const DECL_INT: &str = "DEFINE VARIABLE v-x AS INTEGER NO-UNDO.\n";
const DECL_FRAME: &str = "DEFINE FRAME f-x WITH SIDE-LABELS.\n";
const DECL_LOBS: &str =
    "DEFINE VARIABLE v-x AS LONGCHAR NO-UNDO.\nDEFINE VARIABLE v-y AS LONGCHAR NO-UNDO.\n";
const DECL_HANDLE: &str = "DEFINE VARIABLE v-x AS HANDLE NO-UNDO.\n";

const FORMS: &[Form] = &[
    Form {
        name: "PROMPT-FOR",
        decls: DECL_X,
        credits: true,
        statement: "PROMPT-FOR v-x FORMAT \"x(10)\" WITH FRAME f-main.",
        undefined: Some("PROMPT-FOR v-nope FORMAT \"x(10)\" WITH FRAME f-main."),
        bare: None,
    },
    Form {
        name: "GET",
        decls: DECL_HANDLE,
        credits: true,
        statement: "GET FIRST v-x NO-LOCK.",
        undefined: None,
        bare: Some("GET NEXT q-detail NO-LOCK."),
    },
    Form {
        name: "CLOSE QUERY",
        decls: DECL_HANDLE,
        credits: true,
        statement: "CLOSE QUERY v-x.",
        undefined: None,
        bare: Some("CLOSE QUERY q-detail."),
    },
    Form {
        name: "CLOSE STORED-PROC",
        decls: DECL_STORED,
        credits: true,
        statement: "CLOSE STORED-PROC sp-detail v-st = PROC-STATUS WHERE PROC-HANDLE = v-x.",
        undefined: Some(
            "CLOSE STORED-PROC sp-detail v-st = PROC-STATUS WHERE PROC-HANDLE = v-nope.",
        ),
        bare: Some("CLOSE STORED-PROCEDURE sp-detail."),
    },
    Form {
        name: "REPOSITION",
        decls: DECL_INT,
        credits: true,
        statement: "REPOSITION q-detail TO ROW v-x NO-ERROR.",
        undefined: Some("REPOSITION q-detail TO ROW v-nope NO-ERROR."),
        bare: Some("REPOSITION q-detail FORWARD 2."),
    },
    Form {
        name: "ACCUMULATE",
        decls: DECL_INT,
        credits: true,
        statement: "ACCUMULATE v-x (TOTAL COUNT).",
        undefined: Some("ACCUMULATE v-x (TOTAL BY v-nope)."),
        bare: None,
    },
    Form {
        name: "COPY-LOB",
        decls: DECL_LOBS,
        credits: true,
        statement: "COPY-LOB FROM OBJECT v-x STARTING AT 1 FOR 10 TO OBJECT v-y APPEND NO-ERROR.",
        undefined: Some(
            "COPY-LOB FROM OBJECT v-nope STARTING AT 1 FOR 10 TO OBJECT v-y APPEND NO-ERROR.",
        ),
        bare: None,
    },
    Form {
        name: "COLOR",
        decls: DECL_X,
        credits: true,
        statement: "COLOR DISPLAY NORMAL v-x WITH FRAME f-main.",
        undefined: Some("COLOR DISPLAY NORMAL v-nope WITH FRAME f-main."),
        bare: None,
    },
    Form {
        name: "COLOR DISPLAY",
        decls: DECL_X,
        credits: true,
        statement: "COLOR DISPLAY MESSAGES PROMPT NORMAL v-x.",
        undefined: Some("COLOR DISPLAY VALUE(\"x\") PROMPT VALUE(\"y\") v-nope."),
        bare: Some("COLOR DISPLAY NORMAL PROMPT INPUT v-x WITH FRAME f-main."),
    },
    Form {
        name: "CLEAR",
        decls: DECL_FRAME,
        credits: false,
        statement: "CLEAR FRAME f-x ALL NO-PAUSE.",
        undefined: None,
        bare: None,
    },
];

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

fn project(source: &str) -> (TempDir, std::path::PathBuf) {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("main.p");
    fs::write(&file, source).unwrap();
    (tmp, file)
}

fn check(file: &Path, json: bool) -> (String, String) {
    let mut cmd = oxabl();
    cmd.arg("check").arg("--no-format");
    if json {
        cmd.arg("--json");
    }
    let out = cmd.arg(file).output().unwrap();
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn a_variable_the_form_names_is_credited() {
    for f in FORMS.iter().filter(|f| f.credits) {
        let source = format!("{}{}\n", f.decls, f.statement);
        let (_tmp, file) = project(&source);
        let (text, _) = check(&file, false);
        assert!(
            !text.contains("LINT0002"),
            "{}: v-x reads as unused:\n{text}",
            f.name
        );
        let (json, err) = check(&file, true);
        let json: Value = serde_json::from_str(&json)
            .unwrap_or_else(|e| panic!("{}: not JSON ({e}):\n{json}\n{err}", f.name));
        assert_eq!(
            json["unjudged_symbols"], 0,
            "{}: the statement still counts as unmodelled:\n{json}",
            f.name
        );
    }
}

#[test]
fn an_undefined_name_in_the_form_is_reported() {
    for f in FORMS {
        let Some(undefined) = f.undefined else {
            continue;
        };
        let source = format!("{}{}\n", f.decls, undefined);
        let (_tmp, file) = project(&source);
        let (text, _) = check(&file, false);
        assert!(
            text.contains("LINT0001") && text.contains("v-nope"),
            "{}: expected an undefined-symbol finding for v-nope:\n{text}",
            f.name
        );
    }
}

#[test]
fn a_bare_name_that_is_not_a_symbol_is_not_reported() {
    for f in FORMS {
        let Some(bare) = f.bare else {
            continue;
        };
        let source = format!("{}{}\n", f.decls, bare);
        let (_tmp, file) = project(&source);
        let (text, _) = check(&file, false);
        assert!(
            !text.contains("LINT0001"),
            "{}: a bare query or frame name reads as undefined:\n{text}",
            f.name
        );
    }
}

#[test]
fn the_formatter_keeps_the_form_stable() {
    for f in FORMS {
        let source = format!("{}{}\n", f.decls, f.statement);
        let (_tmp, file) = project(&source);
        let run = || {
            let out = oxabl().arg("format").arg(&file).output().unwrap();
            assert!(out.status.success(), "{}: format failed: {out:?}", f.name);
            fs::read_to_string(&file).unwrap()
        };
        let first = run();
        let second = run();
        assert_eq!(first, second, "{}: format is not idempotent", f.name);
        assert!(
            first.to_uppercase().contains(&f.name.to_uppercase()),
            "{}: the statement was lost:\n{first}",
            f.name
        );
    }
}

/// The codes `oxabl check` reports for `source` in a project with the lint
/// groups `groups` on, one per finding.
fn codes_with(groups: &str, source: &str) -> Vec<String> {
    let (tmp, _) = project(source);
    fs::write(
        tmp.path().join("oxabl.toml"),
        format!("[workspace]\nname = \"p\"\n[workspace.lint]\nenable = [{groups}]\n"),
    )
    .unwrap();
    let mut cmd = oxabl();
    let out = cmd
        .current_dir(tmp.path())
        .args(["check", "--no-format", "--json", "main.p"])
        .output()
        .unwrap();
    let json: Value = serde_json::from_slice(&out.stdout)
        .unwrap_or_else(|e| panic!("not JSON ({e}): {}", String::from_utf8_lossy(&out.stderr)));
    json["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["code"].as_str().unwrap().to_string())
        .collect()
}

const MAYBE_NO_RECORD: &str = "DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS INTEGER FIELD n AS CHARACTER INDEX ik k.\n\
                               DEFINE VARIABLE v-x AS CHARACTER NO-UNDO.\n\
                               FIND FIRST tt NO-ERROR.\n";

/// COLOR names the widgets of its fields; it does not read the record. A
/// `VALUE(...)` color is read.
#[test]
fn color_fields_are_not_record_reads() {
    for (statement, nl003) in [
        ("COLOR DISPLAY NORMAL tt.n.", 0),
        ("COLOR DISPLAY NORMAL PROMPT INPUT tt.n WITH FRAME f.", 0),
        ("COLOR DISPLAY VALUE(tt.n) v-x.", 1),
        ("DISPLAY tt.n.", 1),
    ] {
        let codes = codes_with(
            "\"strict.nulls\"",
            &format!("{MAYBE_NO_RECORD}{statement}\n"),
        );
        let found = codes.iter().filter(|c| *c == "NL003").count();
        assert_eq!(found, nl003, "{statement}: {codes:?}");
    }
}

/// A dynamic call in the options or the EDITING block of a head-parsed form
/// is reported.
#[test]
fn a_dynamic_call_in_the_tail_of_a_form_is_reported() {
    for statement in [
        "PROMPT-FOR v-x EDITING: v-x = DYNAMIC-FUNCTION(\"f\"). END.",
        "PROMPT-FOR v-x WITH FRAME f TITLE DYNAMIC-FUNCTION(\"t\").",
        "COLOR DISPLAY NORMAL v-x WITH FRAME f TITLE DYNAMIC-FUNCTION(\"t2\").",
    ] {
        let codes = codes_with("\"strict.calls\"", &format!("{DECL_X}{statement}\n"));
        let found = codes.iter().filter(|c| *c == "CL002").count();
        assert_eq!(found, 1, "{statement}: {codes:?}");
    }
}
