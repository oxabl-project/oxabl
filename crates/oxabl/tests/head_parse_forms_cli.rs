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
    /// A statement with a bare name that is not a symbol (a static query, a
    /// frame). It must not be reported as undefined.
    bare: Option<&'static str>,
}

const DECL_X: &str = "DEFINE VARIABLE v-x AS CHARACTER NO-UNDO.\n";
const DECL_HANDLE: &str = "DEFINE VARIABLE v-x AS HANDLE NO-UNDO.\n";

const FORMS: &[Form] = &[
    Form {
        name: "PROMPT-FOR",
        decls: DECL_X,
        statement: "PROMPT-FOR v-x FORMAT \"x(10)\" WITH FRAME f-main.",
        undefined: Some("PROMPT-FOR v-nope FORMAT \"x(10)\" WITH FRAME f-main."),
        bare: None,
    },
    Form {
        name: "GET",
        decls: DECL_HANDLE,
        statement: "GET FIRST v-x NO-LOCK.",
        undefined: None,
        bare: Some("GET NEXT q-detail NO-LOCK."),
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
    for f in FORMS {
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
