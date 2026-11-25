//! Built-in classes through `oxabl check`, for the cases the review of the
//! catalog found: a scalar return that the documentation gives as an
//! `EXTENT`, a `SUPER:` receiver, and a project on an OpenEdge version newer
//! than the catalog.

#![cfg(unix)]

use std::process::Command;

use serde_json::Value;

const STRICT: &str = "[workspace]\nname = \"builtins\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n\
    [workspace.lint]\nenable = [\"strict.calls\"]\n";

/// `(trimmed line, message)` of each finding of the `CL006` and `CL007` rules
/// for `source` checked with `config`, in source order.
fn findings(config: &str, files: &[(&str, &str)], source: &str) -> Vec<(String, String)> {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), config).unwrap();
    for (name, text) in files {
        let path = dir.path().join(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
    std::fs::write(dir.path().join("main.p"), source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["check", "--json", "--no-format", "main.p"])
        .current_dir(dir.path())
        .output()
        .unwrap();
    let json: Value = serde_json::from_slice(&out.stdout)
        .unwrap_or_else(|e| panic!("not JSON ({e}): {}", String::from_utf8_lossy(&out.stderr)));
    assert_eq!(json["failures"], serde_json::json!([]), "{json}");
    let mut found: Vec<(u64, String, String)> = json["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["code"] == "CL006" || d["code"] == "CL007" || d["code"] == "LINT0001")
        .map(|d| {
            let line = d["start"]["line"].as_u64().unwrap();
            let text = source.lines().nth(line as usize - 1).unwrap().trim();
            (
                line,
                text.to_string(),
                d["message"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    found.sort();
    found.into_iter().map(|(_, l, m)| (l, m)).collect()
}

fn row(line: &str, message: &str) -> (String, String) {
    (line.to_string(), message.to_string())
}

#[test]
fn a_scalar_that_a_built_in_member_returns_may_be_an_extent() {
    let source = "DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.\n\
        DEFINE VARIABLE oHdr AS OpenEdge.Net.HTTP.HttpHeader NO-UNDO.\n\
        DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.\n\
        lc = OpenEdge.Core.String:Join(oJson:GetNames(), \",\").\n\
        lc = OpenEdge.Core.String:Join(oHdr:GetParameterValues(), \",\").\n\
        lc = OpenEdge.Core.String:Join(\"a,b\", \",\").\n\
        MESSAGE lc.\n";
    // `GetNames` returns an EXTENT (an override fixes the row). The catalog
    // gives `GetParameterValues` a scalar return, which may have lost its
    // EXTENT: no finding. A literal is a scalar for sure.
    assert_eq!(
        findings(STRICT, &[], source),
        [row(
            "lc = OpenEdge.Core.String:Join(\"a,b\", \",\").",
            "no overload of `OpenEdge.Core.String:Join` takes arguments of these types: `CHARACTER`, `CHARACTER`"
        )]
    );
}

#[test]
fn a_super_receiver_is_not_judged_by_the_member_rules() {
    let class = "CLASS app.MyErr INHERITS Progress.Lang.AppError:\n\
        METHOD PUBLIC VOID Go():\n\
        SUPER:NoSuchThing().\n\
        MESSAGE SUPER:GetMessage(1, 2).\n\
        MESSAGE SUPER:GetMessage(1).\n\
        END METHOD.\n\
        END CLASS.\n";
    let source = "DEFINE VARIABLE o AS app.MyErr NO-UNDO.\nMESSAGE o.\n";
    // The rules judge a receiver whose type is a built-in type; `SUPER` in a
    // user class is that class. Its own file has no finding either.
    assert_eq!(findings(STRICT, &[("app/MyErr.cls", class)], source), []);
    assert_eq!(findings(STRICT, &[("app/Other.cls", class)], class), []);
}

const VERSIONED: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    MESSAGE oErr:GetMesage(1).\n\
    MESSAGE oErr:GetMessage(1, 2).\n\
    MESSAGE oErr:GetMessage(\"1\").\n";

fn with_version(version: &str) -> String {
    format!("{STRICT}[workspace.preprocessor]\nproversion = \"{version}\"\n")
}

#[test]
fn a_newer_openedge_version_silences_the_absence_findings() {
    let types = row(
        "MESSAGE oErr:GetMessage(\"1\").",
        "no overload of `Progress.Lang.AppError:GetMessage` takes arguments of these types: `CHARACTER`",
    );
    let every = vec![
        row(
            "MESSAGE oErr:GetMesage(1).",
            "built-in type `Progress.Lang.AppError` has no member `GetMesage`",
        ),
        row(
            "MESSAGE oErr:GetMessage(1, 2).",
            "no overload of `Progress.Lang.AppError:GetMessage` takes 2 arguments",
        ),
        types.clone(),
    ];
    // The catalog's version, an older one, and none: every finding.
    for config in [
        with_version("12.8"),
        with_version("12.2.4"),
        STRICT.to_string(),
    ] {
        assert_eq!(findings(&config, &[], VERSIONED), every, "{config}");
    }
    // A newer version may have the member or the overload: only a type that
    // never converts is still reported.
    for version in ["12.9", "13.0"] {
        assert_eq!(
            findings(&with_version(version), &[], VERSIONED),
            std::slice::from_ref(&types),
            "{version}"
        );
    }
}
